open! Core
module Unix := Core_unix

(** Support for an expanded range of mmap() system calls. *)

(** Type of memory mapped buffers. *)
type t : mutable_data [@@deriving sexp_of]

(** Return the actual memory mapped buffer. *)
val bigstring : t -> Bigstring.t

(** Return an [Ext_pointer.t] to the beginning of the data buffer in memory. Importantly,
    this points to the actual data, not the header that precedes it.

    This is unsafe because it allows accessing memory after it has been unmapped. Unless
    global state points to the Mmap.t, it's possible that the Mmap.t will be garbage
    collected (and its memory unmapped); holding onto the Ext_pointer alone isn't enough
    to prevent the memory it points to from being freed. *)
val unsafe_ext_pointer : t -> Ocaml_intrinsics.Ext_pointer.t

(** Offset at which the mapping starts. Measured in bytes from the beginning of the file.

    The offset is an [int64] to support large files on 32-bit architectures. *)
val start_of_mapping : t -> int64

(** Like in the [Unix] module, all the following system calls might raise
    [Unix.Unix_error] or [Failure]. *)

module Protection : sig
  include Flags.S

  (** Pages may be executed. *)
  val exec : t

  (** Pages may be read. *)
  val read : t

  (** Pages may be written. *)
  val write : t

  (** Pages may not be accessed. *)
  val none : t
end

module Map_visibility : sig
  type t =
    | Shared
    | Private
end

(** see mmap(2) man pages for a description of all [mmap] flags *)
module Map_flags : sig
  include Flags.S

  val map_anonymous : t
  val map_fixed : t
  val map_growsdown : t
  val map_hugetlb : t
  val map_huge_2mb : t
  val map_locked : t
  val map_nonblock : t
  val map_noreserve : t
  val map_populate : t
  val map_stack : t
  val map_shared : t
  val map_private : t
end

(** [mmap64] reimplements the [Bigstring.map_file] function but exposes more controls to
    the user.

    Unlike the system call, this version will attempt to grow the underlying file (using
    [pwrite]) if [len] exceeds the file size. (This is also done by the Bigarray module's
    [mmap] call, and hence by [Bigstring.map_file].)

    The mapping will be unmapped either by the finalizer on the returned value [t] (or
    equivalently on [bigstring t]) or by an explicit call to
    [Bigstring.unsafe_destroy (bigstring t)]. *)
val mmap64
  :  Unix.File_descr.t
  -> visibility:Map_visibility.t
  -> ?pos:int64 (* [int64] for the same reason as [start_of_mapping] *)
  -> ?len:int
  -> ?protection:Protection.t
  -> ?flags:Map_flags.t
  -> unit
  -> t

val mmap_anonymous
  :  visibility:Map_visibility.t
  -> ?pos:int64 (* [int64] for the same reason as [start_of_mapping] *)
  -> len:int
  -> ?protection:Protection.t
  -> ?flags:Map_flags.t
  -> unit
  -> t

module Mremap_flags : sig
  include Flags.S

  val maymove : t
  val fixed : t
end

(* Q: why does this function not just update the data pointer in [t]? A: this seems safer.
   For instance:

   {[
     b.{10} <- 'e';
     mremap b ~new_size:5;
     b.{10} <- 'c'
   ]}

   The compiler could assume that the pointer in b is constant. This is not the case but
   it seems safer not to rely on it.
*)

(** [mremap] allows one to expand or shrink a memory mapping without unmapping and
    remapping a file. Like [mmap] this will grow the underlying file using [pwrite] if
    [new_size] exceeds its size.

    The original [bigstring t] is destroyed the same way [Bigstring.unsafe_destroy] does.
    It shouldn't be used after this call. *)
val mremap : t -> new_size:int -> ?flags:Mremap_flags.t -> unit -> t

(** [get_page_size] returns the size of a memory page. Somewhat incomplete, as there seems
    to be no standard way to query huge page sizes. On our present (Q1 2014) hardware,
    normal page size is 4kB and huge pages are 2048kB. Aligning to a huge page improves
    the success rate of transparent huge pages, where enabled. *)
val get_page_size_in_bytes : unit -> int

(** see msync(2) man pages for a description of all [msync] flags *)
module Sync_flags : sig
  include Flags.S

  val async : t
  val sync : t
  val invalidate : t
end

(** [msync t ~pos ~len flags] flushes changes made to the in-core copy of a file that was
    mapped into memory using [mmap] back to disk. Without use of this call there is no
    guarantee that changes are written back before munmap(2) is called. To be more
    precise, the part of the file that corresponds to the memory area starting at [pos]
    and having length [len] is updated.

    This is a no-op on anonymous mmaps, but will succeed.

    [pos] and [len] are expressed in bytes. *)
val msync
  :  t
  -> ?pos:int (* default: 0 *)
  -> ?len:int (* default: Bigstring.length (bigstring t) - pos *)
  -> ?flags:Sync_flags.t
  -> unit
  -> unit

(** [mlock t ~pos ~len] locks pages in the address range starting at [pos] and continuing
    for [len] bytes. All pages that contain a part of the specified address range are
    guaranteed to be resident in RAM when the call returns successfully; the pages are
    guaranteed to stay in RAM until later unlocked.

    [pos] and [len] are expressed in bytes. *)
val mlock
  :  t
  -> ?pos:int (* default: 0 *)
  -> ?len:int (* default: Bigstring.length (bigstring t) - pos *)
  -> unit
  -> unit

(** [munlock t ~pos ~len] unlocks pages in the address range starting at [pos] and
    continuing for [len] bytes. After this call, all pages that contain a part of the
    specified memory range can be moved to external swap space again by the kernel.

    [pos] and [len] are expressed in bytes. *)
val munlock
  :  t
  -> ?pos:int (* default: 0 *)
  -> ?len:int (* default: Bigstring.length (bigstring t) - pos *)
  -> unit
  -> unit

(** [mprotect t ~pos ~len] changes page protection in the address range starting at [pos]
    and continuing for [len] bytes.

    [pos] and [len] are expressed in bytes. *)
val mprotect : t -> ?pos:int -> ?len:int -> protection:Protection.t -> unit -> unit

(** see madvise(2) man pages for a description of advice flags. *)
module Advice : sig
  type t = private Int63.t [@@deriving compare ~localize, sexp]

  include Stringable with type t := t

  val of_int63 : Int63.t -> t
  val normal : t
  val random : t
  val sequential : t
  val willneed : t
  val dontneed : t
  val remove : t
  val dontfork : t
  val dofork : t
  val thp : t
  val no_thp : t
end

(** [madvise] system call advises the kernel about how to handle paging input/output in
    the address range beginning at [pos] and with size [len] bytes. It allows an
    application to tell the kernel how it expects to use some mapped or shared memory
    areas, so that the kernel can choose appropriate read-ahead and caching techniques.

    This call does not influence the semantics of the application (except in the case of
    [Advice.dontneed]), but may influence its performance. The kernel is free to ignore
    the advice. *)
val madvise : t -> ?pos:int -> ?len:int -> advice:Advice.t -> unit -> unit

(** Encapsulates the result of [mincore] for a given [offset] in pages to the [pos]
    position requested or for all of the requested pages in the call. *)
module Incore : sig
  type t

  val get : t -> offset:int -> bool
  val get_between : t -> offset:int -> num_pages:int -> bool list
  val get_all : t -> bool list
  val percentage_in_core : t -> float
end

(** [mincore] checks if the pages containing the address range beginning at [pos] and with
    size [len] bytes are currently present in physical memory (and thus will not cause a
    page fault when accessed).

    See mincore(2) man pages for further detail. *)
val mincore : t -> ?pos:int -> ?len:int -> unit -> Incore.t

(** [mincore_reload] updates the data in the [Incore.t] for the same [pos] and [len] as
    the original one. *)
val mincore_reload : t -> Incore.t -> unit

(** {v
 [fallocate] ensures the range [offset..offset+len) is allocated in the filesystem,
    extending the file with zeroes if its length was less than offset+len bytes.

    We normally extend short files with [pwrite] instead of [fallocate], which leaves a
    hole in the file instead of allocating blocks of zeroes on disk.

    It is useful to call [fallocate] before [mmap64] in a few cases:
    - hugetlbfs does not support [pwrite], so [mmap64] just fails if the file is smaller
      than the length passed to [mmap64].
    - holes might not be desirable when using [map_populate], since [map_populate] is not
      guaranteed to fill holes (the behaviour is filesystem-dependent).
    v} *)
val fallocate
  :  Unix.File_descr.t
  -> ?pos:int64 (* default: 0 *)
  -> len:int
  -> unit
  -> unit
