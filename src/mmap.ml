open Core
module Unix = Core_unix

type t =
  { fd : Unix.File_descr.t
  ; bigstring : (Bigstring.t[@sexp.opaque] (* much too big *))
  ; start_of_mapping : int64
  }
[@@deriving fields ~getters, sexp_of]

module Protection = struct
  external flag_exec : unit -> Int63.t = "zero_mmap_PROT_EXEC"
  external flag_read : unit -> Int63.t = "zero_mmap_PROT_READ"
  external flag_write : unit -> Int63.t = "zero_mmap_PROT_WRITE"
  external flag_none : unit -> Int63.t = "zero_mmap_PROT_NONE"

  let exec = flag_exec ()
  let read = flag_read ()
  let write = flag_write ()
  let none = flag_none ()
  let () = assert (Int63.equal none Int63.zero)

  include Flags.Make (struct
      let known = [ exec, "exec"; read, "read"; write, "write" ]
      let remove_zero_flags = false
      let allow_intersecting = false
      let should_print_error = true
    end)
end

module Map_visibility = struct
  type t =
    | Shared
    | Private
end

module Map_flags = struct
  external flag_map_shared : unit -> Int63.t = "zero_mmap_MAP_SHARED"
  external flag_map_private : unit -> Int63.t = "zero_mmap_MAP_PRIVATE"
  external flag_map_anonymous : unit -> Int63.t = "zero_mmap_MAP_ANONYMOUS"
  external flag_map_fixed : unit -> Int63.t = "zero_mmap_MAP_FIXED"
  external flag_map_growsdown : unit -> Int63.t = "zero_mmap_MAP_GROWSDOWN"
  external flag_map_hugetlb : unit -> Int63.t = "zero_mmap_MAP_HUGETLB"
  external flag_map_huge_2mb : unit -> Int63.t = "zero_mmap_MAP_HUGE_2MB"
  external flag_map_locked : unit -> Int63.t = "zero_mmap_MAP_LOCKED"
  external flag_map_nonblock : unit -> Int63.t = "zero_mmap_MAP_NONBLOCK"
  external flag_map_noreserve : unit -> Int63.t = "zero_mmap_MAP_NORESERVE"
  external flag_map_populate : unit -> Int63.t = "zero_mmap_MAP_POPULATE"
  external flag_map_stack : unit -> Int63.t = "zero_mmap_MAP_STACK"

  let map_shared = flag_map_shared ()
  let map_private = flag_map_private ()
  let map_anonymous = flag_map_anonymous ()
  let map_fixed = flag_map_fixed ()
  let map_growsdown = flag_map_growsdown ()
  let map_hugetlb = flag_map_hugetlb ()
  let map_huge_2mb = flag_map_huge_2mb ()
  let map_locked = flag_map_locked ()
  let map_nonblock = flag_map_nonblock ()
  let map_noreserve = flag_map_noreserve ()
  let map_populate = flag_map_populate ()
  let map_stack = flag_map_stack ()

  include Flags.Make (struct
      let known =
        [ map_shared, "map_shared"
        ; map_private, "map_private"
        ; map_anonymous, "map_anonymous"
        ; map_fixed, "map_fixed"
        ; map_growsdown, "map_growsdown"
        ; map_hugetlb, "map_hugetlb"
        ; map_huge_2mb, "map_huge_2mb"
        ; map_locked, "map_locked"
        ; map_nonblock, "map_nonblock"
        ; map_noreserve, "map_noreserve"
        ; map_populate, "map_populate"
        ; map_stack, "map_stack"
        ]
      ;;

      let remove_zero_flags = false
      let allow_intersecting = false
      let should_print_error = true
    end)
end

let file_length fd = Int64.to_int_exn (Unix.fstat fd).st_size

external grow_file
  :  Unix.File_descr.t
  -> pos:int64
  -> len:int
  -> unit
  = "zero_mmap_grow_file"

external unsafe_mmap64
  :  Unix.File_descr.t
  -> pos:int64
  -> len:int
  -> protection:Protection.t
  -> flags:Map_flags.t
  -> Bigstring.t
  = "zero_mmap_mmap64"

let mmap64
  fd
  ~(visibility : Map_visibility.t)
  ?(pos = 0L)
  ?len
  ?(protection = Protection.none)
  ?(flags = Map_flags.empty)
  ()
  =
  if Int64.( < ) pos 0L then invalid_arg "Mmap.mmap64: pos < 0";
  let len =
    match len with
    | None -> file_length fd
    | Some len ->
      if len < 0 then invalid_arg "Mmap.mmap64: len < 0";
      grow_file fd ~pos ~len;
      len
  in
  let flags =
    Map_flags.(
      flags
      +
      match visibility with
      | Shared -> map_shared
      | Private -> map_private)
  in
  { fd
  ; bigstring = unsafe_mmap64 fd ~pos ~len ~protection ~flags
  ; start_of_mapping = pos
  }
;;

let mmap_anonymous ~visibility ?pos ~len ?protection ?(flags = Map_flags.empty) () =
  let fd = Unix.File_descr.of_int (-1) in
  let flags = Map_flags.(flags + map_anonymous) in
  mmap64 fd ~visibility ?pos ~len ?protection ~flags ()
;;

module Mremap_flags = struct
  external flag_maymove : unit -> Int63.t = "zero_mmap_MREMAP_MAYMOVE"
  external flag_fixed : unit -> Int63.t = "zero_mmap_MREMAP_FIXED"

  let maymove = flag_maymove ()
  let fixed = flag_fixed ()

  include Flags.Make (struct
      let known = [ maymove, "maymove"; fixed, "fixed" ]
      let remove_zero_flags = false
      let allow_intersecting = false
      let should_print_error = true
    end)
end

external unsafe_mremap
  :  Bigstring.t
  -> new_size:int
  -> flags:Mremap_flags.t
  -> Bigstring.t
  = "zero_mmap_mremap"

let mremap t ~new_size ?(flags = Mremap_flags.empty) () =
  assert (Bigstring.is_mmapped t.bigstring);
  if new_size < 0 then failwithf "Mmap.mremap: new_size(%d) < 0" new_size ();
  grow_file t.fd ~pos:t.start_of_mapping ~len:new_size;
  match unsafe_mremap t.bigstring ~new_size ~flags with
  | exception mremap_failure ->
    let selected_ulimits =
      List.map
        Unix.RLimit.[ virtual_memory; Ok file_size; Ok stack; Ok num_file_descriptors ]
        ~f:(Or_error.map ~f:(fun r -> r, Or_error.try_with (fun () -> Unix.RLimit.get r)))
    in
    raise_s
      [%message
        (mremap_failure : exn)
          (selected_ulimits
           : (Unix.RLimit.resource * Unix.RLimit.t Or_error.t) Or_error.t list)]
  | new_bigstring -> { t with bigstring = new_bigstring }
;;

external get_page_size_in_bytes : unit -> int = "zero_mmap_get_page_size_in_bytes"
[@@noalloc]

let len_after bstr ~pos = Bigstring.length bstr - pos

let check_args ~loc ~pos ~len bstr =
  if pos < 0 then invalid_arg ("Mmap." ^ loc ^ ": pos < 0");
  if len < 0 then invalid_arg ("Mmap." ^ loc ^ ": len < 0");
  let bstr_len = Bigstring.length bstr in
  if bstr_len < pos + len
  then failwithf "Mmap.%s: bigstring-len(%d) < pos(%d) + len(%d)" loc bstr_len pos len ()
;;

module Sync_flags = struct
  external flag_async : unit -> Int63.t = "zero_mmap_MS_ASYNC"
  external flag_sync : unit -> Int63.t = "zero_mmap_MS_SYNC"
  external flag_invalidate : unit -> Int63.t = "zero_mmap_MS_INVALIDATE"

  let async = flag_async ()
  let sync = flag_sync ()
  let invalidate = flag_invalidate ()

  include Flags.Make (struct
      let known = [ async, "async"; sync, "sync"; invalidate, "invalidate" ]
      let remove_zero_flags = false
      let allow_intersecting = false
      let should_print_error = true
    end)
end

external unsafe_mlock : Bigstring.t -> pos:int -> len:int -> unit = "zero_mmap_mlock"

let mlock t ?(pos = 0) ?(len = len_after t.bigstring ~pos) () =
  check_args ~loc:"mlock" ~pos ~len t.bigstring;
  unsafe_mlock t.bigstring ~pos ~len
;;

external unsafe_munlock : Bigstring.t -> pos:int -> len:int -> unit = "zero_mmap_munlock"

let munlock t ?(pos = 0) ?(len = len_after t.bigstring ~pos) () =
  check_args ~loc:"munlock" ~pos ~len t.bigstring;
  unsafe_munlock t.bigstring ~pos ~len
;;

external unsafe_msync
  :  Bigstring.t
  -> pos:int
  -> len:int
  -> flags:Sync_flags.t
  -> unit
  = "zero_mmap_msync"

let msync t ?(pos = 0) ?(len = len_after t.bigstring ~pos) ?(flags = Sync_flags.empty) () =
  check_args ~loc:"msync" ~pos ~len t.bigstring;
  unsafe_msync t.bigstring ~pos ~len ~flags
;;

module Advice = struct
  type t = Int63.t [@@deriving compare ~localize]

  let of_int63 t = t

  external madv_normal : unit -> Int63.t = "zero_mmap_MADV_NORMAL"
  external madv_random : unit -> Int63.t = "zero_mmap_MADV_RANDOM"
  external madv_sequential : unit -> Int63.t = "zero_mmap_MADV_SEQUENTIAL"
  external madv_willneed : unit -> Int63.t = "zero_mmap_MADV_WILLNEED"
  external madv_dontneed : unit -> Int63.t = "zero_mmap_MADV_DONTNEED"
  external madv_remove : unit -> Int63.t = "zero_mmap_MADV_REMOVE"
  external madv_dontfork : unit -> Int63.t = "zero_mmap_MADV_DONTFORK"
  external madv_dofork : unit -> Int63.t = "zero_mmap_MADV_DOFORK"
  external madv_thp : unit -> Int63.t = "zero_mmap_MADV_HUGEPAGE"
  external madv_no_thp : unit -> Int63.t = "zero_mmap_MADV_NOHUGEPAGE"

  let normal = madv_normal ()
  let random = madv_random ()
  let sequential = madv_sequential ()
  let willneed = madv_willneed ()
  let dontneed = madv_dontneed ()
  let remove = madv_remove ()
  let dontfork = madv_dontfork ()
  let dofork = madv_dofork ()
  let thp = madv_thp ()
  let no_thp = madv_no_thp ()

  let names =
    [ normal, "normal"
    ; random, "random"
    ; sequential, "sequential"
    ; willneed, "willneed"
    ; dontneed, "dontneed"
    ; remove, "remove"
    ; dontfork, "dontfork"
    ; dofork, "dofork"
    ; thp, "thp"
    ; no_thp, "no_thp"
    ]
  ;;

  let to_string t =
    match
      List.find_map names ~f:(fun (n, name) ->
        if Int63.equal n t then Some name else None)
    with
    | Some name -> name
    | None -> Int63.to_string t
  ;;

  let of_string s =
    match
      List.find_map names ~f:(fun (n, name) ->
        if String.equal s name then Some n else None)
    with
    | Some t -> t
    | None ->
      (try Int63.of_string s with
       | _ -> failwiths "Mmap.of_string" s sexp_of_string)
  ;;

  include
    Sexpable.Of_sexpable
      (String)
      (struct
        type nonrec t = t

        let to_sexpable = to_string
        let of_sexpable = of_string
      end)
end

external unsafe_madvise
  :  Bigstring.t
  -> pos:int
  -> len:int
  -> advice:Advice.t
  -> unit
  = "zero_mmap_madvise"

let madvise t ?(pos = 0) ?(len = len_after t.bigstring ~pos) ~advice () =
  check_args ~loc:"madvise" ~pos ~len t.bigstring;
  unsafe_madvise t.bigstring ~pos ~len ~advice
;;

external unsafe_mprotect
  :  Bigstring.t
  -> pos:int
  -> len:int
  -> protection:Protection.t
  -> unit
  = "zero_mmap_mprotect"

let mprotect t ?(pos = 0) ?(len = len_after t.bigstring ~pos) ~protection () =
  check_args ~loc:"mprotect" ~pos ~len t.bigstring;
  unsafe_mprotect t.bigstring ~pos ~len ~protection
;;

module Incore = struct
  type t =
    { present_in_core_char_array : Bigstring.t
    ; pos : int
    ; len : int
    ; num_pages : int
    }

  let create ~pos ~len ~num_pages =
    { present_in_core_char_array = Bigstring.create num_pages; pos; len; num_pages }
  ;;

  let get t ~offset =
    if offset >= t.num_pages
    then
      failwithf
        "Mincore.get: tried to get offset %d while the number of pages loaded is %d"
        offset
        t.num_pages
        ();
    if offset < 0
    then failwithf "Mincore.get: tried to retrieve a negative offset %d" offset ();
    let int_char = Char.to_int (Bigstring.get t.present_in_core_char_array offset) in
    let bit = int_char land 1 in
    match bit with
    | 1 -> true
    | 0 -> false
    | _ -> assert false
  ;;

  let get_between t ~offset ~num_pages =
    List.init num_pages ~f:(fun iter -> get t ~offset:(offset + iter))
  ;;

  let get_all t = List.init t.num_pages ~f:(fun offset -> get t ~offset)

  let percentage_in_core t =
    let is_incore = ref 0 in
    for i = 0 to t.num_pages - 1 do
      let int_char = Char.to_int (Bigstring.get t.present_in_core_char_array i) in
      let bit = int_char land 1 in
      is_incore := !is_incore + bit
    done;
    100. *. Float.of_int !is_incore /. Float.of_int t.num_pages
  ;;
end

external unsafe_mincore
  :  Bigstring.t
  -> pos:int
  -> len:int
  -> result:Bigstring.t
  -> unit
  = "zero_mmap_mincore"

let align_positions_with_page_start ~pos ~len =
  let page_size = get_page_size_in_bytes () in
  let diff = pos mod page_size in
  let pos = pos - diff in
  let len = len + diff in
  let num_pages = (len + page_size - 1) / page_size in
  if num_pages <= 0
  then
    failwithf
      "Mincore: {pos:%d; len:%d} doesn't give a strictly positive number of pages"
      pos
      len
      ();
  pos, num_pages
;;

let mincore_reload t { Incore.pos; len; present_in_core_char_array; _ } =
  unsafe_mincore t.bigstring ~pos ~len ~result:present_in_core_char_array
;;

let mincore t ?(pos = 0) ?(len = len_after t.bigstring ~pos) () =
  check_args ~loc:"mincore" ~pos ~len t.bigstring;
  let pos, num_pages = align_positions_with_page_start ~pos ~len in
  let in_core = Incore.create ~pos ~len ~num_pages in
  mincore_reload t in_core;
  in_core
;;

external unsafe_fallocate
  :  Unix.File_descr.t
  -> pos:int64
  -> len:int
  -> int
  = "zero_fallocate"
[@@noalloc]

let fallocate fd ?(pos = 0L) ~len () =
  let result = unsafe_fallocate fd ~pos ~len in
  if result <> 0
  then (
    let err = Unix.Error.of_system_int ~errno:result in
    raise (Unix.Unix_error (err, "fallocate", "")))
;;

external bigstring_to_ext_pointer
  :  Bigstring.t
  -> Ocaml_intrinsics.Ext_pointer.t
  = "zero_mmap_bigstring_to_ext_pointer"
[@@noalloc]

let unsafe_ext_pointer t = bigstring_to_ext_pointer t.bigstring
