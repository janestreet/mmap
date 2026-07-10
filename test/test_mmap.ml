open! Core
open Import
open Mmap
module Unix = Core_unix

let test f =
  let file, fd = Unix.mkstemp (Filename.temp_dir_name ^/ "mmap_unit_test") in
  protect
    ~finally:(fun () ->
      Unix.unlink file;
      Unix.close fd)
    ~f:(fun () -> f fd)
;;

let%expect_test "mmap_read" =
  test (fun fd ->
    let data = Bigstring.create (8 * 4096) in
    for i = 0 to 4095 do
      Bigstring.unsafe_set_int64_le data ~pos:(i * 8) (i + 1)
    done;
    Bigstring_unix.really_write fd data;
    let { Unix.st_size; _ } = Unix.fstat fd in
    let file_length = Int64.to_int_exn st_size in
    let mmap =
      mmap64
        fd
        ~visibility:Map_visibility.Shared
        ~len:file_length
        ~protection:Protection.(read + write)
        ()
    in
    let mdata = bigstring mmap in
    for i = 0 to 4095 do
      let retrieved = Bigstring.unsafe_get_int64_le_exn mdata ~pos:(i * 8) in
      let expected = i + 1 in
      require
        (expected = retrieved)
        ~if_false_then_print_s:
          (lazy
            [%message
              "mmap should have retrieved previously written data"
                (expected : int)
                (retrieved : int)])
    done;
    Bigstring.unsafe_destroy mdata;
    [%expect {| |}])
;;

let require_eq pos expected retrieved =
  require
    ~here:pos
    (expected = retrieved)
    ~if_false_then_print_s:
      (lazy
        [%message
          "mmap should have retrieved mmap written data"
            (expected : int)
            (retrieved : int)])
;;

let test_write ?pos ~fd () =
  let mmap =
    mmap64
      fd
      ?pos
      ~visibility:Map_visibility.Shared
      ~len:(8 * 4096)
      ~protection:Protection.(read + write)
      ()
  in
  let mdata = bigstring mmap in
  for i = 0 to 4095 do
    Bigstring.unsafe_set_int64_le mdata ~pos:(i * 8) (i + 1)
  done;
  for i = 0 to (Bigstring.length mdata / 8) - 1 do
    let retrieved = Bigstring.unsafe_get_int64_le_exn mdata ~pos:(i * 8) in
    let expected = i + 1 in
    require_eq [%here] expected retrieved
  done;
  msync mmap ~flags:Sync_flags.sync ();
  Bigstring.unsafe_destroy mdata
;;

let%expect_test "mmap_write" =
  test (fun fd ->
    test_write ~fd ();
    [%expect {| |}])
;;

let%expect_test "msync is broken when mmap was created with an offset other than a \
                 multiple of the page size"
  =
  (* The cstub discards errno when raising the unix error, but in the error is always
     EINVAL. Fix is probably for the [zero_mmap_mysnc] stub to do the same adjustment as
     the [zero_mmap_mmap64] stub does with SC_PAGESIZE. *)
  test (fun fd ->
    Expect_test_helpers_core.show_raise (fun () ->
      test_write ~fd ~pos:(Int64.of_int 31) ());
    [%expect {| (raised (Unix.Unix_error "Invalid argument" msync "")) |}])
;;

let%expect_test "msync works broken when mmap was created with an offset which is the \
                 page size"
  =
  test (fun fd ->
    Expect_test_helpers_core.show_raise (fun () ->
      test_write ~fd ~pos:(Int64.of_int 4096) ());
    [%expect {| "did not raise" |}])
;;

let get_vmlock () =
  let lines = In_channel.read_lines "/proc/self/status" in
  let vmlock_line = List.find_exn lines ~f:(String.is_prefix ~prefix:"VmLck:") in
  Scanf.sscanf vmlock_line "VmLck: %d kB" (fun x -> x * 1024)
;;

let require_locked ~expected ~locked =
  require
    (expected = locked)
    ~if_false_then_print_s:
      (lazy
        [%message
          "Did not lock the expected amount of virtual memory"
            (expected : int)
            (locked : int)])
;;

let test_mremap_lock ~do_lock =
  test (fun fd ->
    let data = Bigstring.create (2 * 4096) in
    Bigstring_unix.really_write fd data;
    let vmlock0 = get_vmlock () in
    let mmap =
      mmap64
        fd
        ~visibility:Map_visibility.Shared
        ~len:(1 * 4096)
        ~protection:Protection.(read + write)
        ?flags:Map_flags.(if do_lock then Some map_locked else None)
        ()
    in
    let vmlock1 = get_vmlock () in
    let locked = vmlock1 - vmlock0 in
    let expected = if do_lock then 4096 else 0 in
    require_locked ~expected ~locked;
    let mmap = mremap mmap ~new_size:(2 * 4096) ~flags:Mremap_flags.maymove () in
    let vmlock2 = get_vmlock () in
    let locked = vmlock2 - vmlock1 in
    let expected = if do_lock then 4096 else 0 in
    require_locked ~expected ~locked;
    Bigstring.unsafe_destroy (bigstring mmap))
;;

let%expect_test "mmap and mremap without MAP_LOCK" =
  test_mremap_lock ~do_lock:false;
  [%expect {| |}]
;;

let%expect_test "anonymous mmap" =
  let mmap =
    mmap_anonymous
      ~len:4096
      ~visibility:Map_visibility.Private
      ~protection:Protection.(read + write)
      ()
  in
  let data = bigstring mmap in
  (* Writing and reading should work *)
  Bigstring.unsafe_set_int64_le data ~pos:37 52;
  let got = Bigstring.unsafe_get_int64_le_exn data ~pos:37 in
  require_eq [%here] 52 got;
  (* I'd like to force a remap here, but that requires knowing we have another mapping
     directly after [mmap]; we can't do that without a way to specify a MAP_FIXED address.
     But it should work whether or not we move, it just limits our test coverage.
  *)
  let mmap = mremap mmap ~new_size:(1024 * 1024 * 1024) ~flags:Mremap_flags.maymove () in
  let data = bigstring mmap in
  (* Remapping shouldn't break the contents *)
  let got = Bigstring.unsafe_get_int64_le_exn data ~pos:37 in
  require_eq [%here] 52 got;
  [%expect {| |}]
;;

let%expect_test "access via ext_pointer" =
  let mmap =
    mmap_anonymous
      ~len:4096
      ~visibility:Map_visibility.Private
      ~protection:Protection.(read + write)
      ()
  in
  (* Writing via an Ext_pointer.t should work *)
  let pointer = unsafe_ext_pointer mmap in
  Ocaml_intrinsics.Ext_pointer.store_unboxed_int64 pointer 52L;
  let data = bigstring mmap in
  let got = Bigstring.unsafe_get_int64_le_exn data ~pos:0 in
  require_eq [%here] 52 got;
  (* Reading via an Ext_pointer.t should work *)
  Bigstring.unsafe_set_int64_le data ~pos:0 70;
  let got = Ocaml_intrinsics.Ext_pointer.load_unboxed_int64 pointer in
  require_eq [%here] 70 (got |> Int.of_int64_exn)
;;

let%expect_test "fallocate extends a file with zero-filled blocks" =
  test (fun fd ->
    let len = 8 * 4096 in
    let { Unix.st_size = size_before; _ } = Unix.fstat fd in
    [%test_result: int64] ~expect:0L size_before;
    fallocate fd ~len ();
    let { Unix.st_size = size_after; _ } = Unix.fstat fd in
    [%test_result: int64] ~expect:(Int64.of_int len) size_after;
    let mmap =
      mmap64
        fd
        ~visibility:Map_visibility.Shared
        ~len
        ~protection:Protection.(read + write)
        ()
    in
    let data = bigstring mmap in
    for i = 0 to (len / 8) - 1 do
      [%test_result: int] ~expect:0 (Bigstring.unsafe_get_int64_le_exn data ~pos:(i * 8))
    done;
    Bigstring.unsafe_destroy data;
    [%expect {| |}])
;;

let%expect_test "fallocate at a non-zero offset extends the file" =
  test (fun fd ->
    fallocate fd ~pos:4096L ~len:4096 ();
    let { Unix.st_size; _ } = Unix.fstat fd in
    [%test_result: int64] ~expect:8192L st_size;
    [%expect {| |}])
;;

let%expect_test "fallocate raises Unix_error on a bad fd" =
  Expect_test_helpers_core.show_raise (fun () ->
    fallocate (Core_unix.File_descr.of_int (-1)) ~len:4096 ());
  [%expect {| (raised (Unix.Unix_error "Bad file descriptor" fallocate "")) |}]
;;
