open Core
module Unix = Core_unix

let main () =
  let len = 4096 in
  let memory =
    Mmap.mmap_anonymous
      ~visibility:Shared
      ~flags:Mmap.Map_flags.map_shared
      ~protection:Mmap.Protection.(read + write)
      ~len
      ()
    |> Mmap.bigstring
  in
  Bigstring.memset memory ~pos:0 ~len 'A';
  printf "read before fork: %c\n%!" memory.{0};
  match Unix.fork () with
  | `In_the_child -> memory.{0} <- 'B'
  | `In_the_parent child ->
    Unix.waitpid_exn child;
    printf "read after fork: %c\n%!" memory.{0}
;;

let () = main ()
