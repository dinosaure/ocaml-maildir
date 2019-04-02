let () = Printexc.record_backtrace true

module Store = Maildir_unix

let sanity_make on =
  let open Rresult.R in
  Bos.OS.Dir.create Fpath.(on / "cur") >>= fun _ ->
  Bos.OS.Dir.create Fpath.(on / "new") >>= fun _ ->
  Bos.OS.Dir.create Fpath.(on / "tmp") >>= fun _ ->
  Rresult.R.ok ()

let root = Rresult.R.get_ok (Bos.OS.Dir.current ())
(* XXX(dinosaure): [getcwd] should never fail. *)

let () = Random.self_init ()
let ( <.> ) f g = fun x -> f (g x)
let gettimeofday = Int64.of_float <.> Unix.gettimeofday

let maildir_create root =
  Maildir.create
    ~pid:(Unix.getpid ())
    ~host:(Unix.gethostname ())
    ~random:(Int64.of_int <.> Random.bits)
    root

let global_maildir = ref None
let get_global_maildir () = match !global_maildir with
  | Some maildir -> maildir
  | None -> assert false (* XXX(dinosaure): should never occur. *)

let populate () =
  let open Rresult.R in
  Bos.OS.Dir.tmp ~dir:root "maildir-%s" >>= fun maildir ->
  sanity_make maildir >>= fun () ->
  let maildir = maildir_create maildir in
  Alcotest.(check bool) "populate" Store.(verify fs maildir) true ;
  global_maildir := Some maildir ; (* XXX(dinosaure): save it. *)
  Rresult.R.ok ()

let cons x r = x :: r
let only_new a x = if Maildir.is_new x then x :: a else a

let add_one_from_ex_nihilo () =
  let open Rresult.R in
  let mtime = gettimeofday () in
  Bos.OS.File.tmp "ex-nihilo-%s" >>= fun ex_nihilo ->
  let maildir = get_global_maildir () in
  Store.(add fs maildir ~time:(gettimeofday ()) (fun target fs -> Store.transmit fs ex_nihilo target)) >>= fun () ->
  Unix.sleep 1 ; (* XXX(dinosaure): ok, so [mtime] uses second but OCaml is too
                    fast to see any update from file-system. So we sleep 1s to
                    have a different [mtime] than [maildir.mtime_new].

                    NOTE: [mtime], in point of view of [maildir] does not expect
                    to be a second - see [test_mem]/[maildir_mem] where it's
                    just an integer which it is incremented for any operation.

                    So, this hack is really specific to Unix back-end. *)
  Bos.OS.File.delete ex_nihilo >>= fun _ ->
  let news = Store.scan_only_new (fun news message -> message :: news) [] Store.fs maildir in
  let news = List.fold_left only_new [] news in
  match news with
  | [] -> Alcotest.failf "Add fails"
  | [ message ] ->
      let key = Maildir.to_fpath maildir message in
      Bos.OS.File.exists key >>= fun exists ->
      if exists
      then
        let stat = Unix.stat (Fpath.to_string key) in
        Alcotest.(check int64) "mtime" (Int64.of_float stat.Unix.st_mtime) mtime ;
        Rresult.R.ok ()
      else Alcotest.failf "%a does not exist" Fpath.pp key
  | news ->
      Alcotest.failf "Too much messages (%d new messages): @[<hov>%a@]"
        (List.length news)
        Fmt.(Dump.list (using Maildir.value Maildir.pp_message)) news

let list_of_len n =
  let rec go acc = function
    | 0 -> acc
    | n -> go (n :: acc) (pred n) in
  if n < 0 then Fmt.invalid_arg "list_of_len"
  else go [] n

let add_some_from_ex_nihilo n =
  let open Rresult.R in
  let maildir = get_global_maildir () in
  let results =
    List.map (fun _ ->
        Bos.OS.File.tmp "ex-nihilo-%s" >>= fun ex_nihilo ->
        Store.add Store.fs maildir ~time:(gettimeofday ()) (fun target fs -> Store.transmit fs ex_nihilo target) >>= fun () ->
        Unix.sleep 1 ;
        Bos.OS.File.delete ex_nihilo)
      (list_of_len n) in
  let () = Alcotest.(check bool) "append" (List.for_all (function Ok _ -> true | _ -> false) results) true in
  let news = Store.scan_only_new (fun r m -> m :: r) [] Store.fs maildir in
  let news = List.fold_left only_new [] news in
  Alcotest.(check int) "number of new messages" n (List.length news)

let commit_new_message () =
  let maildir = get_global_maildir () in
  match Store.fold only_new [] Store.fs maildir with
  | [] ->
      Alcotest.failf "Commit fails: no new message"
  | [ message ] ->
      Store.commit Store.fs maildir message ;
      let news = Store.fold only_new [] Store.fs maildir in
      if List.length news <> 0
      then Alcotest.failf "Commit fails, new messages: %a." Fmt.(Dump.list (using Maildir.value Maildir.pp_message)) news
      else Rresult.R.ok ()
  | news ->
      Alcotest.failf "Too much news messages (%d new messages): @[<hov>%a@]"
        (List.length news)
        Fmt.(Dump.list (using Maildir.value Maildir.pp_message)) news

let take n lst =
  let rec go acc lst n = match n, lst with
    | 0, _ -> List.rev acc
    | n, x :: r -> go (x :: acc) r (pred n)
    | _, [] -> Fmt.invalid_arg "take" in
  if n < 0 then Fmt.invalid_arg "take" else go [] lst n

let commit_some n =
  let maildir = get_global_maildir () in
  match Store.fold only_new [] Store.fs maildir with
  | [] -> Alcotest.failf "Commit some fails: no new messages"
  | news ->
      if List.length news < n
      then Alcotest.failf "Commit fails: \
                           number of new messages diverge (%d <> %d)" (List.length news) n ;
      List.iter (Store.commit Store.fs maildir) (take n news) ;
      let news' = Store.fold only_new [] Store.fs maildir in
      if List.length news - List.length news' <> n
      then Alcotest.failf "Commit fails: committed:%d" (List.length news - List.length news')
      else ()

let test_populate () = Alcotest.test_case "populate" `Quick (Rresult.R.failwith_error_msg <.> populate)
let test_add_one () = Alcotest.test_case "add" `Quick (Rresult.R.failwith_error_msg <.> add_one_from_ex_nihilo)
let test_commit () = Alcotest.test_case "commit" `Quick (Rresult.R.failwith_error_msg <.> commit_new_message)
let test_add_some n = Alcotest.test_case (Fmt.strf "add some (%d)" n) `Quick (fun () -> add_some_from_ex_nihilo n)
let test_commit_some n = Alcotest.test_case (Fmt.strf "commit some (%d)" n) `Quick (fun () -> commit_some n)

let () =
  Alcotest.run "maildir-unix"
    [ "populate",
      [ test_populate () ]
    ; "add & commit",
      [ test_add_one ()
      ; test_commit ()
      ; test_add_one ()
      ; test_commit ()
      ; test_add_one ()
      ; test_commit ()
      ; test_add_one ()
      ; test_commit ()
      ; test_add_one ()
      ; test_commit () ]
    ; "add some & commit some",
      [ test_add_some 2
      ; test_commit_some 2
      ; test_add_some 2
      ; test_commit_some 1
      ; test_commit_some 1 ] ]

