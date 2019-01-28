(* The MIT License (MIT)

   Copyright (c) 2014-2017 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

type flag =
  | NEW
  | SEEN
  | REPLIED
  | FLAGGED
  | TRASHED
  | PASSED
  | DRAFT

let compare_flag a b = match a, b with
  | NEW, NEW
  | SEEN, SEEN
  | REPLIED, REPLIED
  | FLAGGED, FLAGGED
  | TRASHED, TRASHED
  | PASSED, PASSED
  | DRAFT, DRAFT -> 0
  | NEW, _ -> 1
  | SEEN, _ -> 1
  | REPLIED, _ -> 1
  | FLAGGED, _ -> 1
  | TRASHED, _ -> 1
  | PASSED, _ -> 1
  | DRAFT, _ -> 1

let char_of_flag = function
  | SEEN -> Some 'S'
  | REPLIED -> Some 'R'
  | FLAGGED -> Some 'F'
  | TRASHED -> Some 'T'
  | PASSED -> Some 'P'
  | DRAFT -> Some 'D'
  | NEW -> None

let string_of_flags flags =
  let l =
    List.fold_right (fun f l ->
      match char_of_flag f with Some c -> c :: l | None -> l
    ) flags []
  in
  let l = List.sort_uniq compare l in
  let b = Bytes.create (List.length l) in
  List.iteri (fun i c -> Bytes.set b i c) l;
  Bytes.unsafe_to_string b

let canonicalize_flags flags =
  let module Set = Set.Make(struct type t = flag let compare = compare_flag end) in
  let set = List.fold_right Set.add flags Set.empty in
  Set.elements set

let pp_flags = Fmt.using string_of_flags Fmt.string

type uniq =
  { sequence : int option
  ; boot : int option
  ; crypto_random : int option
  ; inode : int option
  ; device : int option
  ; microsecond : int option
  ; pid : int option
  ; deliveries : int option }

let pp_uniq ppf t =
  let using_dec key = Fmt.using (function None -> "" | Some v -> Fmt.strf "%c%d" key v) Fmt.string in
  let using_hex key = Fmt.using (function None -> "" | Some v -> Fmt.strf "%c%x" key v) Fmt.string in
  Fmt.pf ppf "%a%a%a%a%a%a%a%a" (* lol *)
    (using_hex '#') t.sequence
    (using_hex 'X') t.boot
    (using_hex 'R') t.crypto_random
    (using_hex 'I') t.inode
    (using_hex 'V') t.device
    (using_dec 'M') t.microsecond
    (using_dec 'P') t.pid
    (using_dec 'Q') t.deliveries

let default_uniq =
  { sequence = None
  ; boot = None
  ; crypto_random = None
  ; inode = None
  ; device = None
  ; microsecond = None
  ; pid = None
  ; deliveries = None }

type uid =
  | Modern of uniq
  | Old0 of int
  | Old1 of int * int

let pp_uid ppf = function
  | Modern uniq -> Fmt.pf ppf "%a" pp_uniq uniq
  | Old0 n -> Fmt.int ppf n
  | Old1 (n, m) -> Fmt.pf ppf "%d_%d" n m

type info =
  | Info of flag list

let pp_info ppf = function
  | Info [] -> Fmt.nop ppf ()
  | Info flags -> Fmt.prefix Fmt.(const char ',') pp_flags ppf flags

type message =
  { time : int
  ; uid : uid
  ; info : info
  ; host : string
  ; parameters : (string * string) list }

let is_new { info = Info flags; _ } = List.exists ((=) NEW) flags

let sanitize_host host =
  let len = String.length host in
  let res = Buffer.create len in
  for i = 0 to len - 1 do
    match host.[i] with
    | '/' -> Buffer.add_string res "\\057"
    | ':' -> Buffer.add_string res "\\072"
    | c -> Buffer.add_char res c
  done;
  Buffer.contents res

let pp_host = Fmt.using sanitize_host Fmt.string

let pp_parameter ppf (k, v) =
  Fmt.pf ppf "%s=%s" k v

let pp_message ppf t =
  Fmt.pf ppf "%d%a%a%a:2,%a"
    t.time pp_uid t.uid pp_host t.host
    Fmt.(iter List.iter (prefix (const char ',') pp_parameter)) t.parameters
    pp_info t.info

type filename = string

let to_filename = Fmt.to_to_string pp_message

let with_new message =
  let Info flags = message.info in
  if List.exists (function NEW -> true | _ -> false) flags
  then { message with info = Info (NEW :: flags) }
  else message

module Parser = struct
  open Angstrom

  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_hexadigit = function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false
  let number = take_while1 is_digit >>| int_of_string
  let hexanumber = take_while1 is_hexadigit >>| fun str -> int_of_string ("0x" ^ str)

  let sequence = char '#' *> hexanumber
  let boot = char 'X' *> hexanumber
  let crypto_random = char 'R' *> hexanumber
  let inode = char 'I' *> hexanumber
  let device = char 'V' *> hexanumber
  let microsecond = char 'M' *> number
  let pid = char 'P' *> number
  let deliveries = char 'Q' *> number

  let modern =
    choice
      [ (sequence >>| fun x -> `Sequence x)
      ; (boot >>| fun x -> `Boot x)
      ; (crypto_random >>| fun x -> `Crypto_random x)
      ; (inode >>| fun x -> `Inode x)
      ; (device >>| fun x -> `Device x)
      ; (microsecond >>| fun x -> `Microsecond x)
      ; (pid >>| fun x -> `Pid x)
      ; (deliveries >>| fun x -> `Deliveries x) ]

  let modern =
    many1 modern >>| fun lst -> List.fold_left
    (fun t -> function
      | `Sequence x -> { t with sequence = Some x }
      | `Boot x -> { t with boot = Some x }
      | `Crypto_random x -> { t with crypto_random = Some x }
      | `Inode x -> { t with inode = Some x }
      | `Device x -> { t with device = Some x }
      | `Microsecond x -> { t with microsecond = Some x }
      | `Pid x -> { t with pid = Some x }
      | `Deliveries x -> { t with deliveries = Some x })
    default_uniq lst

  let old1 = number <* char '_' >>= fun n -> number >>= fun m -> return (n, m)
  let old0 = number

  let uid =
    choice
      [ (modern >>| fun x -> Modern x)
      ; (old1 >>| fun (n, m) -> Old1 (n, m))
      ; (old0 >>| fun x -> Old0 x) ]

  let host =
    take_while1 (function ':' -> false | _ -> true)
    >>| fun str ->
    let len = String.length str in
    let str = Bytes.unsafe_of_string str in
    for i = 0 to len - 1
    do if Bytes.unsafe_get str i = '\057' then Bytes.unsafe_set str i '/' ;
       if Bytes.unsafe_get str i = '\072' then Bytes.unsafe_set str i ':' done ;
    Bytes.unsafe_to_string str

  let parameter =
    take_while1 (function '=' | ':' | ',' -> false | _ -> true) <* char '=' >>= fun key ->
    take_while1 (function '=' | ':' | ',' -> false | _ -> true) >>= fun value -> return (key, value)

  let flag =
    choice
      [ (char 'P' *> return PASSED)
      ; (char 'R' *> return REPLIED)
      ; (char 'S' *> return SEEN)
      ; (char 'T' *> return TRASHED)
      ; (char 'D' *> return DRAFT)
      ; (char 'F' *> return FLAGGED) ]

  let filename =
    number >>= fun time ->
    uid >>= fun uid ->
    host >>= fun host ->
    many (char ',' *> parameter) >>= fun parameters ->
    char ':' *> peek_char >>= function
      (* TODO: | Some '1' -> _ *)
    | Some '2' ->
        char ',' *> many flag >>| fun flags -> { time; uid; host; parameters; info = Info flags }
    | _ -> return { time; uid; host; parameters; info = Info [] }

  let of_filename input =
    match parse_string filename input with
    | Ok v -> Ok v
    | Error _ -> Rresult.R.error_msgf "Invalid filename: %s" input
end

let of_filename = Parser.of_filename

type t =
  { pid : int
  ; host : string
  ; path : Fpath.t
  ; random : unit -> int
  ; mutable mtime_new : int64
  ; mutable mtime_cur : int64
  ; mutable delivered : int }

let create ~pid ~host ~random path =
  { pid
  ; host= sanitize_host host
  ; random
  ; path
  ; mtime_new = -1L
  ; mtime_cur = -1L
  ; delivered = 0 }

let new_message ~time t =
  let message =
    { time
    ; uid= Modern { default_uniq with crypto_random= Some (t.random ())
                                    ; pid= Some t.pid
                                    ; deliveries= Some t.delivered }
    ; host= t.host
    ; info= Info [ NEW ]
    ; parameters= [] } in
  t.delivered <- t.delivered + 1 ;
  message

module type IO = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val return : 'a -> 'a t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
end

module type FS = sig
  type key
  type t

  type +'a io

  val mtime : t -> key -> int64 io
  val fold : t -> key -> (key -> 'a -> 'a io) -> 'a -> 'a io
  val rename : t -> key -> key -> unit io
  val remove : t -> key -> unit io
  val exists : t -> key -> bool io
end

module Make
    (IO : IO)
    (FS : FS with type +'a io = 'a IO.t and type key = Fpath.t) = struct
  open IO

  let add fs t ~time transmit =
    let uid = new_message ~time t in
    let message = to_filename uid in
    let transmit = transmit Fpath.(t.path / "tmp" / message) in
    transmit () >>= function
    | Ok v ->
        let o = Fpath.(t.path / "tmp" / message) in
        let n = Fpath.(t.path / "new" / message) in
        FS.rename fs o n >>= fun () -> return (Ok v)
    | Error _ as err -> return err

  let scan_only_new computation acc fs t =
    FS.mtime fs Fpath.(t.path / "new")
    >>= fun mtime_new ->
    (if mtime_new <> t.mtime_new
     then ( t.mtime_new <- mtime_new
          ; return true )
     else return false)
    >>= fun new_changed ->
    FS.mtime fs Fpath.(t.path / "cur") >>= fun mtime_cur ->
    (if mtime_cur <> t.mtime_cur
     then ( t.mtime_cur <- mtime_cur
          ; return true )
     else return false)
    >>= fun cur_changed ->
    if new_changed || cur_changed
    then
      let computation_new path acc =
        let filename = Fpath.basename path in
        match Parser.of_filename filename with
        | Ok v -> computation acc (with_new v)
        | Error _ -> return acc in
      let computation_cur path acc =
        let filename = Fpath.basename path in
        match Parser.of_filename filename with
        | Ok v -> computation acc v
        | Error _ -> return acc in
      FS.fold fs Fpath.(t.path / "new") computation_new acc >>= fun acc ->
      FS.fold fs Fpath.(t.path / "cur") computation_cur acc >>= fun acc ->
      return acc
    else return acc

  let fold computation acc fs t =
    let computation_new path acc =
      let filename = Fpath.basename path in
      match Parser.of_filename filename with
      | Ok v -> computation acc (with_new v)
      | Error _ -> return acc in
    let computation_cur path acc =
      let filename = Fpath.basename path in
      match Parser.of_filename filename with
      | Ok v -> computation acc v
      | Error _ -> return acc in
    FS.fold fs Fpath.(t.path / "new") computation_new acc >>= fun acc ->
    FS.fold fs Fpath.(t.path / "cur") computation_cur acc >>= fun acc ->
    return acc

  let get t uid =
    let message = to_filename uid in
    if is_new uid
    then Fpath.(t.path / "new" / message)
    else Fpath.(t.path / "cur" / message)

  let remove fs t uid =
    let message = to_filename uid in
    if is_new uid
    then FS.remove fs Fpath.(t.path / "new" / message)
    else FS.remove fs Fpath.(t.path / "cur" / message)

  let get_flags fs t uid =
    let message = to_filename uid in
    let directory = if is_new uid then "new" else "cur" in
    FS.exists fs Fpath.(t.path / directory / message) >>= function
    | true ->
        let Info flags = uid.info in
        return flags
    | false -> return []

  let set_flags fs t uid flags' =
    let message = to_filename uid in
    let directory = if is_new uid then "new" else "cur" in
    FS.exists fs Fpath.(t.path / directory / message) >>= function
    | false -> return ()
    | true ->
        let Info flags = uid.info in
        let flags = canonicalize_flags flags in
        let flags' = canonicalize_flags flags' in
        if flags <> flags'
        then
          let message' = to_filename { uid with info = Info flags' } in
          FS.rename fs Fpath.(t.path / directory / message) Fpath.(t.path / directory / message')
        else return ()
end
