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

(** Library to access mailbox folders in Maildir format *)

(** The type of Maildir folders. *)
type t

(** Message flags *)
type flag =
  | NEW
  | SEEN
  | REPLIED
  | FLAGGED
  | TRASHED
  | PASSED
  | DRAFT

(** The type of message modern-unique identifiers. *)
type uniq =
  { sequence : int option
  ; boot : int option
  ; crypto_random : int option
  ; inode : int option
  ; device : int option
  ; microsecond : int option
  ; pid : int option
  ; deliveries : int option }

(* The type of message unique identifiers. *)
type uid =
  | Modern of uniq
  | Old0 of int
  | Old1 of int * int

(** The type of message informations. *)
type info =
  | Info of flag list

type message =
  { time : int
  ; uid : uid
  ; info : info
  ; host : string
  ; parameters : (string * string) list }

val is_new : message -> bool
(** [is_new message] returns [true] if [message] has the flag {!NEW}. *)

val with_new : message -> message
(** [with_new message] returns a new message {i flagged} with {!NEW}. *)

(** Type of filename. *)
type filename = string

val to_filename : message -> filename
(** [to_filename message] returns a {!filename} which corresponds to [message]
    (according to Maildir format). *)

val of_filename : filename -> (message, Rresult.R.msg) result
(** [of_filename filename] tries to parse [filename] and returns unique message
   identifier. *)

val create : pid:int -> host:string -> random:(unit -> int) -> Fpath.t -> t
(** [create ~pid ~host ~random path] returns a witness of Maildir folders at
    [path]. *)

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

module Make (IO : IO) (FS : FS with type +'a io = 'a IO.t and type key = Fpath.t) : sig
  type ('a, 'b) transmit = unit -> ('a, 'b) result IO.t
  (** Type of transmit process. *)

  val add : FS.t -> t -> time:int -> (FS.key -> ('ok, 'err) transmit) -> ('ok, 'err) result IO.t
  (** [add fs t ~time transmit] adds a new message to Maildir folders [t].
      [transmit] is the process to transmit contents of message to [tmp] folder.
      At the end of [transmit] process, [message] is moved to [new] folder as a
      new message (atomic operation). *)

  val scan_only_new : ('a -> message -> 'a IO.t) -> 'a -> FS.t -> t -> 'a IO.t
  (** [scan_only_new process acc fs t] scans only new messages in [t]. *)

  val fold : ('a -> message -> 'a IO.t) -> 'a -> FS.t -> t -> 'a IO.t
  (** [fold process acc fs t] scans messages [cur]rent and [new] messages in [t]. *)

  val get : t -> message -> FS.key
  (** [get t message] returns location of [message] in [t]. *)

  val remove : FS.t -> t -> message -> unit IO.t
  (** [remove fs t message] removes [message] from [t] and [fs]. *)

  val get_flags : FS.t -> t -> message -> flag list IO.t
  (** [get_flags fs t message] returns flags of [message] available in [t] and [fs]. *)

  val set_flags : FS.t -> t -> message -> flag list -> unit IO.t
  (** [set_flags fs t messages flags] sets flags of [message] in [t] and [fs] to [flags]. *)
end
