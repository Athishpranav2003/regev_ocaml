
type private_key = { s : Z.t array; n : int; m : int; bound : int; q : Z.t; range : Z.t }
type public_key = { a : Z.t array array; n : int; m : int; bound : int; q : Z.t; range : Z.t }

val gen_secret_key : int -> int -> int -> Z.t -> Z.t -> private_key

val gen_public_key : private_key -> public_key

val encrypt : public_key -> Z.t -> ?r:Z.t array -> unit -> Z.t array 

val decrypt : private_key -> Z.t array -> Z.t

val add : Z.t array -> Z.t array -> Z.t array
