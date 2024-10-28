
type private_key = { s : Z.t array; n : int; m : int; bound : int; q : Z.t }
type public_key = { a : Z.t array array; n : int; m : int; bound : int; q : Z.t }

val gen_secret_key : int -> int -> int -> Z.t -> private_key

val gen_public_key : private_key -> public_key

val encrypt : public_key -> int -> Z.t array

val decrypt : private_key -> Z.t array -> int
