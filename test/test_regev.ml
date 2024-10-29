open Regev

let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)
let n = 32
let q = Z.shift_left Z.one 32
let bound = 5

let m = n * 2 * 32

let secret_key = gen_secret_key n m bound q Z.(~$4)

let public_key = gen_public_key secret_key 

let _ =
  let msg = Z.zero in
  let decrypted = decrypt secret_key (encrypt public_key msg ()) in
  assert (msg = decrypted);
  let msg = Z.one in
  let decrypted = decrypt secret_key (encrypt public_key msg ()) in
  assert (msg = decrypted);
  let encrypted = encrypt public_key msg () in
  let decrypted = decrypt secret_key (add encrypted (add encrypted encrypted)) in
  assert (Z.equal decrypted (Z.(~$3)));
  let r_1 = Array.init public_key.m (fun _ -> (Mirage_crypto_pk.Z_extra.gen Z.(~$2))) in
  let r_2 = Array.init public_key.m (fun _ -> (Mirage_crypto_pk.Z_extra.gen Z.(~$2))) in
  let encrypted_1 = encrypt public_key Z.one ~r:r_1 () in
  let encrypted_2 = encrypt public_key Z.(~$2) ~r:r_2 () in
  let encrypted = add encrypted_1 encrypted_2 in
  let encrypted_true = encrypt public_key Z.(~$3) ~r:(add r_1 r_2) () in
  assert (Array.for_all2 Z.equal encrypted encrypted_true)
