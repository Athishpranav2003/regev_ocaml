type private_key = {s: Z.t array; n: int; m: int; bound: int; q:Z.t }

type public_key = {a: Z.t array array; n: int ; m: int; bound: int; q:Z.t }

let gen_secret_key n m bound q =
  let _ = if (float n < log (log (Z.to_float q))) then
    failwith "n must be greater than log(log(q))"
  in
  let s = Array.init n (fun _ -> Mirage_crypto_pk.Z_extra.gen q) in
  s.(n-1) <- Z.(~$(-1));
  {s; n; m; bound; q}

let gen_public_key (priv_key:private_key) =
  let n = priv_key.n in
  let m = priv_key.m in
  let bound = priv_key.bound in
  let q = priv_key.q in
  let a = Array.init_matrix n m (fun _ _ -> Mirage_crypto_pk.Z_extra.gen q) in
  for j = 0 to m - 1 do
    a.(n-1).(j) <- Z.of_int (Random.int bound);
    for i = 0 to n - 2 do
      a.(n-1).(j) <- Z.rem (Z.add a.(n-1).(j) (Z.mul a.(i).(j) priv_key.s.(i))) q
    done
  done;
  {a; n; m; bound; q}

let encrypt (public_key:public_key) msg =
  let r = Array.init public_key.m (fun _ -> (Mirage_crypto_pk.Z_extra.gen (Z.of_int public_key.bound))) in

  let cipher = Array.make public_key.n Z.zero in
  cipher.(public_key.n-1) <- Z.(mul (of_int msg)  (div public_key.q Z.(~$2)));
  for i = 0 to public_key.n - 1 do
    for j = 0 to public_key.m - 1 do
      cipher.(i) <- Z.(add cipher.(i) (mul public_key.a.(i).(j)  r.(j)))
    done
  done;
  cipher
  
let decrypt (private_key:private_key) c =
  let msg = ref Z.zero in
  for i = 0 to private_key.n - 1 do
    msg :=  Z.(rem (add !msg (mul c.(i)  private_key.s.(i))) private_key.q)
  done;
  if (Z.abs !msg) < (Z.div private_key.q Z.(~$4)) then
    0
  else 1
