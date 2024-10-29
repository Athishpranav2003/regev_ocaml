type private_key = {s: Z.t array; n: int; m: int; bound: int; q:Z.t; range: Z.t}

type public_key = {a: Z.t array array; n: int ; m: int; bound: int; q:Z.t; range: Z.t }

let sample_gaussian bound sigma =
  let gaussian_pdf x mu sigma =
    let pi = 4.0 *. atan 1.0 in
    let exp_term = -.((x -. mu) ** 2.0) /. (2.0 *. sigma ** 2.0) in
    (1.0 /. (sigma *. sqrt (2.0 *. pi))) *. exp exp_term in

  let discrete_gaussian_probs x_min x_max mu sigma =
    let range = List.init (x_max - x_min + 1) (fun i -> x_min + i) in
    let probs = List.map (fun x -> gaussian_pdf (float_of_int x) mu sigma) range in
    let sum_probs = List.fold_left (+.) 0.0 probs in
    List.map (fun p -> p /. sum_probs) probs in

  let probs = discrete_gaussian_probs (-bound) bound 0.0 sigma in
  let cumulative_probs = List.fold_left (fun acc p ->
    match acc with
    | [] -> [p]
    | h :: _ -> (p +. h) :: acc) [] probs |> List.rev
  in
  let u = Random.float 1.0 in
  let rec find_sample i = function
    | [] -> bound
    | p :: ps -> if u <= p then (-bound) + i else find_sample (i + 1) ps
    in
    find_sample 0 cumulative_probs

let gen_secret_key n m bound q range =
  let _ = if (float n < log (log (Z.to_float q))) then
    failwith "n must be greater than log(log(q))"
  in
  let _ = if (float m < 2.0 *. float n *. log (Z.to_float q)) then
    failwith "m must be greater than 2nlog(q)"
  in
  let _ = if bound*m > Z.to_int q / 4 then
    failwith "bound*m must be less than q/4"
  in
  let s = Array.init n (fun _ -> Mirage_crypto_pk.Z_extra.gen q) in
  s.(n-1) <- Z.(~$(-1));
  {s; n; m; bound; q; range}

let gen_public_key (priv_key:private_key) =
  let n = priv_key.n in
  let m = priv_key.m in
  let bound = priv_key.bound in
  let q = priv_key.q in
  let range = priv_key.range in
  let a = Array.init_matrix n m (fun _ _ -> Mirage_crypto_pk.Z_extra.gen q) in
  for j = 0 to m - 1 do
    a.(n-1).(j) <- Z.of_int (abs (sample_gaussian bound 10000000000.0));
    for i = 0 to n - 2 do
      a.(n-1).(j) <- (Z.add a.(n-1).(j) (Z.mul a.(i).(j) priv_key.s.(i)))
    done
  done;
  {a; n; m; bound; q; range}

let encrypt (public_key:public_key) msg =
  let r = Array.init public_key.m (fun _ -> (Mirage_crypto_pk.Z_extra.gen Z.(~$2))) in

  let cipher = Array.make public_key.n Z.zero in
  cipher.(public_key.n-1) <- Z.(mul msg  (div public_key.q public_key.range));
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
  Z.(div (abs !msg) (div private_key.q private_key.range))

let add c1 c2 =
  Array.map2 Z.add c1 c2
