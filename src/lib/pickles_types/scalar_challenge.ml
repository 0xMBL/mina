type 'f t = Scalar_challenge of 'f
[@@deriving bin_io, sexp, compare, eq, yojson]

let create t = Scalar_challenge t

let typ f =
  let there (Scalar_challenge x) = x in
  let back x = Scalar_challenge x in
  Snarky.Typ.(transport_var (transport f ~there ~back) ~there ~back)

let map (Scalar_challenge x) ~f = Scalar_challenge (f x)
