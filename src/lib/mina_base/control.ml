[%%import "/src/config.mlh"]

open Core_kernel

(* TODO: temporary hack *)
[%%ifdef consensus_mechanism]

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      | Proof of Pickles.Side_loaded.Proof.Stable.V1.t
      | Signature of Signature.Stable.V1.t
      | None_given
    [@@deriving sexp, equal, yojson, hash, compare]

    let to_latest = Fn.id
  end
end]

(* lazy, to prevent spawning Rust threads at startup, which prevents daemonization *)
let gen_with_dummies : t Quickcheck.Generator.t Lazy.t =
  lazy
    (Quickcheck.Generator.of_list
       (let dummy_proof =
          let n2 = Pickles_types.Nat.N2.n in
          let proof = Pickles.Proof.dummy n2 n2 n2 in
          Proof proof
        in
        let dummy_signature = Signature Signature.dummy in
        [ dummy_proof; dummy_signature; None_given ]))

[%%else]

[%%versioned
module Stable = struct
  module V1 = struct
    type t = Proof of unit | Signature of Signature.Stable.V1.t | None_given
    [@@deriving sexp, equal, yojson, hash, compare]

    let to_latest = Fn.id
  end
end]

[%%endif]

module Tag = struct
  type t = Proof | Signature | None_given [@@deriving equal, compare, sexp]
end

let tag : t -> Tag.t = function
  | Proof _ ->
      Proof
  | Signature _ ->
      Signature
  | None_given ->
      None_given
