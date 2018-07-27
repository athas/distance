-- | Various distance functions.

import "has_distance"

import "../vector/vector"

-- | A parametric module for construction an Euclidean measure.
module mk_euclidean (R: real) (V: vector)
                   : has_distance with t = V.vector R.t
                                  with distance = R.t = {
  type t = V.vector R.t
  type distance = R.t

  let distance (u: t) (v: t): distance =
    V.zip u v
    |> V.map (uncurry (R.-) >-> (R.((** i32 2))))
    |> V.reduce (R.+) (R.i32 0)
    |> R.sqrt
}

-- | A wrapper around `mk_euclidean`@term that represents a point as
-- an array.
module mk_euclidean' (R: real) = mk_euclidean R any_vector

-- | A parametric module for computing the Manhattan/cityblock distance.
module mk_manhattan (R: real) (V: vector)
                  : has_distance with t = V.vector R.t
                                 with distance = R.t = {
  type t = V.vector R.t
  type distance = R.t

  let distance (u: t) (v: t): distance =
    V.zip u v
    |> V.map (uncurry (R.-) >-> R.abs)
    |> V.reduce (R.+) (R.i32 0)
}

-- | A wrapper around `mk_manhattan`@term that represents a point as
-- an array.
module mk_manhattan' (R: real) = mk_manhattan R any_vector

-- | A parametric module for computing the Cosine distance.
module mk_cosine (R: real) (V: vector)
                : has_distance with t = V.vector R.t
                               with distance = R.t = {
  type t = V.vector R.t
  type distance = R.t

  let dotprod (u: t) (v: t) =
    V.zip u v
    |> V.map (uncurry (R.*))
    |> V.reduce (R.+) (R.i32 0)

  let norm (v: t) =
    v
    |> V.map (R.((** i32 2)))
    |> V.reduce (R.+) (R.i32 0)
    |> R.sqrt

  let distance (u: t) (v: t): distance =
    R.(i32 1 - (dotprod u v / (norm u * norm v)))
}

-- | A wrapper around `mk_cosine`@term that represents a point as
-- an array.
module mk_cosine' (R: real) = mk_cosine R any_vector
