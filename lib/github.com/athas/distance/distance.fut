-- | Distance functions for numeric vectors.
--
-- Each distance function is exposed as a parametric modules that lets
-- you control both the scalar type and the dimensionality of the
-- space that the vectors inhabit.  This is concretely done by
-- specifying the representation of vectors as a module that
-- implements the `vector`@mtype@"../vector/vector" module type.
--
-- When using low-dimensional spaces (say, less than 10), use an
-- appropriately constructed statically sized vector module (see
-- `vector_1`@term@"../vector/vector" and
-- `cat_vector`@term@"../vector/vector").  For larger spaces, use
-- `any_vector`@term@"../vector/vector".

--  For convenience, the `vector` package is re-exported from this
-- file.
open import "../vector/vector"

open import "has_distance"

local module helpers (R: real) (V: vector) = {
  let dotprod u v =
    V.zip u v
    |> V.map (uncurry (R.*))
    |> V.reduce (R.+) (R.i32 0)

  let norm x =
    x
    |> V.map (R.((** i32 2)))
    |> V.reduce (R.+) (R.i32 0)
    |> R.sqrt
}

-- | Bray-Curtis distance.
module mk_braycurtis (R: real) (V: vector)
                   : has_distance with t = V.vector R.t
                                  with distance = R.t = {
  type t = V.vector R.t
  type distance = R.t

  let distance (u: t) (v: t): distance =
    let sum = V.reduce (R.+) (R.i32 0)
    in (V.zip u v |> V.map (uncurry (R.-) >-> R.abs) |> sum) R./
       (V.zip u v |> V.map (uncurry (R.+) >-> R.abs) |> V.map R.abs |> sum)
}

-- | Canberra distance.
module mk_canberra (R: real) (V: vector)
                   : has_distance with t = V.vector R.t
                                  with distance = R.t = {
  type t = V.vector R.t
  type distance = R.t

  let distance (u: t) (v: t): distance =
    let f (ui, vi) = R.(abs (ui - vi) / (abs ui + abs vi))
    in V.zip u v |> V.map f |> V.reduce (R.+) (R.i32 0)
}

-- | Chebyshev distance.
module mk_chebyshev (R: real) (V: vector)
                   : has_distance with t = V.vector R.t
                                  with distance = R.t = {
  type t = V.vector R.t
  type distance = R.t

  let distance (u: t) (v: t): distance =
    V.zip u v |> V.map (uncurry (R.-) >-> R.abs) |> V.reduce (R.max) R.lowest
}

-- | Correlation distance.
module mk_correlation (R: real) (V: vector)
                   : has_distance with t = V.vector R.t
                                  with distance = R.t = {
  type t = V.vector R.t
  type distance = R.t

  open (helpers R V)

  let distance (u: t) (v: t): distance =
    let mean x = R.(V.reduce (+) (i32 0) x / i32 V.length)
    let u' = V.map (R.- mean u) u
    let v' = V.map (R.- mean v) v
    in R.(i32 1 - (u' `dotprod` v') / (norm u' * norm v'))
}

-- | Cosine distance.
module mk_cosine (R: real) (V: vector)
                : has_distance with t = V.vector R.t
                               with distance = R.t = {
  type t = V.vector R.t
  type distance = R.t

  open (helpers R V)

  let distance (u: t) (v: t): distance =
    R.(i32 1 - (dotprod u v / (norm u * norm v)))
}

-- | Euclidean distance.
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

-- | Manhattan/cityblock distance.
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

-- | Order-2 Minkowski distance.
module mk_minkowski (R: real) (V: vector)
                    : has_distance with t = V.vector R.t
                                   with distance = R.t = {
  type t = V.vector R.t
  type distance = R.t

  open (helpers R V)

  let distance (u: t) (v: t): distance =
    V.zip u v
    |> V.map (uncurry (R.-))
    |> norm
}

-- | Squared euclidean distance.
module mk_sqeuclidean (R: real) (V: vector)
                   : has_distance with t = V.vector R.t
                                  with distance = R.t = {
  type t = V.vector R.t
  type distance = R.t

  open (helpers R V)

  let distance (u: t) (v: t): distance =
    V.zip u v
    |> V.map (uncurry (R.-))
    |> V.map (R.((** i32 2)))
    |> V.reduce (R.+) (R.i32 0)
}
