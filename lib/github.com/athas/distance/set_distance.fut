-- | Distance functions for boolean vectors (representing sets).

--  For convenience, the `vector` package is re-exported from this
-- file.
open import "../vector/vector"

open import "has_distance"

-- | Dice dissimilarity.
module mk_dice (R: real) (V: vector):
       has_distance with t = V.vector bool
                    with distance = R.t = {
  type t = V.vector bool
  type distance = R.t

  let distance u v =
    let tf (x, y) = R.bool (x && !y)
    let ft (x, y) = R.bool (!x && y)
    let tt (x, y) = R.bool (x && y)
    let c_tf = V.zip u v |> V.map tf |> V.reduce (R.+) (R.i32 0)
    let c_ft = V.zip u v |> V.map ft |> V.reduce (R.+) (R.i32 0)
    let c_tt = V.zip u v |> V.map tt |> V.reduce (R.+) (R.i32 0)
    in R.((c_tf + c_ft) / (i32 2 * c_tt + c_ft + c_tf))
}

-- | Hamming distance.
module mk_hamming (R: real) (V: vector):
       has_distance with t = V.vector bool
                    with distance = R.t = {
  type t = V.vector bool
  type distance = R.t

  let distance (u: t) (v: t) =
    V.zip u v
    |> V.map (uncurry (!=) >-> R.bool)
    |> V.reduce (R.+) (R.i32 0)
    |> R.((/ i32 V.length))
}

-- | Jaccard dissimilarity.
module mk_jaccard (R: real) (V: vector):
       has_distance with t = V.vector bool
                    with distance = R.t = {
  type t = V.vector bool
  type distance = R.t

  let distance u v =
    let tf (x, y) = R.bool (x && !y)
    let ft (x, y) = R.bool (!x && y)
    let tt (x, y) = R.bool (x && y)
    let c_tf = V.zip u v |> V.map tf |> V.reduce (R.+) (R.i32 0)
    let c_ft = V.zip u v |> V.map ft |> V.reduce (R.+) (R.i32 0)
    let c_tt = V.zip u v |> V.map tt |> V.reduce (R.+) (R.i32 0)
    in R.((c_tf + c_ft) / (c_tt + c_ft + c_tf))
}

-- | Kulsinski dissimilarity.
module mk_kulsinski (R: real) (V: vector):
       has_distance with t = V.vector bool
                    with distance = R.t = {
  type t = V.vector bool
  type distance = R.t

  let distance u v =
    let tf (x, y) = R.bool (x && !y)
    let ft (x, y) = R.bool (!x && y)
    let tt (x, y) = R.bool (x && y)
    let c_tf = V.zip u v |> V.map tf |> V.reduce (R.+) (R.i32 0)
    let c_ft = V.zip u v |> V.map ft |> V.reduce (R.+) (R.i32 0)
    let c_tt = V.zip u v |> V.map tt |> V.reduce (R.+) (R.i32 0)
    in R.((c_tf + c_ft - c_tt + i32 V.length) / (c_ft + c_tf + i32 V.length))
}

-- | Rogers-Tanimoto dissimilarity.
module mk_rogerstanimoto (R: real) (V: vector):
       has_distance with t = V.vector bool
                    with distance = R.t = {
  type t = V.vector bool
  type distance = R.t

  let distance u v =
    let ff (x, y) = R.bool (!x && !y)
    let tt (x, y) = R.bool (x && y)
    let ft (x, y) = R.bool (!x && y)
    let tf (x, y) = R.bool (x && !y)
    let c_tf = V.zip u v |> V.map tf |> V.reduce (R.+) (R.i32 0)
    let c_ft = V.zip u v |> V.map ft |> V.reduce (R.+) (R.i32 0)
    let c_ff = V.zip u v |> V.map ff |> V.reduce (R.+) (R.i32 0)
    let c_tt = V.zip u v |> V.map tt |> V.reduce (R.+) (R.i32 0)
    let r = R.(i32 2 * (c_tf + c_ft))
    in R.(r / (c_tt + c_ff + r))
}

-- | Russell-Rao dissimilarity.
module mk_russellrao (R: real) (V: vector):
       has_distance with t = V.vector bool
                    with distance = R.t = {
  type t = V.vector bool
  type distance = R.t

  let distance u v =
    let c_tt = V.zip u v
               |> V.map (uncurry (&&) >-> R.bool)
               |> V.reduce (R.+) (R.i32 0)
    let n = R.i32 V.length
    in R.((n - c_tt) / n)
}

-- | Sokal-Michener dissimilarity.
module mk_sokalmichener (R: real) (V: vector):
       has_distance with t = V.vector bool
                    with distance = R.t = {
  type t = V.vector bool
  type distance = R.t

  let distance u v =
    let ff (x, y) = R.bool (!x && !y)
    let tt (x, y) = R.bool (x && y)
    let ft (x, y) = R.bool (!x && y)
    let tf (x, y) = R.bool (x && !y)
    let c_tf = V.zip u v |> V.map tf |> V.reduce (R.+) (R.i32 0)
    let c_ft = V.zip u v |> V.map ft |> V.reduce (R.+) (R.i32 0)
    let c_ff = V.zip u v |> V.map ff |> V.reduce (R.+) (R.i32 0)
    let c_tt = V.zip u v |> V.map tt |> V.reduce (R.+) (R.i32 0)
    let r = R.(i32 2 * (c_tf + c_ft))
    let s = R.(c_ff + c_tt)
    in R.(r / (s + r))
}

-- | Sokal-Sneath dissimilarity.
module mk_sokalsneath (R: real) (V: vector):
       has_distance with t = V.vector bool
                    with distance = R.t = {
  type t = V.vector bool
  type distance = R.t

  let distance u v =
    let tt (x, y) = R.bool (x && y)
    let ft (x, y) = R.bool (!x && y)
    let tf (x, y) = R.bool (x && !y)
    let c_tf = V.zip u v |> V.map tf |> V.reduce (R.+) (R.i32 0)
    let c_ft = V.zip u v |> V.map ft |> V.reduce (R.+) (R.i32 0)
    let c_tt = V.zip u v |> V.map tt |> V.reduce (R.+) (R.i32 0)
    let r = R.(i32 2 * (c_tf + c_ft))
    in R.(r / (c_tt + r))
}


-- | Yule dissimilarity.
module mk_yule (R: real) (V: vector):
       has_distance with t = V.vector bool
                    with distance = R.t = {
  type t = V.vector bool
  type distance = R.t

  let distance u v =
    let ff (x, y) = R.bool (!x && !y)
    let tt (x, y) = R.bool (x && y)
    let ft (x, y) = R.bool (!x && y)
    let tf (x, y) = R.bool (x && !y)
    let c_tf = V.zip u v |> V.map tf |> V.reduce (R.+) (R.i32 0)
    let c_ft = V.zip u v |> V.map ft |> V.reduce (R.+) (R.i32 0)
    let c_ff = V.zip u v |> V.map ff |> V.reduce (R.+) (R.i32 0)
    let c_tt = V.zip u v |> V.map tt |> V.reduce (R.+) (R.i32 0)
    let r = R.(i32 2 * (c_tf + c_ft))
    in R.(r / (c_tt * c_ff + (r/i32 2)))
}
