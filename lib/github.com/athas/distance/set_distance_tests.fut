-- | ignore

import "set_distance"

module vector_3 = cat_vector vector_1 (cat_vector vector_1 vector_1)
let vec3 't (xs: []t) = vector_3.from_array (xs :> [vector_3.length]t)

-- ==
-- entry: test_dice
-- input { true false false false true false }
-- output { 1f32 }
module dice = mk_dice f32 vector_3
entry test_dice x0 y0 z0 x1 y1 z1 =
  dice.distance (vec3 [x0, y0, z0])
                (vec3 [x1, y1, z1])

-- ==
-- entry: test_hamming
-- input { true false false false true false }
-- output { 0.6666666666666666f32 }
module hamming = mk_hamming f32 vector_3
entry test_hamming x0 y0 z0 x1 y1 z1 =
  hamming.distance (vec3 [x0, y0, z0])
                   (vec3 [x1, y1, z1])

-- ==
-- entry: test_jaccard
-- input { true false false false true false }
-- output { 1f32 }
module jaccard = mk_jaccard f32 vector_3
entry test_jaccard x0 y0 z0 x1 y1 z1 =
  jaccard.distance (vec3 [x0, y0, z0])
                   (vec3 [x1, y1, z1])

-- ==
-- entry: test_kulsinski
-- input { false true false true true true }
-- output { 0.8f32 }
module kulsinski = mk_kulsinski f32 vector_3
entry test_kulsinski x0 y0 z0 x1 y1 z1 =
  kulsinski.distance (vec3 [x0, y0, z0])
                     (vec3 [x1, y1, z1])

-- ==
-- entry: test_rogerstanimoto
-- input { true false true true true true }
-- output { 0.5f32 }
module rogerstanimoto = mk_rogerstanimoto f32 vector_3
entry test_rogerstanimoto x0 y0 z0 x1 y1 z1 =
  rogerstanimoto.distance (vec3 [x0, y0, z0])
                          (vec3 [x1, y1, z1])

-- ==
-- entry: test_russellrao
-- input { true false true true true true }
-- output { 0.3333333333333333f32 }
module russellrao = mk_russellrao f32 vector_3
entry test_russellrao x0 y0 z0 x1 y1 z1 =
  russellrao.distance (vec3 [x0, y0, z0])
                      (vec3 [x1, y1, z1])

-- ==
-- entry: test_sokalsneath
-- input { true false true true true true }
-- output { 0.5f32 }
module sokalsneath = mk_sokalsneath f32 vector_3
entry test_sokalsneath x0 y0 z0 x1 y1 z1 =
  sokalsneath.distance (vec3 [x0, y0, z0])
                       (vec3 [x1, y1, z1])

-- ==
-- entry: test_yule
-- input { true false false false true false }
-- output { 2f32 }
module yule = mk_yule f32 vector_3
entry test_yule x0 y0 z0 x1 y1 z1 =
  yule.distance (vec3 [x0, y0, z0])
                (vec3 [x1, y1, z1])
