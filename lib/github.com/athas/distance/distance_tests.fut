-- | ignore

import "distance"

module vector_2 = cat_vector vector_1 vector_1
let vec2 't (xs: []t) = vector_2.from_array (xs :> [vector_2.length]t)

-- ==
-- entry: test_braycurtis
-- input { 1f32 2f32 3f32 4f32 }
-- output { 0.4f32 }
module braycurtis = mk_braycurtis f32 vector_2
entry test_braycurtis x0 y0 x1 y1 =
  braycurtis.distance (vec2 [x0, y0])
                      (vec2 [x1, y1])

-- ==
-- entry: test_canberra
-- input { 1f32 2f32 3f32 4f32 }
-- output { 0.8333333333333333f32 }
module canberra = mk_canberra f32 vector_2
entry test_canberra x0 y0 x1 y1 =
  canberra.distance (vec2 [x0, y0])
                    (vec2 [x1, y1])

-- ==
-- entry: test_chebyshev
-- input { 1f32 2f32 3f32 4f32 }
-- output { 2f32 }
module chebyshev = mk_chebyshev f32 vector_2
entry test_chebyshev x0 y0 x1 y1 =
  chebyshev.distance (vec2 [x0, y0])
                     (vec2 [x1, y1])

-- ==
-- entry: test_correlation
-- input { 0f32 0.1f32 10f32 1f32 }
-- output { 2f32 }
module correlation = mk_correlation f32 vector_2
entry test_correlation x0 y0 x1 y1 =
  correlation.distance (vec2 [x0, y0])
                       (vec2 [x1, y1])

-- ==
-- entry: test_cosine
-- input { 1f32 1f32 1f32 0f32 }
-- output { 0.29289321881345254f32 }
module cosine = mk_cosine f32 vector_2
entry test_cosine x0 y0 x1 y1 =
  cosine.distance (vec2 [x0, y0])
                  (vec2 [x1, y1])

-- ==
-- entry: test_euclidean
-- input { 0f32 0f32 1f32 1f32 }
-- output { 1.4142135623730951f32 }
module euclidean = mk_euclidean f32 vector_2
entry test_euclidean x0 y0 x1 y1 =
  euclidean.distance (vec2 [x0, y0])
                     (vec2 [x1, y1])

-- ==
-- entry: test_manhattan
-- input { 0f32 0f32 1f32 1f32 }
-- output { 2f32 }
module manhattan = mk_manhattan f32 vector_2
entry test_manhattan x0 y0 x1 y1 =
  manhattan.distance (vec2 [x0, y0])
                     (vec2 [x1, y1])

-- ==
-- entry: test_minkowski
-- input { 0f32 1f32 2f32 3f32 }
-- output { 2.8284271247461903f32 }
module minkowski = mk_minkowski f32 vector_2
entry test_minkowski x0 y0 x1 y1 =
  minkowski.distance (vec2 [x0, y0])
                     (vec2 [x1, y1])

-- ==
-- entry: test_sqeuclidean
-- input { 0f32 1f32 2f32 3f32 }
-- output { 8f32 }
module sqeuclidean = mk_sqeuclidean f32 vector_2
entry test_sqeuclidean x0 y0 x1 y1 =
  sqeuclidean.distance (vec2 [x0, y0])
                       (vec2 [x1, y1])
