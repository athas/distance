import "distance"
import "../vector/vector"

module vector_2 = cat_vector vector_1 vector_1

-- ==
-- entry: test_euclidean
-- input { 0f32 0f32 1f32 1f32 }
-- output { 1.4142135623730951f32 }
module euclidean = mk_euclidean f32 vector_2
entry test_euclidean x0 y0 x1 y1 =
  euclidean.distance (vector_2.from_array [x0, y0])
                     (vector_2.from_array [x1, y1])

-- ==
-- entry: test_manhattan
-- input { 0f32 0f32 1f32 1f32 }
-- output { 2f32 }
module manhattan = mk_manhattan f32 vector_2
entry test_manhattan x0 y0 x1 y1 =
  manhattan.distance (vector_2.from_array [x0, y0])
                     (vector_2.from_array [x1, y1])

-- ==
-- entry: test_cosine
-- input { 1f32 1f32 1f32 0f32 }
-- output { 0.29289321881345254f32 }
module cosine = mk_cosine f32 vector_2
entry test_cosine x0 y0 x1 y1 =
  cosine.distance (vector_2.from_array [x0, y0])
                  (vector_2.from_array [x1, y1])
