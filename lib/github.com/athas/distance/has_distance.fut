-- | Basic concepts for distance functions.

-- | A module that defines a distance function for points in some
-- space.  Usually you only care about the function (and the type on
-- which it operates), but a module type is necessary to conveniently
-- define parametric modules that define distance functions.
module type has_distance = {
  -- | The type of a point in the space.
  type t

  -- | The type in which distance is measured.  Usually a scalar of some sort.
  type distance

  -- | The distance function itself.
  val distance : t -> t -> distance
}
