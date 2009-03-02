module Particle where
import Data.SpacePart.AABB
import Data.SpacePart.QuadTree as Quad


data Particle = Dust Double Double | Wall Double Double deriving (Eq,Ord,Show)

radius_dust = 2.5 :: Double
radius_wall = 5.0 :: Double

radius (Dust _ _) = radius_dust
radius (Wall _ _) = radius_wall



particleBoundaryPointsGeneric dust_radius x y =
    [
     ((x - dust_radius), (y - dust_radius)),
     ((x - dust_radius), (y + dust_radius)),
     ((x + dust_radius), (y + dust_radius)),
     ((x + dust_radius), (y - dust_radius))]

particleBoundaryPoints (Dust x y) = particleBoundaryPointsGeneric radius_dust x y
particleBoundaryPoints (Wall x y) = particleBoundaryPointsGeneric radius_wall x y

particleBoundaryEdgesGeneric dust_radius x y =
    [
     (((x - dust_radius), (y - dust_radius)), ((x - dust_radius), (y + dust_radius))),
     (((x - dust_radius), (y + dust_radius)), ((x + dust_radius), (y + dust_radius))),
     (((x + dust_radius), (y + dust_radius)), ((x + dust_radius), (y - dust_radius))),
     (((x + dust_radius), (y - dust_radius)), ((x - dust_radius), (y - dust_radius)))]

particleBoundaryEdges (Dust x y) = particleBoundaryEdgesGeneric radius_dust x y
particleBoundaryEdges (Wall x y) = particleBoundaryEdgesGeneric radius_wall x y

particleBoundaryExtentsGeneric radius x y = (((x - radius),(y - radius)),((x+radius),(y+radius)))

particleBoundaryExtents (Dust x y) = particleBoundaryExtentsGeneric radius_dust x y
particleBoundaryExtents (Wall x y) = particleBoundaryExtentsGeneric radius_wall x y

particleBoundaryGeneric radius x y = Boundary { boundary_corner = (x - radius, y - radius),
                                                boundary_size = 2*radius }

particleBoundary (Dust x y) = particleBoundaryGeneric radius_dust  x y
particleBoundary (Wall x y) = particleBoundaryGeneric radius_wall x y


instance HasBoundary Particle where
    boundary_points =  particleBoundaryPoints
    boundary_edges =   particleBoundaryEdges
    boundary_extents = particleBoundaryExtents
    boundary_square = particleBoundary


quadtest =
    let tree = foldl (\o x -> Quad.insert x o) empty [(Wall (fromIntegral i) (fromIntegral i)) | i <- [1..100]] 
    in map (\x -> Quad.query (Boundary { boundary_corner = (x,x), boundary_size = 1 }) tree) [1..20]
