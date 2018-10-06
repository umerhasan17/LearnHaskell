type Vertex = (Float, Float)

distance :: Vertex -> Vertex -> Float
-- takes vertex tuples and returns the distance between them
-- uses Pythagoras
distance (x1 , y1) (x2 , y2)
  = sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)