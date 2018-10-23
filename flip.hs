-- let f x y z = (x,y,z)

f1 = id
f2 = flip
f3 = (flip .)
f4 = (flip .). flip
f5 = flip.(flip.)
f6 = (flip.).flip.(flip.)