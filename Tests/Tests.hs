module Tests where
    import IC.TestSuite
    import KMeans
    
    
    -- lookUp
    lookUpTestCases
      = [ (2,fig1aAssignment) ==> [(2,9),(4,9),(3,7)]
        ]
    
    -- distance
    distanceTestCases
      = [ ((1,1),(1,1)) ==> 0,
          ((1,1),(1,2)) ==> 1,
          ((2,1),(1,2)) ==> 2
        ]
    
    -- sumPoints
    sumPointsTestCases
      = [ ((1,2),(4,5)) ==> (5,7)
        ]
    
    -- centroid
    centroidTestCases
      = [ [(1,2),(3,4)] ==> (2,3),
          [(1,2),(3,4),(5,6)] ==> (3,4)
        ]
    
    -- assign
    assignTestCases
      = [ ((5,7),[(1,2),(5,4),(3,1)]) ==> ((5,7),2),
          ((1,7),[(1,2),(5,4),(3,1)]) ==> ((1,7),1)
        ]
    
    -- assignAll
    assignAllTestCases
      = [ ([(5,7),(1,7)],[(1,2),(5,4),(3,1)]) ==> [((5,7),2),((1,7),1)],
          ([(5,7),(1,7)],[(6,2),(5,4),(3,1)]) ==> [((5,7),2),((1,7),2)]
        ]
    
    -- adjustCentroids
    adjustCentroidsTestCases
      = [ ([((5,7),2),((1,7),1),((3,4),2),((8,5),2)],2) ==> [(1,7),(5,5)],
          ([((5,7),2),((1,7),1),((3,4),1),((8,5),2)],2) ==> [(2,5),(6,6)],
          (fig1aAssignment,3) ==> [(4,1),(3,8),(6,5)],
          (fig1bAssignment,3) ==> [(4,1),(3,7),(6,5)]
        ]
    
    -- cluster
    clusterTestCases
      = [ ([((11,4),1),((4,7),2),((6,8),2),((3,6),2),((14,12),1)],[(11,4),(4,7)],2) ==> ([((11,4),1),((4,7),2),((6,8),2),((3,6),2),((14,12),1)],[(12,8),(4,7)]),
          ([((7,6),1),((3,8),2),((3,4),2),((1,6),2),((5,7),1),((10,6),1),((14,11),1),((4,15),2)],[(7,6),(3,8)],2) ==> ([((7,6),1),((3,8),2),((3,4),2),((1,6),2),((5,7),1),((10,6),1),((14,11),1),((4,15),2)],[(9,7),(2,8)])
        ]
    
    -- kmeans
    kmeansTestCases
      = [ ([(11,4),(4,7),(6,8),(3,6),(14,12)],2) ==> ([((11,4),1),((4,7),2),((6,8),2),((3,6),2),((14,12),1)],[(12,8),(4,7)]),
          ([(7,6),(3,8),(3,4),(1,6),(5,7),(10,6),(14,11),(4,15)],2) ==> ([((7,6),1),((3,8),2),((3,4),2),((1,6),2),((5,7),1),((10,6),1),((14,11),1),((4,15),2)],[(9,7),(2,8)]),
          (points,1) ==> ([((3,1),1),((2,9),1),((5,6),1),((7,8),1),((2,3),1),((6,7),1),((3,5),1),((4,9),1),((9,3),1),((1,1),1),((2,1),1),((9,1),1),((7,2),1),((3,7),1)],[(4,4)]),
          (points,2) ==> ([((3,1),1),((2,9),2),((5,6),2),((7,8),2),((2,3),1),((6,7),2),((3,5),2),((4,9),2),((9,3),1),((1,1),1),((2,1),1),((9,1),1),((7,2),1),((3,7),2)],[(4,1),(4,7)]),
          (points,3) ==> result,
          (points2,2) ==> result2,
          (points3,4) ==> result3
        ]
    
    allTestCases
      = [ TestCase  "lookUp"           (uncurry lookUp)
                                       lookUpTestCases
    
        , TestCase  "distance"         (uncurry distance)
                                       distanceTestCases
    
        , TestCase  "sumPoints"        (uncurry sumPoints)
                                       sumPointsTestCases
    
        , TestCase  "centroid"         (centroid)
                                       centroidTestCases
    
        , TestCase  "assign"           (uncurry assign)
                                       assignTestCases
    
        , TestCase  "assignAll"        (uncurry assignAll)
                                       assignAllTestCases
    
        , TestCase  "adjustCentroids"  (uncurry adjustCentroids)
                                       adjustCentroidsTestCases
    
        , TestCase  "cluster"          (uncurry3 cluster)
                                       clusterTestCases
    
        , TestCase  "kmeans"           (uncurry kmeans)
                                       kmeansTestCases
        ]
    
    runTests = mapM_ goTest allTestCases
    
    main = runTests