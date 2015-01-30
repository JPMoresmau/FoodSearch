module AI.FoodSearch.BaseTest where
import AI.FoodSearch.Base


import Test.Tasty
import Test.Tasty.HUnit



baseTests :: TestTree
baseTests = do
  let sz = (5,5)
      sm = 3
      pos = (2,2)
      w = buildWorld sz pos sm
  testGroup "BaseTests" 
    [ testCase "Neighbours" $ do
        let sz1=(3,3)
        neighbours (1,1) sz1 @?= [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)]
        neighbours (0,0) sz1 @?= [(0,1),(1,0),(1,1)]
    , testCase "All Neighbours" $ do
        allNeighbours (1,1) @?= [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
        allNeighbours (0,0) @?= [(-1,-1),(-1,0),(-1,1),(0,-1),(0,0),(0,1),(1,-1),(1,0),(1,1)]
    , testCase "World" $ do
        currentSmell w pos @?= 3
        foundFood w pos @?= True
        currentSmell w (1,1) @?= 2
        foundFood w (1,1) @?= False
        currentSmell w (0,0) @?= 1
        foundFood w (0,0) @?= False
        currentSmell w (0,1) @?= 1
        foundFood w (0,1) @?= False
        currentSmell w (0,2) @?= 1
        foundFood w (0,2) @?= False
        currentSmell w (4,4) @?= 1
        foundFood w (4,4) @?= False
        currentSmell w (3,4) @?= 1
        foundFood w (3,4) @?= False
    , testCase "Base Algorithm Step" $ do
        algStep w baseAlg (0,0) @?= (1,1)
        algStep w baseAlg (1,1) @?= (2,2)
        algStep w baseAlg (2,2) @?= (2,2)
    , testCase "Base Algorithm Full" $ do
        algSteps w baseAlg 5 (0,0) @?= (2,2)
        algSteps w baseAlg 1 (0,0) @?= (1,1)
        algSteps w baseAlg 5 (4,4) @?= (2,2)
    ]
  
