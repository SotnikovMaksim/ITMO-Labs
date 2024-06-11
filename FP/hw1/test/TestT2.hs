module TestT2
  ( testNplus,
    testNmult,
    testNsub,
    testNcmp,
    testNFromNatural,
    testNToNum,
    testNEven,
    testNOdd,
    testNdiv,
    testNmod,
  )
where

import HW1.T2
import Test.HUnit.Base (Test (TestList), (~?=))

testNplus :: Test
testNplus =
  TestList
    [ nplus Z Z ~?= Z,
      nplus Z (S (S Z)) ~?= S (S Z),
      nplus (S (S Z)) Z ~?= S (S Z),
      nplus (S (S Z)) (S (S (S Z))) ~?= S (S (S (S (S Z))))
    ]

testNmult :: Test
testNmult =
  TestList
    [ nmult Z Z ~?= Z,
      nmult Z (S (S Z)) ~?= Z,
      nmult (S (S Z)) Z ~?= Z,
      nmult (S (S Z)) (S (S (S Z))) ~?= S (S (S (S (S (S Z)))))
    ]

testNsub :: Test
testNsub =
  TestList
    [ nsub (S (S Z)) Z ~?= Just (S (S Z)),
      nsub (S (S Z)) (S (S Z)) ~?= Just Z,
      nsub Z (S (S Z)) ~?= Nothing
    ]

testNcmp :: Test
testNcmp =
  TestList
    [ ncmp Z Z ~?= EQ,
      ncmp (S (S Z)) (S (S Z)) ~?= EQ,
      ncmp (S (S Z)) Z ~?= GT,
      ncmp Z (S (S Z)) ~?= LT
    ]

testNFromNatural :: Test
testNFromNatural =
  TestList
    [ nFromNatural 0 ~?= Z,
      nFromNatural 1 ~?= S Z,
      nFromNatural 2 ~?= S (S Z),
      nFromNatural 3 ~?= S (S (S Z))
    ]

testNToNum :: Test
testNToNum =
  TestList
    [ nToNum Z ~?= 0,
      nToNum (S Z) ~?= 1,
      nToNum (S (S Z)) ~?= 2,
      nToNum (S (S (S Z))) ~?= 3
    ]

testNEven :: Test
testNEven =
  TestList
    [ nEven Z ~?= True,
      nEven (S Z) ~?= False,
      nEven (S (S Z)) ~?= True,
      nEven (S (S (S Z))) ~?= False
    ]

testNOdd :: Test
testNOdd =
  TestList
    [ nOdd Z ~?= False,
      nOdd (S Z) ~?= True,
      nOdd (S (S Z)) ~?= False,
      nOdd (S (S (S Z))) ~?= True
    ]

testNdiv :: Test
testNdiv =
  TestList
    [ ndiv (S (S (S Z))) (S (S Z)) ~?= S Z,
      ndiv (S (S (S Z))) Z ~?= undefined
    ]

testNmod :: Test
testNmod =
  TestList
    [ nmod (S (S (S Z))) (S (S Z)) ~?= S Z,
      nmod (S (S (S (S Z)))) (S (S Z)) ~?= Z,
      nmod (S (S (S Z))) Z ~?= undefined -- Error expected. I only found deprecated 'assertError' from HUnit.Tools :(
    ]
