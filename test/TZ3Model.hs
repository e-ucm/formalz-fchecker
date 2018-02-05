module TZ3Model where
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Silently (silence)
import Test.HUnit

import LogicIR.Backend.Z3.Model

(~~) model ast = runP modelP model @?= ast

modelTests =
  [ "a?length -> 2 \n\
  \ i!0 -> 1 \n\
  \ a -> (_ as-array k!1) \n\
  \ k!1 -> { \n\
  \   1 -> (- 2) \n\
  \   else -> (- 2) \n\
  \ }" ~~
    [ ("a?length", IntVal 2)
    , ("i!0", IntVal 1)
    , ("a", ArrayRef "k!1")
    , ("k!1", ArrayFunc [ InstVal 1 $ IntVal (-2)
                        , InstElse $ IntVal (-2)
                        ])
    ]
  , "a?length -> 3 \n\
  \ i!0 -> 1 \n\
  \ a -> (_ as-array k!2) \n\
  \ j!1 -> 2 \n\
  \ k!2 -> { \n\
  \   2 -> 1.0 \n\
  \   else -> 1.0 \n\
  \ }" ~~
    [ ("a?length", IntVal 3)
    , ("i!0", IntVal 1)
    , ("a", ArrayRef "k!2")
    , ("j!1", IntVal 2)
    , ("k!2", ArrayFunc [ InstVal 2 $ RealVal 1.0
                        , InstElse $ RealVal 1.0
                        ])
    ]
  , "a?length -> 3 \n\
  \ i!0 -> 1 \n\
  \ a -> (_ as-array k!2) \n\
  \ j!1 -> 2 \n\
  \ k!2 -> { \n\
  \   2 -> (- 2.0) \n\
  \   else -> (- 2.0) \n\
  \ }" ~~
    [ ("a?length", IntVal 3)
    , ("i!0", IntVal 1)
    , ("a", ArrayRef "k!2")
    , ("j!1", IntVal 2)
    , ("k!2", ArrayFunc [ InstVal 2 $ RealVal (-2.0)
                        , InstElse $ RealVal (-2.0)
                        ])
    ]
  ]
