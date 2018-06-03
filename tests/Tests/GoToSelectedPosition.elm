module Tests.GoToSelectedPosition exposing (..)

import ArchitectureTest exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)
import Tests.Common exposing (..)


doesNothingIfNoHover : Test
doesNothingIfNoHover =
    msgTestWithPrecondition "GoToHoveredPosition sets hover within bounds" app hover <|
        \_ _ _ msg finalModel ->
            modelBeforeMsg
                |> Expect.equal finalModel
