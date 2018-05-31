module Tests.NoOp exposing (..)

import ArchitectureTest exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)
import Tests.Common exposing (..)


doesNothing : Test
doesNothing =
    msgTest "NoOp does nothing" app noOp <|
        \_ _ modelBeforeMsg _ finalModel ->
            modelBeforeMsg
                |> Expect.equal finalModel
