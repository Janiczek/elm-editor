module Tests.Common exposing (..)

import ArchitectureTest.Types exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)


app : TestedApp Model Msg
app =
    { model = ConstantModel initModel
    , update = NormalUpdate update
    , msgFuzzer = msgFuzzer
    }


msgFuzzer : Fuzzer Msg
msgFuzzer =
    Fuzz.oneOf
        [ noOp
        , moveUp
        , moveDown
        , moveLeft
        , moveRight
        , newLine
        , insertChar
        , removeCharBefore
        , removeCharAfter
        ]


noOp : Fuzzer Msg
noOp =
    Fuzz.constant NoOp


moveUp : Fuzzer Msg
moveUp =
    Fuzz.constant MoveUp


moveDown : Fuzzer Msg
moveDown =
    Fuzz.constant MoveDown


moveLeft : Fuzzer Msg
moveLeft =
    Fuzz.constant MoveLeft


moveRight : Fuzzer Msg
moveRight =
    Fuzz.constant MoveRight


newLine : Fuzzer Msg
newLine =
    Fuzz.constant NewLine


insertChar : Fuzzer Msg
insertChar =
    Fuzz.char |> Fuzz.map InsertChar


removeCharBefore : Fuzzer Msg
removeCharBefore =
    Fuzz.constant RemoveCharBefore


removeCharAfter : Fuzzer Msg
removeCharAfter =
    Fuzz.constant RemoveCharAfter
