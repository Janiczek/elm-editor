module Tests.MoveRight exposing (..)

import ArchitectureTest exposing (..)
import Array.Hamt as Array
import Expect exposing (Expectation)
import Main exposing (isEndOfDocument, lastColumn, lastLine)
import Test exposing (..)
import Tests.Common exposing (..)


doesNothingOnEndOfDocument : Test
doesNothingOnEndOfDocument =
    msgTestWithPrecondition "MoveRight does nothing on end of document"
        app
        moveRight
        (\model -> isEndOfDocument model.lines model.position)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel
                |> Expect.equal modelBeforeMsg


movesRightIfNotOnLastColumn : Test
movesRightIfNotOnLastColumn =
    msgTestWithPrecondition "MoveRight moves right if not on last column"
        app
        moveRight
        (\model -> model.position.column /= lastColumn model.lines model.position.line)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.position.column
                |> Expect.equal (modelBeforeMsg.position.column + 1)


movesToStartOfNextLineIfOnTheLastColumnAndNotOnLastLine : Test
movesToStartOfNextLineIfOnTheLastColumnAndNotOnLastLine =
    msgTestWithPrecondition "MoveRight moves to start of next line if on the last column and not on last line"
        app
        moveRight
        (\model ->
            (model.position.column == lastColumn model.lines model.position.line)
                && (model.position.line /= lastLine model.lines)
        )
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            Expect.all
                [ .line >> Expect.equal (modelBeforeMsg.position.line + 1)
                , .column >> Expect.equal 0
                ]
                finalModel.position
