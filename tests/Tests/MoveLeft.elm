module Tests.MoveLeft exposing (..)

import ArchitectureTest exposing (..)
import Array.Hamt as Array
import Expect exposing (Expectation)
import Main exposing (isStartOfDocument, lineLength)
import Test exposing (..)
import Tests.Common exposing (..)


doesNothingOnStartOfDocument : Test
doesNothingOnStartOfDocument =
    msgTestWithPrecondition "MoveLeft does nothing on start of document"
        app
        moveLeft
        (\model -> isStartOfDocument model.position)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel
                |> Expect.equal modelBeforeMsg


movesLeftIfNotOnFirstColumn : Test
movesLeftIfNotOnFirstColumn =
    msgTestWithPrecondition "MoveLeft moves left if not on first column"
        app
        moveLeft
        (\model -> model.position.column /= 0)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.position.column
                |> Expect.equal (modelBeforeMsg.position.column - 1)


movesToEndOfPreviousLineIfOnTheFirstColumnAndNotOnFirstLine : Test
movesToEndOfPreviousLineIfOnTheFirstColumnAndNotOnFirstLine =
    msgTestWithPrecondition "MoveLeft moves to end of previous line if on the first column and not on first line"
        app
        moveLeft
        (\model -> model.position.column == 0 && model.position.line /= 0)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            Expect.all
                [ .line >> Expect.equal (modelBeforeMsg.position.line - 1)
                , .column
                    >> Expect.equal
                        (lineLength
                            modelBeforeMsg.lines
                            (modelBeforeMsg.position.line - 1)
                        )
                ]
                finalModel.position
