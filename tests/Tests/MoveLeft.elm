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
        (\model -> isStartOfDocument model.cursor)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel
                |> Expect.equal modelBeforeMsg


movesLeftIfNotOnFirstColumn : Test
movesLeftIfNotOnFirstColumn =
    msgTestWithPrecondition "MoveLeft moves left if not on first column"
        app
        moveLeft
        (\model -> model.cursor.column /= 0)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.cursor.column
                |> Expect.equal (modelBeforeMsg.cursor.column - 1)


movesToEndOfPreviousLineIfOnTheFirstColumnAndNotOnFirstLine : Test
movesToEndOfPreviousLineIfOnTheFirstColumnAndNotOnFirstLine =
    msgTestWithPrecondition "MoveLeft moves to end of previous line if on the first column and not on first line"
        app
        moveLeft
        (\model -> model.cursor.column == 0 && model.cursor.line /= 0)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            Expect.all
                [ .line >> Expect.equal (modelBeforeMsg.cursor.line - 1)
                , .column
                    >> Expect.equal
                        (lineLength
                            modelBeforeMsg.lines
                            (modelBeforeMsg.cursor.line - 1)
                        )
                ]
                finalModel.cursor
