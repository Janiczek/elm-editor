module Tests.MoveDown exposing (..)

import ArchitectureTest exposing (..)
import Array.Hamt as Array
import Expect exposing (Expectation)
import Main exposing (endOfDocument, lastColumn, lastLine, lineLength)
import Test exposing (..)
import Tests.Common exposing (..)


jumpsToEndOfLineIfOnLastLine : Test
jumpsToEndOfLineIfOnLastLine =
    msgTestWithPrecondition "MoveDown jumps to end of line if on last line"
        app
        moveDown
        (\model -> model.position.line == lastLine model.lines)
    <|
        \_ _ _ _ finalModel ->
            finalModel.position
                |> Expect.equal (endOfDocument finalModel.lines)


movesDownALineIfNotOnLastLine : Test
movesDownALineIfNotOnLastLine =
    msgTestWithPrecondition "MoveDown moves down a line if not on last line"
        app
        moveDown
        (\model -> model.position.line /= lastLine model.lines)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.position.line
                |> Expect.equal (modelBeforeMsg.position.line + 1)


staysOnSameColumnIfNotOnLastLineAndEnoughCharsBelowCursor : Test
staysOnSameColumnIfNotOnLastLineAndEnoughCharsBelowCursor =
    msgTestWithPrecondition "MoveDown stays on same column if not on last line and enough chars below cursor"
        app
        moveDown
        (\model ->
            (model.position.line /= lastLine model.lines)
                && (lineLength model.lines (model.position.line + 1) >= model.position.column)
        )
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.position.column
                |> Expect.equal modelBeforeMsg.position.column


movesToLastColumnIfNotOnLastLineAndNoCharBelowCursor : Test
movesToLastColumnIfNotOnLastLineAndNoCharBelowCursor =
    msgTestWithPrecondition "MoveDown moves to last column if not on last line and no char below cursor"
        app
        moveDown
        (\model ->
            (model.position.line /= lastLine model.lines)
                && (lineLength model.lines (model.position.line + 1) < model.position.column)
        )
    <|
        \_ _ _ _ finalModel ->
            finalModel.position.column
                |> Expect.equal
                    (lastColumn
                        finalModel.lines
                        finalModel.position.line
                    )
