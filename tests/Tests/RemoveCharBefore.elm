module Tests.RemoveCharBefore exposing (..)

import ArchitectureTest exposing (..)
import Array.Hamt as Array
import Expect exposing (Expectation)
import Main exposing (isStartOfDocument, lineContent, lineLength)
import Test exposing (..)
import Tests.Common exposing (..)


doesNothingOnStartOfDocument : Test
doesNothingOnStartOfDocument =
    msgTestWithPrecondition "RemoveCharBefore does nothing on start of document"
        app
        removeCharBefore
        (\model -> isStartOfDocument model.cursor)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel
                |> Expect.equal modelBeforeMsg


decreasesLineCountOnFirstColumnOfNotFirstLine : Test
decreasesLineCountOnFirstColumnOfNotFirstLine =
    msgTestWithPrecondition "RemoveCharBefore decreases line count on first column of not first line"
        app
        removeCharBefore
        (\model ->
            (model.cursor.column == 0)
                && (model.cursor.line /= 0)
        )
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            Array.length finalModel.lines
                |> Expect.equal (Array.length modelBeforeMsg.lines - 1)


combinesLinesTogetherOnFirstColumnOfNotFirstLine : Test
combinesLinesTogetherOnFirstColumnOfNotFirstLine =
    msgTestWithPrecondition "RemoveCharBefore combines lines together on first column of not first line"
        app
        removeCharBefore
        (\model ->
            (model.cursor.column == 0)
                && (model.cursor.line /= 0)
        )
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            let
                newLine =
                    lineContent finalModel.lines finalModel.cursor.line

                oldLine1 =
                    lineContent modelBeforeMsg.lines (modelBeforeMsg.cursor.line - 1)

                oldLine2 =
                    lineContent modelBeforeMsg.lines modelBeforeMsg.cursor.line
            in
            newLine
                |> Expect.equal (oldLine1 ++ oldLine2)


movesUpALineOnFirstColumnOfNotFirstLine : Test
movesUpALineOnFirstColumnOfNotFirstLine =
    msgTestWithPrecondition "RemoveCharBefore moves up a line on first column of not first line"
        app
        removeCharBefore
        (\model ->
            (model.cursor.column == 0)
                && (model.cursor.line /= 0)
        )
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.cursor.line
                |> Expect.equal (modelBeforeMsg.cursor.line - 1)


movesToPreviousEndOfPreviousLineOnFirstColumnOfNotFirstLine : Test
movesToPreviousEndOfPreviousLineOnFirstColumnOfNotFirstLine =
    msgTestWithPrecondition "RemoveCharBefore moves to previous end of previous line on first column of not first line"
        app
        removeCharBefore
        (\model ->
            (model.cursor.column == 0)
                && (model.cursor.line /= 0)
        )
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.cursor.column
                |> Expect.equal (lineLength modelBeforeMsg.lines (modelBeforeMsg.cursor.line - 1))


decreasesCurrentLineLengthOnNotFirstColumn : Test
decreasesCurrentLineLengthOnNotFirstColumn =
    msgTestWithPrecondition "RemoveCharBefore decreases current line length on not first column"
        app
        removeCharBefore
        (\model -> model.cursor.column /= 0)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            lineContent finalModel.lines finalModel.cursor.line
                |> String.length
                |> Expect.equal
                    ((lineContent modelBeforeMsg.lines modelBeforeMsg.cursor.line
                        |> String.length
                     )
                        - 1
                    )


movesLeftOnNotFirstColumn : Test
movesLeftOnNotFirstColumn =
    msgTestWithPrecondition "RemoveCharBefore moves left on not first column"
        app
        removeCharBefore
        (\model -> model.cursor.column /= 0)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.cursor.column
                |> Expect.equal (modelBeforeMsg.cursor.column - 1)


doesntMoveUpOrDownOnNotFirstColumn : Test
doesntMoveUpOrDownOnNotFirstColumn =
    msgTestWithPrecondition "RemoveCharBefore doesn't move up or down on not first column"
        app
        removeCharBefore
        (\model -> model.cursor.column /= 0)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            finalModel.cursor.line
                |> Expect.equal modelBeforeMsg.cursor.line


removesTheCharOnNotFirstColumn : Test
removesTheCharOnNotFirstColumn =
    msgTestWithPrecondition "RemoveCharBefore removes the char on not first column"
        app
        removeCharBefore
        (\model -> model.cursor.column /= 0)
    <|
        \_ _ modelBeforeMsg _ finalModel ->
            let
                oldLine =
                    lineContent modelBeforeMsg.lines modelBeforeMsg.cursor.line

                oldLinePart1 =
                    oldLine
                        |> String.left (modelBeforeMsg.cursor.column - 1)

                oldLinePart2 =
                    oldLine
                        |> String.dropLeft modelBeforeMsg.cursor.column

                expectedNewLine =
                    oldLinePart1 ++ oldLinePart2

                newLine =
                    lineContent finalModel.lines finalModel.cursor.line
            in
            newLine
                |> Expect.equal expectedNewLine
