module Tests.Invariants exposing (..)

import ArchitectureTest exposing (..)
import Array.Hamt as Array
import Expect exposing (Expectation)
import Main exposing (..)
import Test exposing (..)
import Tests.Common exposing (..)


cursorLineIsAlwaysPositive : Test
cursorLineIsAlwaysPositive =
    invariantTest "cursor.line is always positive" app <|
        \_ _ finalModel ->
            finalModel.cursor.line
                |> Expect.atLeast 0


cursorLineNeverGetsToNonexistingLine : Test
cursorLineNeverGetsToNonexistingLine =
    invariantTest "cursor.line never gets to nonexisting line" app <|
        \_ _ finalModel ->
            finalModel.cursor.line
                |> Expect.atMost (lastLine finalModel.lines)


cursorColumnIsAlwaysPositive : Test
cursorColumnIsAlwaysPositive =
    invariantTest "cursor.column is always positive" app <|
        \_ _ finalModel ->
            finalModel.cursor.column
                |> Expect.atLeast 0


cursorColumnNeverGetsMoreThanOneCharAfterLineContents : Test
cursorColumnNeverGetsMoreThanOneCharAfterLineContents =
    invariantTest "cursor.column never gets more than one char after line contents" app <|
        \_ _ finalModel ->
            finalModel.cursor.column
                |> Expect.atMost
                    (lastColumn
                        finalModel.lines
                        finalModel.cursor.line
                    )


linesArrayNeverEmpty : Test
linesArrayNeverEmpty =
    invariantTest "lines array never empty" app <|
        \_ _ finalModel ->
            finalModel.lines
                |> Array.length
                |> Expect.atLeast 1


hoverAlwaysWithinBounds : Test
hoverAlwaysWithinBounds =
    invariantTest "hover always within bounds" app <|
        \_ _ finalModel ->
            case finalModel.hover of
                NoHover ->
                    Expect.pass

                HoverLine line ->
                    Expect.all
                        [ Expect.atLeast 0
                        , Expect.atMost (lastLine finalModel.lines)
                        ]
                        line

                HoverChar position ->
                    Expect.all
                        [ .line >> Expect.atLeast 0
                        , .line >> Expect.atMost (lastLine finalModel.lines)
                        , .column >> Expect.atLeast 0
                        , \{ line, column } -> column |> Expect.atMost (lastColumn finalModel.lines line)
                        ]
                        position
