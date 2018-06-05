module CodeEditor exposing (..)

import Test exposing (..)
import Tests.GoToHoveredPosition as GoToHoveredPosition
import Tests.Hover as Hover
import Tests.InsertChar as InsertChar
import Tests.Invariants as Invariants
import Tests.MoveDown as MoveDown
import Tests.MoveLeft as MoveLeft
import Tests.MoveRight as MoveRight
import Tests.MoveUp as MoveUp
import Tests.NewLine as NewLine
import Tests.NoOp as NoOp
import Tests.RemoveCharAfter as RemoveCharAfter
import Tests.RemoveCharBefore as RemoveCharBefore


suite : Test
suite =
    concat
        [ describe "Invariants"
            [ describe "cursor"
                [ describe "line"
                    [ Invariants.cursorLineIsAlwaysPositive
                    , Invariants.cursorLineNeverGetsToNonexistingLine
                    ]
                , describe "column"
                    [ Invariants.cursorColumnIsAlwaysPositive
                    , Invariants.cursorColumnNeverGetsMoreThanOneCharAfterLineContents
                    ]
                ]
            , describe "lines"
                [ Invariants.linesArrayNeverEmpty
                ]
            , describe "hover"
                [ Invariants.hoverAlwaysWithinBounds
                ]
            , todo "selection"
            ]
        , describe "Msgs"
            [ describe "NoOp"
                [ NoOp.doesNothing
                ]
            , describe "MoveUp"
                [ MoveUp.jumpsToStartOfLineIfOnFirstLine
                , MoveUp.movesUpALineIfNotOnFirstLine
                , MoveUp.staysOnSameColumnIfNotOnFirstLineAndEnoughCharsAboveCursor
                , MoveUp.movesToLastColumnIfNotOnFirstLineAndNoCharAboveCursor
                ]
            , describe "MoveDown"
                [ MoveDown.jumpsToEndOfLineIfOnLastLine
                , MoveDown.movesDownALineIfNotOnLastLine
                , MoveDown.staysOnSameColumnIfNotOnLastLineAndEnoughCharsBelowCursor
                , MoveDown.movesToLastColumnIfNotOnLastLineAndNoCharBelowCursor
                ]
            , describe "MoveLeft"
                [ MoveLeft.doesNothingOnStartOfDocument
                , MoveLeft.movesLeftIfNotOnFirstColumn
                , MoveLeft.movesToEndOfPreviousLineIfOnTheFirstColumnAndNotOnFirstLine
                ]
            , describe "MoveRight"
                [ MoveRight.doesNothingOnEndOfDocument
                , MoveRight.movesRightIfNotOnLastColumn
                , MoveRight.movesToStartOfNextLineIfOnTheLastColumnAndNotOnLastLine
                ]
            , describe "NewLine"
                [ NewLine.movesDownALine
                , NewLine.movesToFirstColumn
                , NewLine.addsALine
                , NewLine.splitsALineIntoTwo
                ]
            , describe "InsertChar"
                [ InsertChar.movesRight
                , InsertChar.doesntMoveUpOrDown
                , InsertChar.makesLineLonger
                , InsertChar.insertsChar
                ]
            , describe "RemoveCharBefore"
                [ RemoveCharBefore.doesNothingOnStartOfDocument
                , RemoveCharBefore.decreasesLineCountOnFirstColumnOfNotFirstLine
                , RemoveCharBefore.combinesLinesTogetherOnFirstColumnOfNotFirstLine
                , RemoveCharBefore.movesUpALineOnFirstColumnOfNotFirstLine
                , RemoveCharBefore.movesToPreviousEndOfPreviousLineOnFirstColumnOfNotFirstLine
                , RemoveCharBefore.decreasesCurrentLineLengthOnNotFirstColumn
                , RemoveCharBefore.movesLeftOnNotFirstColumn
                , RemoveCharBefore.doesntMoveUpOrDownOnNotFirstColumn
                , RemoveCharBefore.removesTheCharOnNotFirstColumn
                ]
            , describe "RemoveCharAfter"
                [ RemoveCharAfter.doesNothingOnEndOfDocument
                , RemoveCharAfter.doesntMove
                , RemoveCharAfter.decreasesLineCountOnLastColumnOfNotLastLine
                , RemoveCharAfter.combinesLinesTogetherOnLastColumnOfNotLastLine
                , RemoveCharAfter.decreasesCurrentLineLengthOnNotLastColumn
                , RemoveCharAfter.removesTheCharOnNotLastColumn
                ]
            , describe "Hover"
                [ Hover.setsHoverWithinBounds
                ]
            , describe "GoToHoveredPosition"
                [ GoToHoveredPosition.doesNothingIfHoverIsNoHover
                , GoToHoveredPosition.movesToLastColumnOfHoveredLineIfHoverIsHoverLine
                , GoToHoveredPosition.movesToHoveredPositionIfHoverIsHoverChar
                ]
            , todo "StartSelecting"
            , todo "StopSelecting"
            ]
        ]
