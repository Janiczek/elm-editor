module Tests exposing (..)

import Test exposing (..)
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
    describe "Code Editor"
        [ describe "Invariants"
            [ describe "position"
                [ describe "line"
                    [ Invariants.positionLineIsAlwaysPositive
                    , Invariants.positionLineNeverGetsToNonexistingLine
                    ]
                , describe "column"
                    [ Invariants.positionColumnIsAlwaysPositive
                    , Invariants.positionColumnNeverGetsMoreThanOneCharAfterLineContents
                    ]
                ]
            , describe "lines"
                [ Invariants.linesArrayNeverEmpty
                ]
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
            ]
        ]
