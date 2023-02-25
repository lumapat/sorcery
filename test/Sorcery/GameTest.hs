module Sorcery.GameTest (tests) where

import qualified Sorcery.Game as T

import Control.Lens ((^.))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Game"
      [ gameStartTests
      , gameActionTests
      ]

gameStartTests :: TestTree
gameStartTests = testGroup "Starts off with"
    [ testCase "Player one's turn first"
        $ T.gameCurrentTurn newGame @?= T.PlayerOneTurn
    , playerStartTests "Player one" (T.gamePlayerOne newGame)
    , playerStartTests "Player two" (T.gamePlayerOne newGame)
    ]
    where
        newGame = T.newGame
        playerStartTests playerName player = testGroup (playerName ++ " with ")
            [ testCase "20 life"
                $ player ^. T.playerLifeTotal @?= 20
            , testCase "Seven cards in their hand"
                $ length (T.playerHand player) @?= 7
            , testCase "40 cards total (hand and library)"
                $ (sum . fmap (length . ($ player))) [T.playerHand, T.playerLibrary] @?= 40
            , testCase "An empty battlefield"
                $ T.playerBattlefield player @?= []
            , testCase "An empty exile zone"
                $ T.playerGraveyard player @?= []
            , testCase "An empty graveyard"
                $ T.playerExile player @?= []
            ]

gameActionTests = testGroup "When a player"
    [ drawTests
    ]
    where
        drawTests = testGroup "draws a card"
            [ testCase "the card is put into their hand"
                $ length (T.playerHand playerThatDrewCard) @?= 8
            , testCase "the card is removed from their library"
                $ length (T.playerLibrary playerThatDrewCard) @?= 32
            , testCase "the number of cards stay the same between their hand and library"
                $ (sum . fmap (length . ($ playerThatDrewCard))) [T.playerHand, T.playerLibrary] @?= 40
            ]
            where
                (playerThatDrewCard, _) = T.drawCards 1 T.newPlayer