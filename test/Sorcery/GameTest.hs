module Sorcery.GameTest (tests) where

import qualified Sorcery.Game as T

import Test.Tasty
import Test.Tasty.HUnit

tests = testGroup "Game"
      [ newGameTests
      ]

newGameTests = testGroup "New Game"
    [ gameStartTests
    ]

gameStartTests = testGroup "Starts off with"
    [ testCase "Player one turn start"
        $ T.gameCurrentTurn newGame @?= T.PlayerOneTurn
    , playerStartTests "Player one" (T.gamePlayerOne newGame)
    , playerStartTests "Player two" (T.gamePlayerOne newGame)
    ]
    where
        newGame = T.newGame
        playerStartTests playerName player = testGroup (playerName ++ " with ")
            [ testCase "20 life"
                $ T.playerLifeTotal player @?= 20
            , testCase "An empty battlefield"
                $ T.playerBattlefield player @?= []
            , testCase "Seven cards in their hand"
                $ length (T.playerHand player) @?= 7
            ]
