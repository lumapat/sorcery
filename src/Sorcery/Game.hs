{-# LANGUAGE TemplateHaskell #-}

module Sorcery.Game
    ( Card(..)
    , CardZone
    , CardZones(..)
    , Game(..)
    , PerTurnAction(..)
    , Player(..)
    , PlayerTurn(..)
    , drawCards
    , newGame
    , newPlayer -- TODO: Does this need to be exported?
    , playerBattlefield
    , playerExile
    , playerGraveyard
    , playerHand
    , playerLibrary
    -- Lenses
    -- TODO: Export or not? Or abstract away
    , cardZoneBattlefield
    , cardZoneExile
    , cardZoneGraveyard
    , cardZoneHand
    , cardZoneLibrary
    , playerCardZones
    , playerLifeTotal
    , playerTurnActions
    ) where

import Control.Lens

-- TODO: Move all of these into their own "game state" module
data Card
    = GenericCreatureCard
    | GenericCSInstantCard
    | GenericDrawSorceryCard
    deriving (Eq, Show)

type CardZone = [Card]
data CardZones = CardZones
    { _cardZoneHand :: CardZone
    , _cardZoneLibrary :: CardZone
    , _cardZoneBattlefield :: CardZone
    , _cardZoneGraveyard :: CardZone
    , _cardZoneExile :: CardZone
    } deriving (Eq, Show)

data PerTurnAction
    = DrawForTurn
    | PlayLandForTurn
    | EndTurn
    deriving (Eq, Show)

data Player = Player
    { _playerCardZones :: CardZones
    , _playerLifeTotal :: Int
    , _playerTurnActions :: [PerTurnAction]
    } deriving (Eq, Show)

data PlayerTurn
    = PlayerOneTurn
    | PlayerTwoTurn
    deriving (Eq, Show)

data Game = Game
    { gamePlayerOne :: Player
    , gamePlayerTwo :: Player
    , gameCurrentTurn :: PlayerTurn
    } deriving (Eq, Show)

-- Generate lenses
makeLenses ''CardZones
makeLenses ''Player

playerHand :: Player -> CardZone
playerHand = view (playerCardZones . cardZoneHand)

playerLibrary :: Player -> CardZone
playerLibrary = view (playerCardZones . cardZoneLibrary)

playerBattlefield :: Player -> CardZone
playerBattlefield = view (playerCardZones . cardZoneBattlefield)

playerGraveyard :: Player -> CardZone
playerGraveyard = view (playerCardZones . cardZoneGraveyard)

playerExile :: Player -> CardZone
playerExile = view (playerCardZones . cardZoneExile)

newGame :: Game
newGame = Game newPlayer newPlayer PlayerOneTurn

startingLifeTotal :: Int
startingLifeTotal = 20

startingTurnActions :: [PerTurnAction]
startingTurnActions = [PlayLandForTurn, EndTurn]

newPlayer :: Player
newPlayer = Player
          { _playerCardZones = newCardZones
          , _playerLifeTotal = startingLifeTotal
          , _playerTurnActions = startingTurnActions
          }

newCardZones :: CardZones
newCardZones = CardZones
             { _cardZoneHand = newHand
             , _cardZoneLibrary = newLibraryOfSize (40 - length newHand)
             , _cardZoneBattlefield = []
             , _cardZoneGraveyard = []
             , _cardZoneExile = []
             }

newHand :: CardZone
newHand =
    [ GenericCreatureCard
    , GenericCSInstantCard
    , GenericDrawSorceryCard
    , GenericCreatureCard
    , GenericCreatureCard
    , GenericCSInstantCard
    , GenericDrawSorceryCard
    ]

newLibraryOfSize :: Int -> CardZone
newLibraryOfSize s = take s
                   $ cycle
                   [ GenericCreatureCard
                   , GenericCSInstantCard
                   , GenericDrawSorceryCard
                   ]

-- Game Actions
-- TODO: Optimize or make cleaner?
drawCards :: Int -> Player -> (Player, [Card])
drawCards numTimes player = (over (playerCardZones . cardZoneHand) (++ drawnCards) player', drawnCards)
    where
        drawnCards = take numTimes $ playerLibrary player
        player' = over (playerCardZones . cardZoneLibrary) (drop numTimes) player