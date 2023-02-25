module Sorcery.Game
    ( Card(..)
    , CardZone
    , CardZones(..)
    , Game(..)
    , PerTurnAction(..)
    , Player(..)
    , PlayerTurn(..)
    , newGame
    , playerBattlefield
    , playerExile
    , playerGraveyard
    , playerHand
    , playerLibrary
    ) where

data Card
    = GenericCreatureCard
    | GenericCSInstantCard
    | GenericDrawSorceryCard
    deriving (Eq, Show)

type CardZone = [Card]
data CardZones = CardZones
    { cardZoneHand :: CardZone
    , cardZoneLibrary :: CardZone
    , cardZoneBattlefield :: CardZone
    , cardZoneGraveyard :: CardZone
    , cardZoneExile :: CardZone
    } deriving (Eq, Show)

playerHand :: Player -> CardZone
playerHand = cardZoneHand . playerCardZones

playerLibrary :: Player -> CardZone
playerLibrary = cardZoneLibrary . playerCardZones

playerBattlefield :: Player -> CardZone
playerBattlefield = cardZoneBattlefield . playerCardZones

playerGraveyard :: Player -> CardZone
playerGraveyard = cardZoneGraveyard . playerCardZones

playerExile :: Player -> CardZone
playerExile = cardZoneExile . playerCardZones

data PerTurnAction
    = DrawForTurn
    | PlayLandForTurn
    | EndTurn
    deriving (Eq, Show)

data Player = Player
    { playerCardZones :: CardZones
    , playerLifeTotal :: Int
    , playerTurnActions :: [PerTurnAction]
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

newGame :: Game
newGame = Game newPlayer newPlayer PlayerOneTurn

startingLifeTotal :: Int
startingLifeTotal = 20

startingTurnActions :: [PerTurnAction]
startingTurnActions = [PlayLandForTurn, EndTurn]

newPlayer :: Player
newPlayer = Player
          { playerCardZones = newCardZones
          , playerLifeTotal = startingLifeTotal
          , playerTurnActions = startingTurnActions
          }

newCardZones :: CardZones
newCardZones = CardZones
             { cardZoneHand = newHand
             , cardZoneLibrary = newLibraryOfSize (40 - length newHand)
             , cardZoneBattlefield = []
             , cardZoneGraveyard = []
             , cardZoneExile = []
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