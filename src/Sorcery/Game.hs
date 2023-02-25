module Sorcery.Game
    ( Card(..)
    , Game(..)
    , PerTurnAction(..)
    , Player(..)
    , PlayerTurn(..)
    , newGame
    ) where

data Card
    = GenericCreatureCard
    | GenericCSInstantCard
    | GenericDrawSorceryCard
    deriving (Eq, Show)

data PerTurnAction
    = DrawForTurn
    | PlayLandForTurn
    | EndTurn
    deriving (Eq, Show)

data Player = Player
    { playerBattlefield :: [Card]
    , playerHand :: [Card]
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
          { playerBattlefield = []
          , playerHand = newHand
          , playerLifeTotal = startingLifeTotal
          , playerTurnActions = startingTurnActions
          }

newHand :: [Card]
newHand =
    [ GenericCreatureCard
    , GenericCSInstantCard
    , GenericDrawSorceryCard
    , GenericCreatureCard
    , GenericCreatureCard
    , GenericCSInstantCard
    , GenericDrawSorceryCard
    ]