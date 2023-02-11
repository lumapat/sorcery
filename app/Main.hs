module Main (main) where

import Options.Applicative

data SorceryOptions = SorceryOptions
    { runName :: String
    } deriving (Eq, Show)

sorceryOptions :: Parser SorceryOptions
sorceryOptions = SorceryOptions
        <$> strOption
            ( long "runName"
           <> short 'n'
           <> metavar "NAME"
           <> showDefault
           <> value "testRun"
           <> help "Name of the current run"
            )

main :: IO ()
main = do
   o <- execParser opts
   putStrLn $ show o
   where
    opts = info (sorceryOptions <**> helper)
         ( fullDesc
        <> progDesc "Do sorcery"
        <> header "Sorcery!~~~~~"
         )
