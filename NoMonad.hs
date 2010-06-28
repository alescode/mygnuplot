module Test (module Test) where

data ParserStatus = ParserStatus { token :: String
                                 , lineNumber :: Int
                                 , colNumber :: Int
                                 }
                                 deriving (Show)
