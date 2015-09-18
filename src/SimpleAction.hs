{-# LANGUAGE GADTs #-}
module SimpleAction where

-- | Basic key/value store API
-- | Get retrieve the value associated with a key
-- | Put sets the value associated with a key
-- | Key/values are strings
data SimpleAction a where
    Get :: String -> SimpleAction String
    Put :: String -> String -> SimpleAction ()

