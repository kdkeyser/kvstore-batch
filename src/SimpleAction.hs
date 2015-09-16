{-# LANGUAGE GADTs #-}
module SimpleAction where

data SimpleAction a where
    Get :: String -> SimpleAction String
    Put :: String -> String -> SimpleAction ()

