{-# LANGUAGE OverloadedStrings #-}

module Server where

import Web.Scotty
import Network.URI (parseURI)
import Data.Monoid (mconcat)

main = scotty 3000 $ do
  get "/" $ do
    html $ mconcat ["<h1>Hello, ", "world!</h1>"]
