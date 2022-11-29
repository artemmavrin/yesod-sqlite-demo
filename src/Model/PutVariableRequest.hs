{-# LANGUAGE TemplateHaskell #-}

module Model.PutVariableRequest (PutVariableRequest (value)) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

data PutVariableRequest = PutVariableRequest { value :: Double }

$(deriveJSON defaultOptions ''PutVariableRequest)
