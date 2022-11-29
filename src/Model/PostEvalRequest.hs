{-# LANGUAGE TemplateHaskell #-}

module Model.PostEvalRequest (PostEvalRequest (expr)) where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text (Text)

data PostEvalRequest = PostEvalRequest { expr :: Text }

$(deriveJSON defaultOptions ''PostEvalRequest)
