{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler
  ( getVarsR
  , getVarR
  , postVarsR
  , putVarR
  , deleteVarR
  , postEvalR
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), object, toJSON)
import Data.Bool (bool)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isNothing, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Database.Persist
  ( Entity(entityKey, entityVal)
  , Filter
  , SelectOpt(Asc, Desc, LimitTo)
  , (=.)
  , (==.)
  , deleteBy
  , getBy
  , insert
  , selectList
  , update
  )
import Network.HTTP.Types (status400, status500)
import Yesod
  ( TypedContent
  , defaultLayout
  , lookupGetParam
  , provideRep
  , requireCheckJsonBody
  , selectRep
  , sendResponseStatus
  , whamlet
  , whamletFile
  )
import Yesod.Persist (getBy404, runDB)

import Expr
import Foundation
import qualified Model.PostEvalRequest as PostEvalRequest
import qualified Model.PutVariableRequest as PutVariableRequest

-------------------------------------------------------------------------------
-- Get variable(s)
-------------------------------------------------------------------------------

-- | Handle @\/variables GET@ requests by returning the variables specified by
-- the filter and limit conditions specified in the query parameters, and
-- sorted by the field specified in the query parameters.
getVarsR :: Handler TypedContent
getVarsR = do
  entities <- selectList <$> getFilters <*> getSelectOpts >>= runDB
  let variables = map entityVal entities
  selectRep $ do
    provideRep (return (toJSON variables))
    provideRep (defaultLayout $(whamletFile "templates/variables-table.hamlet"))

-- | Handle @\/variables\/{name} GET@ requests for a single variable name by
-- returning the variable with that name, if it exists. If it does not exist, a
-- 404 error response is returned.
getVarR :: Text -> Handler TypedContent
getVarR name = do
  entity <- runDB (getBy404 (UniqueName name))
  selectRep $ do
    provideRep (return (toJSON (entityVal entity)))
    provideRep $ do
      let variables = [entityVal entity]
      defaultLayout $(whamletFile "templates/variables-table.hamlet")

getFilters :: Handler [Filter Variable]
getFilters = maybeToList . fmap (VariableName ==.) <$> lookupGetParam "name"

getSelectOpts :: Handler [SelectOpt Variable]
getSelectOpts = do
  maybeSortBy <- lookupGetParam "sort"
  maybeDesc <- lookupGetParam "desc"
  let maybeSort =
        maybeSortBy >>= resolveField (bool Desc Asc (isNothing maybeDesc))
  maybeLimit <- fmap (LimitTo . read . Text.unpack) <$> lookupGetParam "limit"
  return (catMaybes [maybeSort, maybeLimit])
  where
    resolveField ::
         (forall t. EntityField Variable t -> SelectOpt Variable)
      -> Text
      -> Maybe (SelectOpt Variable)
    resolveField sortOrder "name" = Just (sortOrder VariableName)
    resolveField sortOrder "value" = Just (sortOrder VariableValue)
    resolveField sortOrder "created" = Just (sortOrder VariableCreated)
    resolveField sortOrder "updated" = Just (sortOrder VariableUpdated)
    resolveField _ _ = Nothing

-------------------------------------------------------------------------------
-- Create/update/delete variable(s)
-------------------------------------------------------------------------------

-- | Handle @\/variables POST@ requests by creating or updating multiple
-- variables at once. The variables to be updated are specified in the request
-- body, which is a JSON object whose keys are variable names and values are
-- the corresponding values. E.g.
--
-- > {"a": 10, "b": -0.5}
postVarsR :: Handler ()
postVarsR = requireCheckJsonBody >>= mapM_ (uncurry setVariable) . Map.toList

-- | Handle @\/variables\/{name} PUT@ requests by creating or updating a single
-- variable. The name of the variable to be updated is specified in the request
-- URL slug, and the value to assign to that variable is represented in the
-- request body as a JSON object of the form
--
-- > {"value": 100}
putVarR :: Text -> Handler ()
putVarR name =
  requireCheckJsonBody >>= setVariable name . PutVariableRequest.value

-- | Handle @\/variables/{name} DELETE@ requests by deleting the variable with
-- the given name. If a variable with the given name doesn't exist, nothing
-- happens.
deleteVarR :: Text -> Handler ()
deleteVarR name = runDB (deleteBy (UniqueName name))

setVariable :: Text -> Double -> Handler ()
setVariable name value
  | isValidName name = do
    time <- liftIO getCurrentTime
    maybeEntity <- runDB (getBy (UniqueName name))
    case maybeEntity of
      Nothing -> do
        let variable = Variable name value time time
        _ <- runDB (insert variable)
        return ()
      Just entity -> do
        let updates = [VariableValue =. value, VariableUpdated =. time]
        runDB $ update (entityKey entity) updates
setVariable name _
  | otherwise = sendResponseStatus status400 (Text.append "Invalid name: " name)

-------------------------------------------------------------------------------
-- Evaluate an expression
-------------------------------------------------------------------------------

-- | Handle @\/ POST@ requests by evaluating the expression in the request
-- body, which should be a JSON object like
--
-- > {"expr": "a*b + 100/(c-d)"}
--
-- If any of the variables occurring in the expression aren't defined in the
-- variable store, then a 404 is returned.
postEvalR :: Handler TypedContent
postEvalR = do
  request <- requireCheckJsonBody
  case (parseExpr (PostEvalRequest.expr request)) of
    Left err -> sendResponseStatus status400 (show err)
    Right expr -> do
      env <- fmap Map.fromList (mapM loadVar (Set.toList (variablesOf expr)))
      case (evaluateExpr env expr) of
        Nothing ->
          sendResponseStatus status500 ("Unknown error occurred" :: Text)
        Just value ->
          selectRep $ do
            provideRep $ return $ object $ ["value" .= value]
            provideRep $ defaultLayout [whamlet|#{show value}|]
  where
    loadVar :: Text -> Handler (Text, Double)
    loadVar name = do
      entity <- runDB (getBy404 (UniqueName name))
      let variable = entityVal entity
      return (variableName variable, variableValue variable)
