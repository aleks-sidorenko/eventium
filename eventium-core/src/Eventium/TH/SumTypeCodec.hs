{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Eventium.TH.SumTypeCodec
  ( mkSumTypeCodec,
    mkSumTypeEmbedding,
  )
where

import Data.Char (toLower)
import Language.Haskell.TH
import SumTypesX.TH

-- | This is a template haskell function that creates a 'Codec' between
-- two sum types. The first sum type must be a subset of the second sum type.
-- This is useful in situations where you define all the events in your system
-- in one type, and you want to create sum types that are subsets for each
-- 'Projection'.
--
-- For example, assume we have the following three event types and two sum
-- types holding these events:
--
-- @
--    data EventA = EventA
--    data EventB = EventB
--    data EventC = EventC
--
--    data AllEvents
--      = AllEventsEventA EventA
--      | AllEventsEventB EventB
--      | AllEventsEventC EventC
--
--    data MyEvents
--      = MyEventsEventA EventA
--      | MyEventsEventB EventB
-- @
--
-- In this case, @AllEvents@ holds all the events in our system, and @MyEvents@
-- holds some subset of @AllEvents@. If we run
--
-- @
--    mkSumTypeCodec "myEventsCodec" ''MyEvents ''AllEvents
-- @
--
-- we will produce the following code:
--
-- @
--    -- Encoding function
--    myEventsToAllEvents :: MyEvents -> AllEvents
--    myEventsToAllEvents (MyEventsEventA e) = AllEventsEventA e
--    myEventsToAllEvents (MyEventsEventB e) = AllEventsEventB e
--
--    -- Decoding function
--    allEventsToMyEvents :: AllEvents -> Maybe MyEvents
--    allEventsToMyEvents (AllEventsEventA e) = Just (MyEventsEventA e)
--    allEventsToMyEvents (AllEventsEventB e) = Just (MyEventsEventB e)
--    allEventsToMyEvents _ = Nothing
--
--    -- Codec
--    myEventsCodec :: Codec MyEvents AllEvents
--    myEventsCodec = Codec myEventsToAllEvents allEventsToMyEvents
-- @
mkSumTypeCodec :: String -> Name -> Name -> Q [Dec]
mkSumTypeCodec codecName sourceType targetType = do
  -- Construct the encoding/decoding functions
  let encodeFuncName = firstCharToLower (nameBase sourceType) ++ "To" ++ nameBase targetType
      decodeFuncName = firstCharToLower (nameBase targetType) ++ "To" ++ nameBase sourceType
  -- Generate the sum type converter functions
  encodeDecls <- sumTypeConverter encodeFuncName sourceType targetType
  decodeDecls <- partialSumTypeConverter decodeFuncName targetType sourceType

  -- Construct the codec
  codecTypeDecl <- [t|$(conT $ mkName "Codec") $(conT sourceType) $(conT targetType)|]
  codecExp <- [e|$(conE $ mkName "Codec") $(varE $ mkName encodeFuncName) $(varE $ mkName decodeFuncName)|]
  let codecClause = Clause [] (NormalB codecExp) []

  return $
    [ SigD (mkName codecName) codecTypeDecl,
      FunD (mkName codecName) [codecClause]
    ]
      ++ encodeDecls
      ++ decodeDecls

-- | Like 'mkSumTypeCodec' but generates a 'TypeEmbedding' instead of a
-- 'Codec'. Preferred when the purpose is to embed one sum type into
-- another rather than wire-format encoding.
--
-- For example:
--
-- @
--    mkSumTypeEmbedding "myEventsEmbedding" ''MyEvents ''AllEvents
-- @
--
-- produces:
--
-- @
--    myEventsToAllEvents :: MyEvents -> AllEvents
--    allEventsToMyEvents :: AllEvents -> Maybe MyEvents
--    myEventsEmbedding :: TypeEmbedding MyEvents AllEvents
--    myEventsEmbedding = TypeEmbedding myEventsToAllEvents allEventsToMyEvents
-- @
mkSumTypeEmbedding :: String -> Name -> Name -> Q [Dec]
mkSumTypeEmbedding embeddingName sourceType targetType = do
  let embedFuncName = firstCharToLower (nameBase sourceType) ++ "To" ++ nameBase targetType
      extractFuncName = firstCharToLower (nameBase targetType) ++ "To" ++ nameBase sourceType
  embedDecls <- sumTypeConverter embedFuncName sourceType targetType
  extractDecls <- partialSumTypeConverter extractFuncName targetType sourceType

  embeddingTypeDecl <- [t|$(conT $ mkName "TypeEmbedding") $(conT sourceType) $(conT targetType)|]
  embeddingExp <- [e|$(conE $ mkName "TypeEmbedding") $(varE $ mkName embedFuncName) $(varE $ mkName extractFuncName)|]
  let embeddingClause = Clause [] (NormalB embeddingExp) []

  return $
    [ SigD (mkName embeddingName) embeddingTypeDecl,
      FunD (mkName embeddingName) [embeddingClause]
    ]
      ++ embedDecls
      ++ extractDecls

firstCharToLower :: String -> String
firstCharToLower [] = []
firstCharToLower (x : xs) = toLower x : xs
