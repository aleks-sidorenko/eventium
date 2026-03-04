{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Eventium.TH.SumType
  ( -- * Constructing sum types
    constructSumType,
    SumTypeOptions,
    defaultSumTypeOptions,
    sumTypeOptions,
    withTagOptions,
    withConstructorStrictness,
    SumTypeTagOptions (..),
    SumTypeConstructorStrictness (..),

    -- * Converting between sum types
    sumTypeConverter,
    partialSumTypeConverter,
  )
where

import Language.Haskell.TH
import Prelude

-- | Options for constructing sum types.
data SumTypeOptions = SumTypeOptions
  { sumTypeOptionsTagOptions :: SumTypeTagOptions,
    sumTypeOptionsConstructorStrictness :: SumTypeConstructorStrictness
  }

-- | Default options: prefix tags with type name, lazy constructors.
defaultSumTypeOptions :: SumTypeOptions
defaultSumTypeOptions =
  SumTypeOptions
    { sumTypeOptionsTagOptions = PrefixTagsWithTypeName,
      sumTypeOptionsConstructorStrictness = LazySumTypeConstructors
    }

-- | Construct 'SumTypeOptions' from tag options and constructor strictness.
sumTypeOptions :: SumTypeTagOptions -> SumTypeConstructorStrictness -> SumTypeOptions
sumTypeOptions = SumTypeOptions

-- | Set the tag options on a 'SumTypeOptions' value.
withTagOptions :: SumTypeTagOptions -> SumTypeOptions -> SumTypeOptions
withTagOptions opts (SumTypeOptions _ s) = SumTypeOptions opts s

-- | Set the constructor strictness on a 'SumTypeOptions' value.
withConstructorStrictness :: SumTypeConstructorStrictness -> SumTypeOptions -> SumTypeOptions
withConstructorStrictness s (SumTypeOptions opts _) = SumTypeOptions opts s

-- | How to name constructors in the generated sum type.
data SumTypeTagOptions
  = PrefixTagsWithTypeName
  | AppendTypeNameToTags
  | ConstructTagName (String -> String)

-- | Whether generated constructors have strict or lazy fields.
data SumTypeConstructorStrictness
  = LazySumTypeConstructors
  | StrictSumTypeConstructors
  deriving (Show, Eq)

-- | Construct a sum type from a list of existing types.
constructSumType :: String -> SumTypeOptions -> [Name] -> Q [Dec]
constructSumType typeName SumTypeOptions {..} types = do
  let strictness = toSourceStrictness sumTypeOptionsConstructorStrictness
      mkConstructor name =
        NormalC
          (constructorName sumTypeOptionsTagOptions typeName name)
          [(Bang NoSourceUnpackedness strictness, ConT name)]
      constructors = map mkConstructor types
  return [DataD [] (mkName typeName) [] Nothing constructors []]

-- | Generate a total conversion function between two sum types.
sumTypeConverter :: String -> Name -> Name -> Q [Dec]
sumTypeConverter functionName sourceType targetType = do
  bothConstructors <- matchTypeConstructors sourceType targetType
  let funcName = mkName functionName
  funcClauses <- mapM mkSerializeFunc bothConstructors
  typeDecl <- [t|$(conT sourceType) -> $(conT targetType)|]
  return
    [ SigD funcName typeDecl,
      FunD funcName funcClauses
    ]

-- | Generate a partial conversion function between two sum types.
partialSumTypeConverter :: String -> Name -> Name -> Q [Dec]
partialSumTypeConverter functionName sourceType targetType = do
  bothConstructors <- matchTypeConstructors targetType sourceType
  let funcName = mkName functionName
      wildcardClause = Clause [WildP] (NormalB (ConE 'Nothing)) []
  funcClauses <- mapM mkDeserializeFunc bothConstructors
  typeDecl <- [t|$(conT sourceType) -> Maybe $(conT targetType)|]
  return
    [ SigD funcName typeDecl,
      FunD funcName (funcClauses ++ [wildcardClause])
    ]

-- Internal helpers

constructorName :: SumTypeTagOptions -> String -> Name -> Name
constructorName PrefixTagsWithTypeName typeName =
  mkName . (typeName ++) . nameBase
constructorName AppendTypeNameToTags typeName =
  mkName . (++ typeName) . nameBase
constructorName (ConstructTagName mkCtor) _ =
  mkName . mkCtor . nameBase

toSourceStrictness :: SumTypeConstructorStrictness -> SourceStrictness
toSourceStrictness LazySumTypeConstructors = NoSourceStrictness
toSourceStrictness StrictSumTypeConstructors = SourceStrict

data BothConstructors = BothConstructors
  { innerType :: Type,
    sourceConstructor :: Name,
    targetConstructor :: Name
  }

matchTypeConstructors :: Name -> Name -> Q [BothConstructors]
matchTypeConstructors sourceType targetType = do
  sourceConstructors <- typeConstructors sourceType
  targetConstructors <- typeConstructors targetType
  mapM (matchConstructor targetConstructors) sourceConstructors

typeConstructors :: Name -> Q [(Type, Name)]
typeConstructors typeName = do
  info <- reify typeName
  case info of
    (TyConI (DataD _ _ _ _ constructors _)) ->
      mapM go constructors
      where
        go (NormalC name []) =
          fail $ "Constructor " ++ nameBase name ++ " doesn't have any arguments"
        go (NormalC name [(_, type')]) = return (type', name)
        go (NormalC name _) =
          fail $ "Constructor " ++ nameBase name ++ " has more than one argument"
        go _ =
          fail $ "Invalid constructor in " ++ nameBase typeName
    _ -> fail $ nameBase typeName ++ " must be a sum type"

matchConstructor :: [(Type, Name)] -> (Type, Name) -> Q BothConstructors
matchConstructor targetConstructors (type', sourceConstructor) = do
  targetConstructor <-
    maybe
      ( fail $
          "Can't find constructor in target type corresponding to "
            ++ nameBase sourceConstructor
      )
      return
      (lookup type' targetConstructors)
  return $ BothConstructors type' sourceConstructor targetConstructor

mkSerializeFunc :: BothConstructors -> Q Clause
mkSerializeFunc BothConstructors {..} = do
  varName <- newName "value"
  let patternMatch = ConP sourceConstructor [] [VarP varName]
      constructor = AppE (ConE targetConstructor) (VarE varName)
  return $ Clause [patternMatch] (NormalB constructor) []

mkDeserializeFunc :: BothConstructors -> Q Clause
mkDeserializeFunc BothConstructors {..} = do
  varName <- newName "value"
  let patternMatch = ConP targetConstructor [] [VarP varName]
      constructor =
        AppE (ConE 'Just) (AppE (ConE sourceConstructor) (VarE varName))
  return $ Clause [patternMatch] (NormalB constructor) []
