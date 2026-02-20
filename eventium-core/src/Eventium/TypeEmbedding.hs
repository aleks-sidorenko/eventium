-- | Defines the 'TypeEmbedding' type for embedding one sum type into another.
--
-- A 'TypeEmbedding' represents a total injection from type @a@ into type @b@
-- and a partial extraction back. This is used when @a@ is conceptually a
-- subset of @b@ — for example, when embedding a per-aggregate event sum type
-- into an application-wide event sum type.
--
-- Unlike 'Eventium.Codec.Codec', a 'TypeEmbedding' carries no
-- event-type-name metadata and is not intended for wire-format conversion.
module Eventium.TypeEmbedding
  ( TypeEmbedding (..),
    composeEmbeddings,
    idEmbedding,
    embeddingToCodec,
  )
where

import Control.Monad
import Eventium.Codec (Codec (..))

-- | A 'TypeEmbedding' describes a total injection from type @a@ into type @b@
-- and a partial extraction back. This is used when @a@ is conceptually a
-- subset of @b@.
--
-- For example, @TypeEmbedding AccountEvent BankEvent@ embeds account-specific
-- events into the application-wide event sum type.
data TypeEmbedding a b = TypeEmbedding
  { -- | Total injection: every @a@ can be embedded into @b@.
    embed :: a -> b,
    -- | Partial extraction: a @b@ value may or may not contain an @a@.
    extract :: b -> Maybe a
  }

-- | Compose two 'TypeEmbedding's: @a@ embeds into @b@, @b@ embeds into @c@.
composeEmbeddings :: TypeEmbedding a b -> TypeEmbedding b c -> TypeEmbedding a c
composeEmbeddings emb1 emb2 =
  TypeEmbedding
    { embed = embed emb2 . embed emb1,
      extract = extract emb2 >=> extract emb1
    }

-- | Identity embedding where no type conversion is needed.
idEmbedding :: TypeEmbedding a a
idEmbedding = TypeEmbedding id Just

-- | Convert a 'TypeEmbedding' to a 'Codec' for interoperability with
-- APIs that require a 'Codec'.
embeddingToCodec :: TypeEmbedding a b -> Codec a b
embeddingToCodec (TypeEmbedding e x) = Codec e x
