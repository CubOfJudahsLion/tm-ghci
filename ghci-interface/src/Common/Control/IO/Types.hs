{- |
    Module      : Common.Control.IO.Types
    Description : Type-level differentiation of I/O Streams
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    This module implements types for statically differentiating the roles
    of 'Handle's by lifting them to application kind-decorated 'TaggedStream' type.
-}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Control.IO.Types where


import Control.DeepSeq
import Data.Kind
import GHC.Generics
import GHC.TypeLits
import System.IO ( Handle )



-- |  For an 'OutputStream', specifies the kind of output.
type data OutputType  = RegularOutput   -- ^  Normal output, as for @stdout@.
                      | ErrorOutput     -- ^  Error output, as for @stderr@.

-- |  Singleton for 'OutputType'
data SOutputType (ot :: OutputType) where
  SRegularOutput :: SOutputType RegularOutput
  SErrorOutput   :: SOutputType ErrorOutput

deriving instance Show (SOutputType ot)
deriving instance Eq (SOutputType ot)

instance NFData (SOutputType ot) where
  rnf :: SOutputType ot -> ()
  rnf !_ = ()


-- |  Specifies whee the stream is writable or readable.
--    Note that every standard stream is simplex 'Handle'.
type data DirectionType = InputDirection
                        | OutputDirection OutputType

-- |  Singleton for 'DirectionType'
data SDirectionType (dir :: DirectionType) where
  SInputDirection  :: SDirectionType InputDirection
  SOutputDirection :: SOutputType ot -> SDirectionType (OutputDirection ot)

deriving instance Show (SDirectionType dir)
deriving instance Eq (SDirectionType dir)

instance NFData (SDirectionType dir) where
  rnf :: SDirectionType dir -> ()
  rnf !_ = ()


{-# LANGUAGE DataKinds #-}
data StreamTagged app (dir :: DirectionType) t where
  MkAppDir :: application -> SDirectionType direction -> ty -> StreamTagged application direction ty
{-# LANGUAGE NoDataKinds #-}

-- |  An object tagged with an @application@ and @direction@
data StreamableData app (dir :: DirectionType) ob where
  MkTaggedData :: application -> SDirectionType direction -> obj -> StreamableData application direction obj


-- |  A @Stream@ tagged with its newtype Stream StreamType = Stream Handle
-- |  A stream decorated at the type-level with its type and owner.
type TaggedStream app (dir :: DirectionType) = StreamableData app dir Handle

