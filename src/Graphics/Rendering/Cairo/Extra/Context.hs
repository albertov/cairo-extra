{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Rendering.Cairo.Extra.Context (cairoCtx) where

import Data.Monoid (mempty, (<>))
import qualified Language.C.Inline as C
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import Foreign.ForeignPtr
import Graphics.Rendering.Cairo.Types

cairoCtx :: C.Context
cairoCtx = C.baseCtx <> ctx'
  where ctx' = mempty {
    ctxTypesTable =
      [ (C.TypeName "cairo_t", [t| Cairo |])
      , (C.TypeName "cairo_surface_t", [t| Surface |])
      ]
    }
