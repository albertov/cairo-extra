{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Rendering.Cairo.Extra (
  surfaceWriteToPNGByteString
) where

import           Control.Exception (bracket)
import           Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder, toLazyByteString, byteString)
import           Data.IORef
import           Data.Monoid (mempty, (<>))
import           Graphics.Rendering.Cairo.Extra.Context
import           Graphics.Rendering.Cairo.Types
import qualified Graphics.Rendering.Cairo.Internal as Internal

import           Foreign.StablePtr
import           Foreign.C.String (CString)
import qualified Language.C.Inline as C

C.context (cairoCtx <> C.fptrCtx <> C.funCtx)

C.include "<cairo/cairo.h>"
C.include "<string.h>"
C.verbatim "extern cairo_status_t cairoExtraWriteCallback(void *, const unsigned char *,  unsigned int);"

                                        
surfaceWriteToPNGByteString :: Surface -> IO LBS.ByteString
surfaceWriteToPNGByteString surf = do
  ref <- newIORef mempty
  bracket (newStablePtr ref) freeStablePtr $ \refStablePtr -> do
    let refPtr = castStablePtrToPtr refStablePtr
    status <- toEnum . fromIntegral <$> [C.exp|int  {
        cairo_surface_write_to_png_stream (
          $fptr-ptr:(cairo_surface_t *surf),
          &cairoExtraWriteCallback,
          $(void *refPtr)
          )
      }|]
    unless (status == StatusSuccess) $
      fail =<< Internal.statusToString status
    toLazyByteString <$> readIORef ref

cairoExtraWriteCallback
  :: StablePtr (IORef Builder) -> CString -> C.CSize -> IO C.CInt
cairoExtraWriteCallback p c l = do
  s <- BS.packCStringLen (c, fromIntegral l)
  ref <- deRefStablePtr p
  modifyIORef' ref (<> byteString s)
  pure $ fromIntegral $ fromEnum StatusSuccess

foreign export ccall cairoExtraWriteCallback
  :: StablePtr (IORef Builder) -> CString -> C.CSize -> IO C.CInt
