module Codec.Image.DevIL
    (ilInit
    ,readImage,writeImage
    ,Word8
    )
where

import Data.Int
import Data.Word

import Foreign hiding (newArray)
import Foreign.C

import Data.Array.Storable
import Data.Array.Unboxed
import Control.Applicative
import Control.Monad

import System.IO.Unsafe (unsafeInterleaveIO)

#include "IL/il.h"


type ILuint    = #type ILuint
type ILsizei   = #type ILsizei
type ILboolean = #type ILboolean
type ILenum    = #type ILenum
type ILint     = #type ILint
type ILubyte   = #type ILubyte

newtype ImageName = ImageName { fromImageName :: ILuint }


foreign import CALLTYPE "ilInit" ilInitC :: IO ()
foreign import CALLTYPE "ilOriginFunc" ilOriginFuncC :: ILenum -> IO ILboolean
foreign import CALLTYPE "ilEnable" ilEnableC :: ILenum -> IO ILboolean

ilInit :: IO ()
ilInit = do
    ilInitC
    ilOriginFuncC (#const IL_ORIGIN_LOWER_LEFT)
    ilEnableC (#const IL_ORIGIN_SET)
    return ()

readImage :: FilePath -> IO (UArray (Int,Int,Int) Word8)
readImage x =
  do [inname] <- ilGenImages 1
     ilBindImage inname
     ilLoadImage x
     a <- toArrayRGBA
     ilDeleteImages [inname]
     return a

writeImage :: FilePath -> UArray (Int,Int,Int) Word8 -> IO ()
writeImage f a =
  do [outname] <- ilGenImages 1
     ilBindImage outname
     fromArrayRGBA a
     ilSaveImage f
     ilDeleteImages [outname]

foreign import CALLTYPE "ilGenImages" ilGenImagesC
  :: ILsizei -> Ptr ILuint -> IO ()


ilGenImages :: Int -> IO [ImageName]
ilGenImages num = do
    ar <- newArray (0, num-1) 0
    withStorableArray ar $ \p -> do
        ilGenImagesC (fromIntegral num) p
    map ImageName <$> getElems ar


foreign import CALLTYPE "ilBindImage" ilBindImageC :: ILuint -> IO ()

ilBindImage :: ImageName -> IO ()
ilBindImage (ImageName name) = ilBindImageC name


foreign import CALLTYPE "ilDeleteImages" ilDeleteImagesC
    :: ILsizei -> Ptr ILuint -> IO ()

ilDeleteImages :: [ImageName] -> IO ()
ilDeleteImages names = do
    ar <- newListArray (0, length names-1) (fromImageName <$> names)
    withStorableArray ar $ \p -> do
        ilDeleteImagesC (fromIntegral $ length names) p


foreign import CALLTYPE "ilLoadImage" ilLoadImageC :: CString -> IO ILboolean

ilLoadImage :: FilePath -> IO Bool
ilLoadImage file = do
    (0 /=) <$> withCString file ilLoadImageC


foreign import CALLTYPE "ilSaveImage" ilSaveImageC :: CString -> IO ILboolean

ilSaveImage :: FilePath -> IO Bool
ilSaveImage file = do
    (0 /=) <$> withCString file ilSaveImageC


foreign import CALLTYPE "ilConvertImage" ilConvertImageC
    :: ILenum -> ILenum -> IO Bool

foreign import CALLTYPE "ilGetInteger" ilGetIntegerC
    :: ILenum -> IO ILint

foreign import CALLTYPE "ilCopyPixels" ilCopyPixelsC
    :: ILuint -> ILuint -> ILuint   -- x y z
    -> ILuint -> ILuint -> ILuint   -- w h depth
    -> ILenum -> ILenum             -- format type
    -> Ptr ()                       -- data (copy into this pointer)
    -> IO ()

foreign import CALLTYPE "ilSetPixels" ilSetPixelsC
    :: ILuint -> ILuint -> ILuint   -- x y z
    -> ILuint -> ILuint -> ILuint   -- w h depth
    -> ILenum -> ILenum             -- format type
    -> Ptr ()                       -- data (copy from this pointer)
    -> IO ()

foreign import CALLTYPE "ilTexImage" ilTexImageC
    :: ILuint -> ILuint -> ILuint   -- w h depth
    -> ILubyte -> ILenum -> ILenum  -- numberOfChannels format type
    -> Ptr ()                       -- data (copy from this pointer)
    -> IO Bool

il_RGBA = (#const IL_RGBA) :: ILenum
il_UNSIGNED_BYTE = (#const IL_UNSIGNED_BYTE) :: ILenum
il_IMAGE_HEIGHT = (#const IL_IMAGE_HEIGHT) :: ILenum
il_IMAGE_WIDTH  = (#const IL_IMAGE_WIDTH)  :: ILenum

-- array indices are (x,y,channel) where channel: 0=Red, 1=Green, 2=Blue, 3=Alpha
toArrayRGBA :: IO (UArray (Int,Int,Int) Word8)
toArrayRGBA = do
    width  <- ilGetIntegerC il_IMAGE_WIDTH
    height <- ilGetIntegerC il_IMAGE_HEIGHT
    let bounds = ((0,0,0), (fromIntegral width-1, fromIntegral height-1, 3))
    ar <- newArray_ bounds
    withStorableArray ar $ \p -> do
        ilCopyPixelsC 0 0 0 
                      (fromIntegral width) (fromIntegral height) 1 
                      il_RGBA il_UNSIGNED_BYTE 
                      (castPtr p)
    listArray bounds <$> lazyElems ar

lazyElems ar = do
    ixs <- range <$> getBounds ar
    go ixs
  where
    go [] = return []
    go (ix:ixs) = unsafeInterleaveIO $ liftM2 (:) (readArray ar ix) (go ixs)

-- same as toArrayRGBA
fromArrayRGBA :: UArray (Int,Int,Int) Word8 -> IO ()
fromArrayRGBA dat = do
    let ((0,0,0), (maxx,maxy,3)) = bounds dat
    ar <- unsafeThaw dat
    withStorableArray ar $ \p -> do
        ilTexImageC (fromIntegral maxx+1) (fromIntegral maxy+1) 1
                    4 il_RGBA il_UNSIGNED_BYTE
                    (castPtr p)
    return ()

-- vim: ft=haskell :
