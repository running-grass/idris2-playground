module Image
import System.File
import System.File.Buffer
import Data.Buffer
import Data.String
import System.FFI
import Data.Vect
import Data.Matrix

import public Image.Type
import public Image.Jpeg

export
flipHor : Image m n -> Image m n
flipHor (MkImage matrix) = MkImage $ flipHor matrix

export
flipVer : Image m n -> Image m n
flipVer (MkImage matrix) = MkImage $ flipVer matrix