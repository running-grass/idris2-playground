module Image.Type

import Data.Matrix
import Data.Prim.Bits8

import Generics.Derive

%language ElabReflection

public export
isBits8 : Num a => Ord a => a -> Bool
isBits8 i = i >= 0 && i <= 255

private
toBits8 : Num a => Ord a => Cast a Bits8 => a -> Maybe Bits8
toBits8 n = if isBits8 n then Just $ cast n else Nothing

public export
record Red where
  constructor MkRed
  value       : Bits8

%runElab derive "Red" [Generic, Meta, Eq, Ord, Show]

namespace Red
  export
  fromInteger : (i : Integer) -> {auto 0 prf : isBits8 i === True} -> Red
  fromInteger int =  MkRed . cast $ int


public export
record Green where
  constructor MkGreen
  value       : Bits8

%runElab derive "Green" [Generic, Meta, Eq, Ord, Show]

namespace Green
  export
  fromInteger : (i : Integer) -> {auto 0 prf : isBits8 i === True} -> Green
  fromInteger int =  MkGreen . cast $ int


public export
record Blue where
  constructor MkBlue
  value       : Bits8

%runElab derive "Blue" [Generic, Meta, Eq, Ord, Show]

namespace Blue
  export
  fromInteger : (i : Integer) -> {auto 0 prf : isBits8 i === True} -> Blue
  fromInteger int =  MkBlue . cast $ int


public export
record Alpha where
  constructor MkAlpha
  value       : Bits8

%runElab derive "Alpha" [Generic, Meta, Eq, Ord, Show]

namespace Alpha
  export
  fromInteger : (i : Integer) -> {auto 0 prf : isBits8 i === True} -> Alpha
  fromInteger int =  MkAlpha . cast $ int


public export
record RGB where
  constructor MkRGB
  red       : Red
  green     : Green
  blue      : Blue

%runElab derive "RGB" [Generic, Meta, Eq, Ord, Show]

namespace RGB
  export
  white : RGB
  white = MkRGB 255 255 255

  export
  black : RGB
  black = MkRGB 0 0 0

  export
  fromBits8s : (red : Bits8) -> (green: Bits8) -> (blue: Bits8) -> RGB
  fromBits8s r g b = MkRGB (MkRed r) (MkGreen g) (MkBlue b)

  export
  fromTuple : (Bits8, Bits8, Bits8) -> RGB
  fromTuple (r, g, b) = fromBits8s r g b

  export
  fromNums : Num n => Ord n => Cast n Bits8 =>  (red : n) -> (green: n) -> (blue: n) -> Maybe RGB
  fromNums r g b = caseRGB (toBits8 r) (toBits8 g) (toBits8 b)
    where
      caseRGB : Maybe Bits8 -> Maybe Bits8 -> Maybe Bits8 -> Maybe RGB
      caseRGB (Just r') (Just g') (Just b') = Just $ fromBits8s r' g' b'
      caseRGB _ _ _ = Nothing

  export
  fromTuple' : Num n => Ord n => Cast n Bits8 => (n, n, n) -> Maybe RGB
  fromTuple' (r, g, b) = fromNums r g b

public export
record RGBA where
  constructor MkRGBA
  red       : Red
  green     : Green
  blue      : Blue
  alpha     : Alpha

%runElab derive "RGBA" [Generic, Meta, Eq, Ord, Show]

namespace RGBA
  export
  white : RGBA
  white = MkRGBA 255 255 255 255

  export
  black : RGBA
  black = MkRGBA 0 0 0 255

  export
  transparent : RGBA
  transparent = MkRGBA 0 0 0 0

export
toRGB : RGBA -> RGB
toRGB (MkRGBA r g b _) = MkRGB r g b


export
toRGBA : RGB -> RGBA
toRGBA (MkRGB r g b ) = MkRGBA r g b 255


public export
record Image (rows: Nat) (cols: Nat) where
  constructor MkImage
  value       : Matrix rows cols RGB


public export
Eq (Image rows cols) where
  (MkImage lv) == (MkImage rv) = lv == rv

public export
Show  (Image rows cols) where
  show (MkImage img) = "MkImage (" ++ show img ++ ")"


-- public export
-- ImageRGBA : (m: Nat) -> (n: Nat) ->Type
-- ImageRGBA m n = Matrix m n RGBA