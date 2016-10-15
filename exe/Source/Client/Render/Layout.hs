module Source.Client.Render.Layout
  ( imageBox
  , renderImageElements
  , layoutEmptyImage
  ) where

import Control.Lens
import Graphics.Vty as Vty

import Source.Layout

layoutEmptyImage :: Layout x y Image
layoutEmptyImage = layoutSingleton emptyImage

imageBox :: Vty.Image -> Box Int Int
imageBox image = Box a b
  where
    a = Point 0 0
    b = Point (imageWidth image) (imageHeight image)

renderImageElements :: [Element Int Int Image] -> Picture
renderImageElements [] = picForImage emptyImage
renderImageElements els = picForLayers (renderElement <$> els)
  where
    renderElement el =
      translate ax ay . crop (bx-ax) (by-ay) $
        el ^. elementObject
      where
        Box (Point ax ay) (Point bx by) = el ^. elementBox
