module Source.Layout
  ( Point(..)
  , pointX
  , pointY
  , Box(..)
  , boxA
  , boxB
  , boxInside
  , Element(..)
  , elementBox
  , elementObject
  , elementsPartition
  , Collage()
  , collageBox
  , collageSuperimpose
  , collageElement
  , collageElements
  , Layout(..)
  , layoutCollage
  , layoutSingleton
  , layoutSuperimpose
  , layoutMove
  , layoutCompose
  , layoutWidth
  , layoutHeight
  , layoutVerticalLeft
  , layoutVerticalRight
  , layoutHorizontalTop
  , layoutPadLeft
  , layoutPadRight
  , layoutPadTop
  , layoutPadBottom
  ) where

import Control.Lens
import Data.Monoid
import Data.Either

data Point x y =
  Point {
    _pointX :: x,
    _pointY :: y }
  deriving (Eq)

makeLenses ''Point

instance (Num x, Num y) => Monoid (Point x y) where
  mempty = Point 0 0
  mappend (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

data Box x y =
  Box {
    _boxA :: Point x y,
    _boxB :: Point x y }
  deriving (Eq)

makeLenses ''Box

data Element x y a =
  Element {
    _elementBox :: Box x y,
    _elementObject :: a }
  deriving (Functor)

makeLenses ''Element

data Collage x y a =
  CollageElement (Box x y) a |
  CollageMove (Box x y) (Point x y) (Collage x y a) |
  CollageSuperimpose (Box x y) (Collage x y a) (Collage x y a)
  deriving (Functor)

boxMove :: Monoid (Point x y) => Point x y -> Box x y -> Box x y
boxMove offset (Box a b) = Box (a <> offset) (b <> offset)

boxSuperimpose :: (Ord x, Ord y) => Box x y -> Box x y -> Box x y
boxSuperimpose (Box a1 b1) (Box a2 b2) =
  Box (pointMin a1 a2) (pointMax b1 b2)
  where
    pointMin (Point x1 y1) (Point x2 y2) = Point (min x1 x2) (min y1 y2)
    pointMax (Point x1 y1) (Point x2 y2) = Point (max x1 x2) (max y1 y2)

boxInside :: (Ord x, Ord y) => Box x y -> Point x y -> Bool
boxInside (Box (Point ax ay) (Point bx by)) (Point x y) =
  x >= ax &&
  x <= bx &&
  y >= ay &&
  y <= by

boxWidth :: Num x => Box x y -> x
boxWidth (Box (Point ax _) (Point bx _)) = bx - ax

boxHeight :: Num y => Box x y -> y
boxHeight (Box (Point _ ay) (Point _ by)) = by - ay

elementDistribEither ::
  Element x y (Either a b) ->
  Either (Element x y a) (Element x y b)
elementDistribEither (Element box object) =
  either (Left . Element box) (Right . Element box) object

elementsPartition ::
  [Element x y (Either a b)] ->
  ([Element x y a], [Element x y b])
elementsPartition = partitionEithers . fmap elementDistribEither

collageBox :: Collage x y a -> Box x y
collageBox = \case
  CollageElement box _ -> box
  CollageMove box _ _ -> box
  CollageSuperimpose box _ _ -> box

collageElement :: Box x y -> a -> Collage x y a
collageElement = CollageElement

collageMove ::
  (Num x, Num y) =>
  Point x y ->
  Collage x y a ->
  Collage x y a
collageMove offset collage = CollageMove box offset collage
  where
    box = boxMove offset (collageBox collage)

collageSuperimpose ::
  (Ord x, Ord y) =>
  Collage x y a ->
  Collage x y a ->
  Collage x y a
collageSuperimpose collage1 collage2 =
  CollageSuperimpose box collage1 collage2
  where
    box = boxSuperimpose (collageBox collage1) (collageBox collage2)

collageElements ::
  (Num x, Num y) =>
  Collage x y a ->
  [Element x y a]
collageElements collage = collageElements' mempty collage `appEndo` []
  where
    collageElements' ::
      (Num x, Num y) =>
      Point x y ->
      Collage x y a ->
      Endo [Element x y a]
    collageElements' offset = \case
      CollageElement box a ->
        let box' = boxMove offset box
        in Endo (Element box' a:)
      CollageMove _ offset' collage1 ->
        collageElements' (offset <> offset') collage1
      CollageSuperimpose _ collage1 collage2 ->
        collageElements' offset collage1 <> collageElements' offset collage2

collageRebox ::
  (Num x, Num y) =>
  (Box x y -> Box x y) ->
  Collage x y a ->
  Collage x y a
collageRebox rebox collage = CollageMove box mempty collage
  where
    box = rebox (collageBox collage)

data Layout x y a = Layout ((a -> Box x y) -> Collage x y a)
  deriving (Functor)

layoutCollage :: (a -> Box x y) -> Layout x y a -> Collage x y a
layoutCollage getBox (Layout getCollage) = getCollage getBox

layoutSingleton :: a -> Layout x y a
layoutSingleton a = Layout $ \getBox ->
  collageElement (getBox a) a

layoutSuperimpose ::
  (Ord x, Ord y) =>
  Layout x y a ->
  Layout x y a ->
  Layout x y a
layoutSuperimpose layout1 layout2 = Layout $ \getBox ->
  collageSuperimpose
    (layoutCollage getBox layout1)
    (layoutCollage getBox layout2)

layoutMove ::
  (Num x, Num y) =>
  Point x y ->
  Layout x y a ->
  Layout x y a
layoutMove offset layout = Layout $ \getBox ->
  collageMove offset (layoutCollage getBox layout)

layoutCompose ::
  (Ord x, Ord y, Num x, Num y) =>
  (Box x y -> Box x y -> Point x y) ->
  Layout x y a ->
  Layout x y a ->
  Layout x y a
layoutCompose composer layout1 layout2 =
  Layout $ \getBox ->
    let
      collage1 = layoutCollage getBox layout1
      collage2 = layoutCollage getBox layout2
      box1 = collageBox collage1
      box2 = collageBox collage2
      offset = composer box1 box2
    in
      collageSuperimpose collage1 (collageMove offset collage2)

layoutWidth :: Num x => (a -> Box x y) -> Layout x y a -> x
layoutWidth getBox layout = boxWidth box
  where
    box = collageBox collage
    collage = layoutCollage getBox layout

layoutHeight :: Num y => (a -> Box x y) -> Layout x y a -> y
layoutHeight getBox layout = boxHeight box
  where
    box = collageBox collage
    collage = layoutCollage getBox layout

layoutVerticalLeft ::
  (Ord x, Ord y, Num x, Num y) =>
  Layout x y a ->
  Layout x y a ->
  Layout x y a
layoutVerticalLeft = layoutCompose $
  \(Box (Point x1 _) (Point _ y1)) ->
  \(Box (Point x2 y2) _) ->
    Point (x1 - x2) (y1 - y2)

layoutVerticalRight ::
  (Ord x, Ord y, Num x, Num y) =>
  Layout x y a ->
  Layout x y a ->
  Layout x y a
layoutVerticalRight = layoutCompose $
  \(Box _ (Point x1 y1)) ->
  \(Box (Point _ y2)(Point x2 _)) ->
    Point (x1 - x2) (y1 - y2)

layoutHorizontalTop ::
  (Ord x, Ord y, Num x, Num y) =>
  Layout x y a ->
  Layout x y a ->
  Layout x y a
layoutHorizontalTop = layoutCompose $
  \(Box (Point _ y1) (Point x1 _)) ->
  \(Box (Point x2 y2) _) ->
    Point (x1 - x2) (y1 - y2)

layoutRebox ::
  (Num x, Num y) =>
  (Box x y -> Box x y) ->
  Layout x y a ->
  Layout x y a
layoutRebox rebox layout = Layout $ \getBox ->
  collageRebox rebox (layoutCollage getBox layout)

layoutPadLeft :: (Num x, Num y) => x -> Layout x y a -> Layout x y a
layoutPadLeft x = layoutRebox (boxA . pointX -~ x)

layoutPadRight :: (Num x, Num y) => x -> Layout x y a -> Layout x y a
layoutPadRight x = layoutRebox (boxB . pointX +~ x)

layoutPadTop :: (Num x, Num y) => y -> Layout x y a -> Layout x y a
layoutPadTop y = layoutRebox (boxA . pointY -~ y)

layoutPadBottom :: (Num x, Num y) => y -> Layout x y a -> Layout x y a
layoutPadBottom y = layoutRebox (boxB . pointY +~ y)
