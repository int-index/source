module Source.Layout
  ( Point(..)
  , pointX
  , pointY
  , Box(..)
  , boxA
  , boxB
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
  , layoutResize
  , layoutVertical
  , boxPadLeft
  , boxPadRight
  , boxPadTop
  , boxPadBottom
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
  CollageMove (Box x y) (Endo (Point x y)) (Collage x y a) |
  CollageSuperimpose (Box x y) (Collage x y a) (Collage x y a)
  deriving (Functor)

boxMove :: (Point x y -> Point x y) -> Box x y -> Box x y
boxMove move (Box a b) = Box (move a) (move b)

boxSuperimpose :: (Ord x, Ord y) => Box x y -> Box x y -> Box x y
boxSuperimpose (Box a1 b1) (Box a2 b2) =
  Box (pointMin a1 a2) (pointMax b1 b2)
  where
    pointMin (Point x1 y1) (Point x2 y2) = Point (min x1 x2) (min y1 y2)
    pointMax (Point x1 y1) (Point x2 y2) = Point (max x1 x2) (max y1 y2)

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

collageResize ::
  (Box x y -> Box x y) ->
  Collage x y a ->
  Collage x y a
collageResize resize = \case
  CollageElement box a ->
    CollageElement (resize box) a
  CollageMove box move collage ->
    CollageMove (resize box) move collage
  CollageSuperimpose box collage1 collage2 ->
    CollageSuperimpose (resize box) collage1 collage2

collageElement :: Box x y -> a -> Collage x y a
collageElement = CollageElement

collageMove ::
  (Point x y -> Point x y) ->
  Collage x y a ->
  Collage x y a
collageMove move collage = CollageMove box (Endo move) collage
  where
    box = boxMove move (collageBox collage)

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
  Collage x y a ->
  [Element x y a]
collageElements collage = collageElements' mempty collage `appEndo` []
  where
    collageElements'
      :: Endo (Point x y)
      -> Collage x y a
      -> Endo [Element x y a]
    collageElements' move = \case
      CollageElement box a ->
        let box' = boxMove (appEndo move) box
        in Endo (Element box' a:)
      CollageMove _ move' collage1 ->
        collageElements' (move <> move') collage1
      CollageSuperimpose _ collage1 collage2 ->
        collageElements' move collage1 <> collageElements' move collage2

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
  (Point x y -> Point x y) ->
  Layout x y a ->
  Layout x y a
layoutMove move layout = Layout $ \getBox ->
  collageMove move (layoutCollage getBox layout)

layoutResize ::
  (Box x y -> Box x y) ->
  Layout x y a ->
  Layout x y a
layoutResize resize layout = Layout $ \getBox ->
  collageResize resize (layoutCollage getBox layout)

layoutVertical ::
  (Ord x, Ord y, Num y) =>
  Layout x y a ->
  Layout x y a ->
  Layout x y a
layoutVertical layout1 layout2 = Layout $ \getBox ->
  let
    collage1 = layoutCollage getBox layout1
    collage2 = layoutCollage getBox layout2
    Box _ (Point _ y1) = collageBox collage1
    Box (Point _ y2) _ = collageBox collage2
    move (Point x y) = Point x (y + y1 - y2)
  in
    collageSuperimpose collage1 (collageMove move collage2)

--

boxPadLeft :: Num x => x -> Box x y -> Box x y
boxPadLeft x (Box (Point ax ay) b) = Box (Point (ax-x) ay) b

boxPadRight :: Num x => x -> Box x y -> Box x y
boxPadRight x (Box a (Point bx by)) = Box a (Point (bx+x) by)

boxPadTop :: Num y => y -> Box x y -> Box x y
boxPadTop y (Box (Point ax ay) b) = Box (Point ax (ay-y)) b

boxPadBottom :: Num y => y -> Box x y -> Box x y
boxPadBottom y (Box a (Point bx by)) = Box a (Point bx (by+y))
