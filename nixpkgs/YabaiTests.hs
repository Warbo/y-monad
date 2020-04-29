#!/usr/bin/env runhaskell
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Werror -W -fno-warn-missing-methods #-}

module Main where

import           Control.Lens hiding (elements)
import           Data.List (intercalate, nub, sort, transpose)
import           Data.Maybe (fromJust, isJust, mapMaybe)
import           GHC.Exts (IsList (..))
import           Numeric.Natural (Natural)
import           Polysemy
import           Polysemy.State
import           Prelude hiding (log)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (QC)
import           Yabai

-- Types must come first, since they're needed by lens's TemplateHaskell

type ForAll a = forall prop. Testable prop => (a -> prop) -> Property

-- Random numbers for use when making decisions
type Seed = Natural

-- We don't want redundancy in our test state, so we try to calculate each field
-- of the required 'DisplayInfo', 'SpaceInfo' and 'WindowInfo' queries from a
-- single source of truth.

-- | A non-empty zipper; that is, a non-empty list with one element "focused"
data NEZipper a = Z [a] a [a] deriving (Eq)

-- | A 'Display' in our test state is a non-empty zipper of 'Space's, where the
--   "focus" is the visible 'Space'
type TestDisplay = NEZipper TestSpace

-- | A pure window manager state, for testing queries and commands
data WMState = State {
    -- The top-level list's elements represent a 'Display', with one of them
    -- focused. The inner lists' elements represent a 'Space', with one of them
    -- visible. The visible 'Space' on the focused 'Display' is focused.
    _stateDisplays :: NEZipper TestDisplay

    -- These can be used whenever an otherwise-pure operation needs to make a
    -- decision
  , _stateSeeds  :: InfiniteList Seed

    -- Maintain a log, so we can give more informative errors on test failure
  , _stateLog :: [String]
  }

-- | A 'Space' in our test state is either a non-empty zipper of 'Window's (with
--   the "focused" element being the focused 'Window'), or 'Nothing' if there is
--   no 'Window' on the 'Space'.
data TestSpace = TS { _testLabel   :: Maybe SpaceLabel
                    , _testWindows :: Maybe (NEZipper Window)
                    } deriving (Eq)

instance Show TestSpace where
  show ts = let f x     = [show x]
                label   = maybe [] f (_testLabel   ts)
                windows = maybe [] f (_testWindows ts)
             in "(" ++ intercalate "," (label ++ windows) ++ ")"

emptySpace = TS { _testLabel = Nothing, _testWindows = Nothing }

-- Basic instances (except for Arbitrary)

-- | Don't try to show an 'InfiniteList'
instance Show WMState where
  show s = case _stateLog s of
    [] -> show (_stateDisplays s)
    l  -> show (_stateDisplays s, l)

-- | We don't care about the remaining random seeds
instance Eq WMState where
  s1 == s2 = _stateDisplays s1 == _stateDisplays s2

-- | Use '[...]' syntax, but highlight the focused element with '{...}', e.g.
--   'Z [3, 2, 1] 4 [5, 6, 7]' should give '[1, 2, 3, {4}, 5, 6, 7]'
instance Show a => Show (NEZipper a) where
  show (Z xs y zs) =
    let wrap pre post s = pre ++ s ++ post
     in wrap "[" "]" . intercalate ", " $
          wrap (map show (reverse xs))
               (map show          zs)
               (pure . wrap "{" "}" . show $ y)

instance Functor NEZipper where
  fmap f (Z xs y zs) = Z (map f xs) (f y) (map f zs)

instance FunctorWithIndex Natural NEZipper where
  imap f (Z xs y zs) = Z (go (subtract 1) (l-1) xs)
                         (f                l    y )
                         (go        (+ 1) (l+1) zs)
    where l = len xs

          go _   _ []     = []
          go inc p (q:qs) = f p q : go inc (inc p) qs

instance Foldable NEZipper where
  foldMap f (Z xs y zs) = foldMap f (reverse xs) <> f y <> foldMap f zs

instance FoldableWithIndex Natural NEZipper where
  ifoldMap f z = case imap f z of
                  Z xs y zs -> mconcat (reverse xs) <> y <> mconcat zs

instance Traversable NEZipper where
  -- | Reverse 'xs' before traversing it, to ensure effects accumulate in the
  --   right order, then reverse the result back to how it started.
  traverse f (Z xs y zs) = Z <$> (reverse <$> traverse f (reverse xs))
                             <*> f y
                             <*> traverse f zs

instance IsList (NEZipper a) where
  type Item (NEZipper a) = a

  fromList []     = error "NEZipper can't be empty"
  fromList (y:zs) = Z [] y zs

  toList (Z xs y zs) = reverse xs ++ [y] ++ zs

-- Simple random number effect
data Random m a where
  Random :: Random m Seed

-- Template Haskell: definitions above here cannot depend on anything below

makeLenses ''WMState
makeLenses ''TestSpace
makeSem    ''Random

-- Optics for getting and setting inside our state

focused :: Lens' (NEZipper a) a
focused inj (Z xs y zs) = f <$> inj y
  where f y' = Z xs y' zs

_next :: Lens' (NEZipper a) (NEZipper a)
_next inj z = prevZ <$> inj (nextZ z)

_currentDisplay :: Lens' WMState TestDisplay
_currentDisplay = stateDisplays . focused

_currentSpace :: Lens' WMState TestSpace
_currentSpace = _currentDisplay . focused

_otherDisplay :: Lens' WMState TestDisplay
_otherDisplay = stateDisplays . _next . focused

_otherSpace :: Lens' WMState TestSpace
_otherSpace = _otherDisplay . focused

displayIndex :: WMState -> Display
displayIndex = DID . focusedI . _stateDisplays

stateDisplayInfos :: WMState -> [DisplayInfo]
stateDisplayInfos = go 0 0 . toList . _stateDisplays
  where go _  _  []     = []
        go di si (d:ds) = let l = si + len d
                           in DI { dDisplay = DID di
                                 , dSpaces  = take (len d) [si..]
                                 } : go (di+1) l ds

-- Helper functions

debug :: (Show a, Testable prop) => a -> prop -> Property
debug x = counterexample (show x) . property

-- | Like 'xs !! n' but returns Maybe
safeIndex :: [a] -> Natural -> Maybe a
safeIndex = go
  where go []     _ = Nothing
        go (x:_ ) 0 = Just x
        go (_:xs) n = go xs (n-1)

nthMod :: [a] -> Natural -> a
nthMod [] _ = err "Can't nthMod empty list"
nthMod xs n = xs !! fromIntegral (n `mod` len xs)

-- | Like 'elemIndex xs x' but returns Mayube
safePosition :: Eq a => a -> [a] -> Maybe Natural
safePosition x = go 0
  where go _ []     = Nothing
        go n (y:ys) = if x == y
                         then Just n
                         else go (n+1) ys

movable :: WMState -> SpaceIndex -> Bool
movable s i = (> 1)                .
              length               .
              first' () (i `elem`) .
              map dSpaces          .
              stateDisplayInfos    $ s

hasMovableSpace :: WMState -> Bool
hasMovableSpace = not . null . spaciousDisplays

spaciousDisplays :: WMState -> [DisplayInfo]
spaciousDisplays = filter ((> 1) . len . dSpaces) . stateDisplayInfos


allLabelled :: WMState -> Bool
allLabelled = all (isJust . sLabel) . testStateSpaces

twoDisplays :: WMState -> Bool
twoDisplays = (== 2) . length . stateDisplayInfos

testStateWindows :: WMState -> [WindowInfo]
testStateWindows state = map windowInfo windows

  where focusedDisplays :: [(Focused, TestDisplay)]
        focusedDisplays = case _stateDisplays state of
          Z xs y zs -> map (unFoc,  ) (reverse xs) ++
                       [   (isFoc, y)            ] ++
                       map (unFoc,  )          zs

        numberedDisplays :: [(Display, Focused, TestDisplay)]
        numberedDisplays = zipWith (\(foc, d) n -> (DID n, foc, d))
                                   focusedDisplays
                                   [0..]

        visibleSpaces :: [(Display, Focused, Visible, TestSpace)]
        visibleSpaces = concatMap
          (\(d, f, Z xs y zs) ->
            let notVis s = (d, f, invis, s)
             in map notVis (reverse xs) ++
                [(d, f, isVis, y)]      ++
                map notVis          zs)
          numberedDisplays

        numberedSpaces :: [(SpaceIndex, Display, Focused, Visible, TestSpace)]
        numberedSpaces = zipWith (\(d, f, v, ts) n -> (SIndex n, d, f, v, ts))
                                 visibleSpaces
                                 [0..]

        windows :: [(SpaceIndex, Display, Focused, Visible, Window)]
        windows = concatMap
          (\(si, d, f, v, ts) -> case _testWindows ts of
            Nothing          -> []
            Just (Z xs y zs) ->
              let notVis w = (si, d, unFoc, v, w)
               in map notVis (reverse xs) ++
                  [(si, d, f, v, y)]      ++
                  map notVis          zs)
          numberedSpaces

        windowInfo :: (SpaceIndex, Display, Focused, Visible, Window)
                   -> WindowInfo
        windowInfo (si, d, f, v, w) = WI {
          wWindow  = w
        , wDisplay = d
        , wSpace   = si
        , wVisible = v
        , wFocused = f
        }

testSpaces :: SpaceIndex -> Display -> Focused -> TestDisplay -> [SpaceInfo]
testSpaces (SIndex i) d focused td = case td of
    Z xs y zs -> let i' = i + fromIntegral (length xs)
                  in zipWith (space invis) [i   ..] (reverse xs) ++
                     [        space isVis   i'               y ] ++
                     zipWith (space invis) [i'+1..]          zs
  where space visible index ts = SI {
            sLabel   = _testLabel ts
          , sIndex   = SIndex index
          , sDisplay = d
          , sWindows = maybe [] toList (_testWindows ts)
          , sVisible = visible
          , sFocused = F (unV visible && unF focused)
          }

-- | Convert the TestSpace entries in the given WMState into Yabai's SpaceInfo
--   format, filling in indices, displays, visibility and focus from the zipper
--   positions.
testStateSpaces :: WMState -> [SpaceInfo]
testStateSpaces s = case _stateDisplays s of
    Z xs y zs -> let l :: [[(Focused, TestDisplay)]]
                     l = [ map (unFoc,) (reverse xs)
                         , [   (isFoc,           y)]
                         , map (unFoc,)          zs
                         ]
                  in go 0 0 (concat l)
  where go di si ds = case ds of
          []               -> []
          (foc, d):ds' -> let spaces = testSpaces si (DID di) foc d
                           in spaces ++ go (di + 1)
                                           (si + len spaces)
                                           ds'

-- | Look up the SpaceInfo for the given SpaceIndex in the given WMState. This
--   will trigger an error if no such index exists; in which case the first
--   argument will be used as the error message.
testStateSpace :: Show a => a -> SpaceIndex -> WMState -> SpaceInfo
testStateSpace info i = first' info ((== i) . sIndex) . testStateSpaces

testStateCurrentSpace :: WMState -> SpaceInfo
testStateCurrentSpace s = first' ( ("error", "No focused space")
                                 )
                                 (unF . sFocused)
                                 (testStateSpaces s)

imapSpaces :: (SpaceIndex -> TestSpace -> TestSpace)
           -> NEZipper TestDisplay
           -> NEZipper TestDisplay
imapSpaces f (Z xs y zs) = Z (reverse xs') y' zs'
  where xi , yi  :: SpaceIndex
        xs', zs' :: [TestDisplay]
        y'       ::  TestDisplay

        (xi, xs') = reduce [] 0  (reverse xs)
        (yi, y' ) =     go    xi          y
        (_ , zs') = reduce [] yi          zs

        go :: SpaceIndex -> TestDisplay -> (SpaceIndex, TestDisplay)
        go i d@(Z sxs sy szs) =
          let ilen                        = i + len d
              (xIndices, yIndex:zIndices) = splitAt (len sxs) [i..ilen]
           in ( ilen
              , Z (zipWith f (reverse xIndices) sxs)
                  (        f          yIndex    sy )
                  (zipWith f          zIndices  szs)
              )

        reduce :: [TestDisplay]
               -> SpaceIndex
               -> [TestDisplay]
               -> (SpaceIndex, [TestDisplay])
        reduce !acc !i []     = (i, reverse acc)
        reduce !acc !i (d:ds) = let (i', d') = go i d
                                 in reduce (d':acc) i' ds

testWindowInfos :: Display -> SpaceIndex -> Visible -> Focused -> [WindowInfo]
testWindowInfos _ _ _ _ {--d s vis foc-} = error "UNDEFINED testWindowInfos"

spaceIndices :: [DisplayInfo] -> [SpaceIndex]
spaceIndices = concatMap dSpaces

incZ :: NEZipper a -> Maybe (NEZipper a)
incZ (Z xs y (z:zs)) = Just (Z (y:xs) z zs)
incZ _               = Nothing

decZ :: NEZipper a -> Maybe (NEZipper a)
decZ (Z (x:xs) y zs) = Just (Z xs x (y:zs))
decZ _               = Nothing

focusedI :: Num n => NEZipper a -> n
focusedI (Z xs _ _) = len xs

firstZ :: NEZipper a -> NEZipper a
firstZ z = case decZ z of
  Nothing -> z
  Just z' -> firstZ z'

lastZ :: NEZipper a -> NEZipper a
lastZ z = case incZ z of
  Nothing -> z
  Just z' -> lastZ z'

nextZ :: NEZipper a -> NEZipper a
nextZ z = case incZ z of
  Nothing -> firstZ z
  Just z' -> z'

prevZ :: NEZipper a -> NEZipper a
prevZ z = case decZ z of
  Nothing -> lastZ z
  Just z' -> z'

-- | Insert the given value into the given zipper. Insert at the given position,
--   modulo the length of the zipper.
insertZ :: Natural -> a -> NEZipper a -> NEZipper a
insertZ pos x z@(Z xs y zs) = Z xs' y' zs'
  where i   = pos `mod` len z
        xs' = case compare i (len xs) of
                LT -> let (pre, post) = splitAt' i (reverse xs)
                       in reverse (pre ++ [x] ++ post)
                EQ -> y:xs
                GT -> xs

        y'  = if i == len xs
                 then x
                 else y

        zs' = if i > len xs
                 then let (pre, post) = splitAt' (i - len xs) zs
                       in pre ++ [x] ++ post
                 else zs

splitAt' :: Integral a => a -> [b] -> ([b], [b])
splitAt' n = splitAt (fromIntegral n)

-- | Pop the focused element off the given NEZipper. Fails if it's the only
--   element. The given Bool determines which direction the zipper will shift if
--   both are possible.
popZ :: NEZipper a -> Bool -> Maybe (a, NEZipper a)
popZ z dir = case z of
  Z    []  _    []  -> Nothing
  Z (x:xs) y    []  -> Just (y, Z xs x [])
  Z    []  y (z:zs) -> Just (y, Z [] z zs)
  Z (x:xs) y (z:zs) -> Just (y, if dir
                                   then Z    xs  x (z:zs)
                                   else Z (x:xs) z    zs)

headZ :: NEZipper a -> a
headZ (Z _ y _) = y

-- | Focus the nth item in the given zipper (if it's that big)
shiftZ :: Natural -> NEZipper a -> Maybe (NEZipper a)
shiftZ n = go n . firstZ
  where go 0 z = pure z
        go m z = incZ z >>= go (m-1)

shiftUntil :: Show a => (NEZipper a -> Bool) -> NEZipper a -> NEZipper a
shiftUntil f = go 0
  where go n z | n > length z = err ( ("error" , "Unsatisfiable predicate")
                                    , ("source", "shiftUntil"             )
                                    , ("zipper", z                        )
                                    , ("tries" , n                        )
                                    )
               | f z          = z
               | otherwise    = go (n+1) (nextZ z)

focusOnDisplay :: Display -> WMState -> WMState
focusOnDisplay (DID d) = over stateDisplays (shiftUntil foc)
  where foc = (== d) . focusedI

-- | Shift the focused display and its focused space until the given SpaceIndex
--   is in focus. Will throw an (informative) error if the given SpaceIndex
--   doesn't exist.
focusOnSpace :: SpaceIndex -> WMState -> WMState
focusOnSpace i state = over (stateDisplays . focused)
                            (shiftUntil correctPos)
                            (focusOnDisplay d state)
  where d = sDisplay (testStateSpace ( ("error" , "Can't focus on space")
                                     , ("source", "focusOnSpace"        )
                                     , ("index" , i                     )
                                     , ("state" , state                 )
                                     )
                                     i
                                     state)

        correctPos = (== localIndex i state) . focusedI

-- | The index of the given Space on its Display, i.e. subtracting the indices
--   on all prior Displays.
localIndex :: SpaceIndex -> WMState -> Natural
localIndex i state = unSIndex i - prev
  where displays  = stateDisplayInfos state
        spaces    = map dSpaces displays
        preSpaces = takeWhile (i `notElem`) spaces
        prev      = sum (map len preSpaces)

-- Pure, in-memory implementation of Yabai queries and commands

queryState :: Member (State WMState) r => Sem (Query ': r) a -> Sem r a
queryState = interpret query
  where query :: Member (State WMState) r => Query m a -> Sem r a
        query q = case q of
          GetDisplays   -> stateDisplayInfos   <$> get
          GetSpaces     -> testStateSpaces     <$> get
          GetWindows    -> testStateWindows    <$> get

logState :: Member (State WMState) r => Sem (Logger ': r) a -> Sem r a
logState = interpret logger
  where logger :: Member (State WMState) r => Logger m a -> Sem r a
        logger (Log' s) = modify (over stateLog (++ [s]))

randomState :: Member (State WMState) r => Sem (Random ': r) a -> Sem r a
randomState = interpret randomise
  where randomise :: Member (State WMState) r => Random m a -> Sem r a
        randomise Random = do
          seeds <- gets _stateSeeds
          let x:xs = getInfiniteList seeds
          modify (set stateSeeds (seeds { getInfiniteList = xs }))
          pure x

commandState :: ( Member Query           r
                , Member Logger          r
                , Member Random          r
                , Member (State WMState) r
                )
             => Sem (Command ': r) a
             -> Sem r a
commandState = interpret command
  where command :: ( Member Logger          r
                   , Member Query           r
                   , Member Random          r
                   , Member (State WMState) r
                   )
                => Command m a
                -> Sem r a
        command c = case c of
          CreateSpace -> do
            seed <- random
            let addSpace (Z xs y zs) = case seed `mod` 4 of
                  0 -> Z (emptySpace:xs) y          zs
                  1 -> Z (         y:xs) emptySpace zs
                  2 -> Z xs              y          (emptySpace:zs)
                  3 -> Z xs              emptySpace (         y:zs)
                  n -> err ( ("source" , "commandState interpreter")
                           , ("branch" , "CreateSpace")
                           , ("error"  , "Random number outside expected range")
                           , ("got"    , n)
                           , ("modulus", 4)
                           )

            modify (over _currentDisplay addSpace)
            pure True

          DestroySpace   -> do seed <- random
                               modify' (destroyCurrentSpace (even seed))

          LabelSpace i l -> labelSpace' i l

          FocusWindow  w -> modify' (focusWindow  w)

          FocusSpace   s -> case s of
            Left  i -> do focusSpace' i
            Right l -> do
              mi <- indexOfSpace l
              case mi of
                Nothing -> pure False
                Just i  -> focusSpace' i

          FocusDisplay foc -> case foc of
            Left  s  -> do modify (over stateDisplays (case s of
                                                        Previous -> prevZ
                                                        Next     -> nextZ
                                                        First    -> firstZ
                                                        Last     -> lastZ))
                           pure True
            Right d -> do modify' (focusDisplay' d)

          MoveWindow w s -> error "MoveWindow" w s{-concat [ ["window", "--move"]
                                    , maybe [] ((:[]) . show) w
                                    , ["--space", show s]
                                    ]-}
          SwapWindow s   -> error "SwapWindow" s{-["window", "--swap"   , show s]-}

          MoveSpace  d   -> moveSpace' d

        modify' :: Member (State WMState) r
                => (WMState -> Maybe WMState)
                -> Sem r Bool
        modify' f = do
          state <- get
          case f state of
            Nothing     -> pure False
            Just state' -> put state' >> pure True

        labelSpace' :: (Member (State WMState) r)
                    => SpaceIndex
                    -> SpaceLabel
                    -> Sem r Bool
        labelSpace' i l@(SLabel ls) =
          let go n s = if n == i
                          then s { _testLabel = mkLabel ls }
                          else s
           in do modify (over stateDisplays (imapSpaces go))
                 s <- get
                 pure . any ((== Just l) . sLabel) . testStateSpaces $ s

        focusWindow = error "focusWindow"

        focusSpace' :: (Member Query r, Member (State WMState) r)
                    => SpaceIndex
                    -> Sem r Bool
        focusSpace' i = do
          md <- displayOfSpace (indexS i)
          case md of
            Nothing -> err ( ("warning", "Couldn't find space's display")
                           , ("source" , "focusSpace'"                  )
                           , ("space"  , i                              )
                           )
            Just d  -> do
              s <- get
              case focusDisplay' d s of
                Nothing -> err "Didn't focus display of space"
                Just s  -> do
                  put s
                  let Z dxs dy _ = _stateDisplays s

                      xLen :: SpaceIndex
                      xLen = sum (map len dxs)

                      go :: (Member (State WMState) r)
                         => NEZipper a
                         -> Sem r (NEZipper a)
                      go z = case compare i (xLen + focusedI z) of
                               EQ -> pure z
                               LT -> maybe (err "Decrement failed") go (decZ z)
                               GT -> maybe (err "Increment failed") go (incZ z)

                  dy' <- go dy
                  modify (set (stateDisplays . focused) dy')
                  pure True

        focusDisplay' :: Display -> WMState -> Maybe WMState
        focusDisplay' (DID d) s =
          let go z = case compare d (focusedI z) of
                       EQ -> Just z
                       LT -> decZ z >>= go
                       GT -> incZ z >>= go

           in (\ds -> set stateDisplays ds s) <$> go (_stateDisplays s)

        moveSpace' :: ( Member Logger          r
                      , Member Query           r
                      , Member Random          r
                      , Member (State WMState) r
                      )
                   => Display
                   -> Sem r Bool
        moveSpace' d = do
          dir   <- even <$> random
          state <- get
          let oldDs         = _stateDisplays state
              oldD          = headZ oldDs
          case popZ oldD dir of
            Nothing -> if focusedI oldDs == unDID d
                          then do log ("Already on Display", d)
                                  pure True
                          else do log "Can't move lone space"
                                  pure False

            Just (space, newD) -> do
              let poppedDs = case oldDs of
                    Z xs _ zs -> Z xs newD zs

              case shiftZ (unDID d) poppedDs of
                Nothing  -> do log ( "New display doesn't exist"
                                   , ("display", d)
                                   )
                               pure False
                Just (Z xs y zs) -> do
                  pos <- random
                  case shiftZ (focusedI oldDs)
                              (Z xs (insertZ pos space y) zs) of
                    Nothing  -> err ()
                    Just ds' -> do
                      modify (set stateDisplays ds')
                      pure True

destroyCurrentSpace :: Bool -> WMState -> Maybe WMState
destroyCurrentSpace choice s = do
    dy' <- dy
    pure (s { _stateDisplays = Z dxs dy' dzs })

  where Z dxs (Z sxs _ szs) dzs = _stateDisplays s

        dy = case (sxs, szs) of
          ([]  , []  ) -> Nothing
          (x:xs, []  ) -> Just (Z  xs x szs)
          ([]  , z:zs) -> Just (Z sxs z  zs)
          (x:xs, z:zs) -> Just (if choice
                                   then Z  xs x szs
                                   else Z sxs z  zs)

-- | Label every Space in the given WMState with its SpaceIndex. This is useful
--   for keeping track of where spaces end up after some rearrangement.
labelWithIndices :: WMState -> WMState
labelWithIndices s = over stateDisplays (imapSpaces f) s
  where f i space = space { _testLabel = mkLabel ('l' : show i) }

replaceElem :: [a] -> a -> Natural -> [a]
replaceElem [] _ _ = error "replaceElem: given empty list"
replaceElem xs x n = take i xs ++ [x] ++ drop (i+1) xs
  where i = fromIntegral n `mod` length xs

nth :: NEZipper a -> Natural -> a
nth z n = l !! i
  where l = toList z
        i = fromIntegral n `mod` length l

uniq l = length l == length (nub l)

-- Generators

instance Arbitrary Natural where
  arbitrary = fromIntegral . abs <$> arbitrary @Int
  shrink n  = fromIntegral . abs <$> (shrink @Int (fromIntegral n))

instance Arbitrary Window where
  arbitrary = WID <$> arbitrary
  shrink (WID n) = map WID (shrink n)

instance Arbitrary Display where
  arbitrary      = DID <$> arbitrary
  shrink (DID d) = map DID (shrink d)

instance Arbitrary SpaceLabel where
  arbitrary = arbitrary `suchThatMap` mkLabel
  shrink (SLabel s) = mapMaybe mkLabel (shrink s)

instance Arbitrary a => Arbitrary (NEZipper a) where
  arbitrary = Z <$> arbitrary <*> arbitrary <*> arbitrary

  shrink (Z xs y zs) = map (\(xs, y, zs) -> Z xs y zs)
                           (shrink (xs, y, zs))

instance Arbitrary TestSpace where
  arbitrary = TS <$> arbitrary
                 <*> arbitrary `suchThat` maybe True (uniq . toList)

  shrink ts =    map (\(l, mws) -> TS { _testLabel = l, _testWindows = mws }) .
              filter (\(_, mws) -> maybe True uniq (toList <$> mws))          .
              shrink $ (_testLabel ts, _testWindows ts)

instance Arbitrary WMState where
  arbitrary = let genD :: Natural -> Gen TestDisplay
                  genD = genNEZipper genS

                  genS :: Natural -> Gen TestSpace
                  genS n = do label <- arbitrary
                              ws    <- case n of
                                0 -> pure Nothing
                                _ -> Just <$> genNEZipper (const arbitrary) n
                              pure TS {
                                _testLabel   = label
                              , _testWindows = ws
                              }

                  -- | Renumber any 'Window' that we've seen before
                  uniqueDisplaysWindows :: [Natural]
                                        -> NEZipper TestDisplay
                                        -> NEZipper TestDisplay
                  uniqueDisplaysWindows seen =
                    snd . uniqueZipper uniqueDisplayWindows seen

                  uniqueZipper :: ([Natural] -> a -> ([Natural], a))
                               -> [Natural]
                               -> NEZipper a
                               -> ([Natural], NEZipper a)
                  uniqueZipper f seen (Z xs y zs) =
                    let (seen'  , xs') = uniqueList f seen  xs
                        (seen'' , zs') = uniqueList f seen' zs
                        (seen''', y' ) = f seen'' y
                     in (seen''', Z xs' y' zs')

                  uniqueList _ seen []     = (seen, [])
                  uniqueList f seen (x:xs) =
                    let (seen' , x' ) = f seen x
                        (seen'', xs') = uniqueList f seen' xs
                     in (seen'', x':xs')

                  uniqueDisplayWindows :: [Natural]
                                       -> TestDisplay
                                       -> ([Natural], TestDisplay)
                  uniqueDisplayWindows = uniqueZipper uniqueSpaceWindows

                  uniqueSpaceWindows :: [Natural]
                                     -> TestSpace
                                     -> ([Natural], TestSpace)
                  uniqueSpaceWindows seen s = case _testWindows s of
                    Nothing -> (seen, s)
                    Just ws -> case uniqueZipper uniqueWindow seen ws of
                      (seen', ws') -> (seen', s { _testWindows = Just ws' })

                  uniqueWindow :: [Natural] -> Window -> ([Natural], Window)
                  uniqueWindow seen (WID w) =
                    if w `elem` seen
                       then uniqueWindow seen (WID (w+1))
                       else (w:seen, WID w)

               in do seeds <- arbitrary
                     size  <- getSize
                     ds    <- genNEZipper genD (fromIntegral (abs size))
                     pure (State { _stateDisplays = uniqueDisplaysWindows [] ds
                                 , _stateSeeds    = seeds
                                 , _stateLog      = []
                                 })

  shrink s =
    let unlabelled = over stateDisplays (fmap unlabel) s
        unlabel = fmap (\space -> space { _testLabel = Nothing })
     in map (\ds -> s { _stateDisplays = ds })
            (shrink (_stateDisplays s)) ++
                 if unlabelled == s then [] else [unlabelled]

-- | Like 'choose (min, max)' but for 'Natural'
chooseNat :: (Natural, Natural) -> Gen Natural
chooseNat (min, max) = fromIntegral <$> choose @Int ( fromIntegral min
                                                    , fromIntegral max
                                                    )

-- | Generate a list of labels to emulate our desired XMonad-like setup
genSpaceLabels :: Gen [SpaceLabel]
genSpaceLabels = do n <- choose (5, 15)
                    vectorOf n arbitrary `suchThat` uniq

genIndexFrom :: WMState -> Gen SpaceIndex
genIndexFrom = fmap sIndex . genSpaceInfoOf

genSpaceInfoOf :: WMState -> Gen SpaceInfo
genSpaceInfoOf = Test.QuickCheck.elements . testStateSpaces

-- | Generate a 'WMState' with the given 'Display' count. We do this by
--   generating an 'arbitrary' 'WMState' with at least that 'Display' count,
--   then discarding down to the required count.
genWMState :: Natural -> Gen WMState
genWMState dCount = do
    s     <- arbitrary `suchThat` (enough . _stateDisplays)
    ds    <- dropDisplays (_stateDisplays s)
    seeds <- arbitrary
    pure (State { _stateDisplays = ds
                , _stateSeeds    = seeds
                , _stateLog      = []
                })

  where countDisplays = fromIntegral . length . toList

        enough  = (>= dCount) . countDisplays

        tooMany = (>  dCount) . countDisplays

        dropDisplays ds@(Z xs y zs) =
          if tooMany ds
             then do fromStart <- arbitrary
                     if fromStart
                        then dropDisplays (Z (drop 1 xs) y         zs )
                        else dropDisplays (Z         xs  y (drop 1 zs))
             else pure ds

genWMState' = fmap labelWithIndices . genWMState

genLabelled    = genWMState' 2 `suchThat` hasMovableSpace
shrinkLabelled = map labelWithIndices .
                 filter ((hasMovableSpace &&& twoDisplays) &&& allLabelled) .
                 shrink

-- | Use the given generator to make a value. The given number is the amount of
--   "fuel" to use; always returns 'Nothing' when there's no fuel.
genMaybe :: (Natural -> Gen a) -> Natural -> Gen (Maybe a)
genMaybe f n = frequency [ (1             , pure Nothing    )
                         , (fromIntegral n, Just <$> f (n-1))
                         ]

-- | Use the given generator to make the elements of a list. The given number is
--   "fuel", which will be divided up randomly between the generators of the
--   elements.
genList :: (Natural -> Gen a) -> Natural -> Gen [a]
genList _ 0 = pure []
genList g n = do n' <- chooseNat (1, n)
                 x  <- g n'
                 (x:) <$> genList g (n - n')

-- | Use the given generator to make the elements of an 'NEZipper'. The given
--   number is "fuel", which will be divided up randomly between the generators
--   of the elements.
genNEZipper :: (Natural -> Gen a) -> Natural -> Gen (NEZipper a)
genNEZipper g 0 = (\y -> Z [] y []) <$> g 0
genNEZipper g n = do yn <- case n of
                             0 -> pure 0
                             m -> chooseNat (1, m)
                     xn <- case n - yn of
                             0 -> pure 0
                             m -> chooseNat (1, m)
                     zn <- case n - yn - xn of
                             0 -> pure 0
                             m -> chooseNat (1, m)

                     xs <- genList g xn
                     y  <-         g yn
                     zs <- genList g zn
                     pure (Z xs y zs)

-- Tests

type TestEffects a =
  Sem (Command ': Logger ': Random ': Query ': State WMState ': '[]) a

forAllDisplays :: ForAll [DisplayInfo]
forAllDisplays = forAllStateful stateDisplayInfos

forAllSpaces :: ForAll [SpaceInfo]
forAllSpaces = forAllStateful testStateSpaces

forAllDisplaySpaces :: ForAll ([DisplayInfo], [SpaceInfo])
forAllDisplaySpaces = forAllStateful
  (\s -> (stateDisplayInfos s, testStateSpaces s))

forAllWindows :: ForAll [WindowInfo]
forAllWindows = forAllStateful testStateWindows

-- | If our test inputs are derived from a 'WMState', we can generate
--   and shrink the state, then re-derive the test inputs.
forAllFromState :: Show a => (WMState -> a) -> ForAll (WMState, a)
forAllFromState f = forAllShrink (do s <- arbitrary
                                     pure (s, f s))
                                 (\(s, _) -> map (\s -> (s, f s))
                                                 (shrink s))

-- | Generates and shrinks a 'WMState', but applies the given function
--   to it to derive the property's input.
forAllStateful :: Show a => (WMState -> a) -> ForAll a
forAllStateful f prop = forAllFromState f (prop . snd)

-- | Test a query by running it on an arbitrary WMState
forAllQ :: Sem (Query ': State WMState ': '[]) a
        -> ForAll (WMState, WMState, a)
forAllQ q f = forAllShrink arbitrary shrink
  (\init -> let (final, result) = run (runState init (queryState q))
             in f (init, final, result))

-- | Test an arbitrary mixture of queries and commands. We generate and shrink
--   pairs of WMState and any other type, which is passed as an argument to the
--   given command. This can be () for commands which don't need an argument,
--   but is useful e.g. if we need a SpaceIndex consistent with the WMState.
forAllQC :: (Show a)
         => Gen (WMState, a)
         -> ((WMState, a) -> [(WMState, a)])
         -> (a -> TestEffects b)
         -> ForAll (WMState, WMState, a, b)
forAllQC gen shr qc f = forAllShrink gen shr
  (\(init, x) -> let (final, result) = runCmd init (qc x)
                  in debug ("final", final)
                           (f (init, final, x, result)))

-- | Interpreter for all of our test effects
runCmd :: WMState -> TestEffects a -> (WMState, a)
runCmd init = run           .
              runState init .
              queryState    .
              randomState   .
              logState      .
              commandState

-- Test helper functions

prop_imapZippersCountFromZero z = want === got
  where f n () = n

        mapped :: NEZipper Natural
        mapped = imap f z

        want, got :: [Natural]
        want   = [0..(len z - 1)]
        got    = toList mapped

prop_imapZippersPreserveOrder z = mapped === z
  where f _ x = x

        mapped :: NEZipper Int
        mapped = imap f z

prop_imapSpacesCountsUpwards s = debug ( ("displays", displays)
                                       , ("mapped"  , mapped  )
                                       )
                                       (got === want)
  where displays = _stateDisplays s
        mapped   = imapSpaces (\i space -> space {
                                _testLabel = Just (SLabel (show i))
                              })
                              displays
        got      = map _testLabel . concatMap toList . toList $ mapped
        want     = take (len got) (map (Just . SLabel . show) [0..])

-- Test stateful interpreter

prop_GeneratedDisplaysCountUpwards = forAllDisplays (\dis ->
  let got  = map dDisplay             dis
      want = map (DID . fromIntegral) [0..(length dis)-1]
   in got === want)

prop_GeneratedDisplaysHaveAtLeastOneSpaceEach = forAllDisplays
  (all (not . null . dSpaces))

prop_GeneratedDisplaysHaveSpaceIndicesCountingUp = forAllDisplays (\dis ->
  let got  = concatMap dSpaces dis
      want = map SIndex (take (length got) [0..])
   in got === want)

prop_GenerateCorrectNumberOfSpaces = forAllDisplaySpaces (\(dis, sis) ->
  let want = sum (map (length . dSpaces) dis)
   in length sis === want)

prop_GenerateCorrectSpaceIndices = forAllDisplaySpaces (\(dis, sis) ->
  let want = concatMap dSpaces dis
      got  = map sIndex sis
   in got === want)

prop_SpacesHaveCorrectDisplays = forAllDisplaySpaces (\(dis, sis) ->
  let onDisplay di si = sIndex si `elem` dSpaces di

      checkDisplay di =
        let spaces  = filter (onDisplay di) sis
            did     = dDisplay di
            check s =
              let sd = sDisplay s
               in debug ( ("Space display", sd    )
                        , ("Display"      , did   )
                        , ("DisplayInfo"  , di    )
                        , ("Space"        , s     )
                        , ("Spaces"       , spaces)
                        )
                        (sd === did)
         in conjoin (map check spaces)

   in conjoin (map checkDisplay dis))

prop_SpaceIndicesCountUp = forAllSpaces (\sis ->
  let got  =                    map sIndex sis
      want = take (length got) (map SIndex [0..])
   in got === want)

prop_OneVisibleSpacePerDisplay = forAllSpaces (\sis ->
  let vis  = filter (unV . sVisible) sis
      got  = map    sDisplay vis
      want = take (len got) (map DID [0..])
   in got === want)

prop_OneFocusedSpace = forAllSpaces (\sis ->
  let foc = filter (unF . sFocused) sis
   in length foc === 1)

prop_WindowsAreUnique = forAllWindows (uniq . map wWindow)

-- Test primitive queries

prop_CanGetDisplays = forAllQ (getDisplays :: Q r [DisplayInfo])
  (\(i, s, displays) -> i === s .&&. not (null displays))

prop_CanGetSpaces   = forAllQ (getSpaces   :: Q r [SpaceInfo  ])
  (\(i, s, spaces  ) -> i === s .&&. not (null spaces  ))

prop_CanGetWindows  = forAllQ (getWindows  :: Q r [WindowInfo ])
  (\(i, s, windows ) -> i === s .&&. length windows >= 0)

-- Test compound queries

prop_displayCountMatchesState = forAllQ (displayCount :: Q r Natural)
  (\(_, s, dCount) -> dCount === (fromIntegral . length . _stateDisplays $ s))

prop_pluggedInCountsDisplays = forAllQ pluggedIn (\(_, s, p) ->
  let len = length (_stateDisplays s)
   in debug ("len", len, "p", p)
            ((len == 2) === p))

prop_emptySpacesHaveNoWindows = forAllQ emptySpaces (\(_, s, empty) ->
  let spaceInfos   = testStateSpaces s
      emptyInfos   = filter ((`elem` empty) . sIndex) spaceInfos
      noWindows si = debug ( ("SpaceInfo"    , si   )
                           , ("Empty indices", empty)
                           )
                           (null (sWindows si))
   in conjoin (map noWindows emptyInfos))

prop_destroyableSpaceHasSiblingsOnItsDisplay = forAllQ destroyableSpace
  (\(_, s, mspace) ->
    isJust mspace ==>
      (let space  = fromJust mspace
           dis    = stateDisplayInfos s
           di     = first' ( ("error"   , "No display with given space")
                           , ("space"   , space                        )
                           , ("displays", dis                          )
                           , ("location", "sibling property")
                           )
                           ((space `elem`) . dSpaces)
                           dis
           spaces = dSpaces di
        in debug ( ("display", di    )
                 , ("spaces" , spaces)
                 , ("space"  , space )
                 )
                 (length spaces > 1)))

-- Test primitive commands

testDisplay = forAllQC
  (do s <- arbitrary
      d <- chooseNat (0, len (_stateDisplays s) - 1)
      pure (s, DID d))
  (\(s, DID d) -> map (\s' -> (s', DID (d `mod` len (_stateDisplays s))))
                      (shrink s))
  (focusDisplay . Right)

testSpaceIndex = forAllQC
  (do s <- arbitrary
      i <- elements (map sIndex (testStateSpaces s))
      pure (s, i))
  (\(s, SIndex i) -> map (\s' -> let sis = testStateSpaces s'
                                  in (s', SIndex (i `mod` len sis)))
                         (shrink s))
  (focusSpace . indexS)

testIndexAndLabel = forAllQC
  (do s <- arbitrary
      i <- genIndexFrom s
      l <- arbitrary
      pure (s, (i, l)))
  (\(s, (SIndex i, l)) ->
    let l' = head (shrink l ++ [l])
     in map (\s' -> (s', (SIndex (i `mod` len (testStateSpaces s')), l')))
            (shrink s))
  (uncurry labelSpace)

testMovableSpace = forAllQC
    (genFrom <$> ((chooseNat (2, 10) >>= genWMState) `suchThat` acceptable)
             <*> (SIndex <$> arbitrary)
             <*> (DID    <$> arbitrary))
    (\(state, newD) ->
      let i        = sIndex (testStateCurrentSpace state)
          f state' = genFrom state' i newD
       in map f . filter acceptable . shrink $ state)
    moveSpace

  where acceptable state = len (_stateDisplays state) > 1 &&
                           hasMovableSpace state

        genFrom state (SIndex sn) (DID dn) =
          let displays = spaciousDisplays state
              movable  = concatMap dSpaces displays
              i        = movable !! fromIntegral (sn `mod` len movable)
              si       = first' ( ("error" , "Couldn't find space")
                                , ("source", "genFrom"            )
                                , ("state" , state                )
                                , ("space" , i                    )
                                )
                                ((== i) . sIndex)
                                (testStateSpaces state)
              oldD     = sDisplay si
              newDs    = filter (/= oldD)  .
                         map dDisplay      .
                         stateDisplayInfos $ state
              newD     = newDs !! fromIntegral (dn `mod` len newDs)
           in (focusOnSpace i state, newD)

prop_FocusingDisplaySucceeds = testDisplay (\(_, final, d, result) ->
  debug ( ("display" , d                      )
        , ("displays", stateDisplayInfos final)
        )
        result)

prop_CanFocusOnDisplay = testDisplay (\(_, final, got, _) ->
  let spaces  = testStateSpaces final
      focused = first' ( ("error" , "No focused space"          )
                       , ("source", "Test: Can focus on display")
                       , ("spaces", spaces                      )
                       )
                       ((== isFoc) . sFocused)
                       spaces
      want    = sDisplay focused
   in debug ( ("want"    , want                   )
            , ("got"     , got                    )
            , ("displays", stateDisplayInfos final)
            , ("spaces"  , spaces                 )
            )
            (got === want))

prop_FocusingByIndexSucceeds = testSpaceIndex (\(_, final, i, result) ->
  debug ( ("index" , i                    )
        , ("spaces", testStateSpaces final)
        , ("log"   ,       _stateLog final)
        )
        result)

prop_CanFocusOnSpacesByIndex = testSpaceIndex (\(_, final, i, _) ->
  let spaces = testStateSpaces final
      space  = first' ( ("error" , "No Space with given SpaceIndex")
                      , ("index" , i                               )
                      , ("spaces", spaces                          )
                      )
                      ((== i) . sIndex)
                      spaces
   in sFocused space === isFoc)

prop_CanLabelSpaces = testIndexAndLabel (\(_, final, (i, want), _) ->
  let spaces  = testStateSpaces final
      space   = first' ( ("error" , "Space not found by index")
                       , ("spaces", spaces                    )
                       )
                       ((== i) . sIndex)
                       spaces
      got     = sLabel space
   in debug ( ("space" , space )
            , ("spaces", spaces)
            , ("index" , i     )
            , ("want"  , want  )
            , ("got"   , got   )
            )
            (got === Just want))

prop_CanMoveFocusedSpaceToDisplay = testMovableSpace (\(init, final, d, _) ->
  let si     = testStateCurrentSpace init
      space  = view (stateDisplays . focused . focused) init
      spaces = nth (_stateDisplays final) (unDID d)

   in debug ( ("debug" , "space should be in spaces")
            , ("info"  , si                         )
            , ("spaces", spaces                     )
            )
            (space `elem` toList spaces))

prop_MovingSpacesDoesNotChangeHowManyThereAre = testMovableSpace
  (\(init, final, _, _) ->
    let old  = testStateSpaces init
        new  = testStateSpaces final
        oldL = length old
        newL = length new
     in debug ( ("debug", "Old length should equal new")
              , ("old info", old)
              , ("new info", new)
              )
              (oldL === newL))

prop_ResultOfMoveIndicatesSuccess = forAllQC
   ((,) <$> arbitrary <*> arbitrary)
   shrink
   moveSpace
   (\(init, final, d, result) ->
     let focusedD  = DID (focusedI (_stateDisplays init))

         toD       = (`nth` unDID d) . _stateDisplays
         toInit    = len (toD init)
         toFinal   = len (toD final)

         fromDID   = DID (focusedI (_stateDisplays init))
         fromD     = (`nth` unDID fromDID) . _stateDisplays
         fromInit  = len (fromD init )
         fromFinal = len (fromD final)
      in conjoin $ [
         debug "Shouldn't fail when nothing to do"
               (property result .||. (d =/= focusedD))

       , debug "Failure should leave state alone"
               (property result .||. (init === final))

       ] ++ if not result || d == focusedD then [] else [
         debug ( ("debug" , "Move should shrink source")
               , ("before", fromInit                   )
               , ("after" , fromFinal                  )
               )
               (fromInit === fromFinal + 1)

       , debug ( ("debug" , "Move should grow destination")
               , ("before", toInit                        )
               , ("after" , toFinal                       )
               )
               (toInit + 1 === toFinal)
       ])

-- Test compound commands

testSwappable cmd = forAllQC
    (do s <- genWMState 2 `suchThat` enoughSpaces
        pure (labelWithIndices s, ()))
    (\(s, ()) -> map (, ())                                 .
                 filter (\s' -> enoughSpaces s' && s' /= s) .
                 map labelWithIndices                       .
                 shrink                                     $ s)
    (const cmd)

  where enoughSpaces s = (( > 2) . len . testStateSpaces $ s) &&
                         ((== 2) . len . _stateDisplays  $ s)

prop_CanMoveNonSingletonSpacesToDisplay = forAllQC
    (do n  <- chooseNat (2, 10)
        s  <- genWMState n
        let spaces = testStateSpaces s
        si   <- elements spaces
        dest <- elements (nub (map sDisplay spaces))

        -- If the chosen space doesn't have a sibling make one
        sibling <- arbitrary
        before  <- arbitrary
        let i          = sIndex   si
            d          = sDisplay si
            adopted :: NEZipper (Bool, NEZipper TestSpace)
            adopted    = imap adopt (_stateDisplays s)
            foundD     = any fst adopted
            ds'        = fmap snd adopted
            s'         = set stateDisplays ds' s
            adopt di z = if DID di == d
              then (True, case z of
                     Z [] y [] -> if before
                                     then Z [sibling] y []
                                     else Z [] y [sibling]
                     _         -> z)
              else (False, z)

        if foundD
           then if movable'  (s', (i, dest))
                   then pure (s', (i, dest))
                   else err ( ("error", "Generated unmovable space")
                            , ("space", i                          )
                            , ("info" , si                         )
                            , ("state", s'                         )
                            )
           else err ("Didn't find the right display"))
    (let truncate :: SpaceIndex
                  -> Display
                  -> WMState
                  -> (WMState, (SpaceIndex, Display))
         truncate (SIndex i) (DID d) s =
           let ds = len (stateDisplayInfos s)
               ss = len (testStateSpaces   s)
            in (s, (SIndex (i `mod` ss), DID (d `mod` ds)))

         go (s, (i, d)) = filter movable'    .
                          map (truncate i d) .
                          shrink             $ s
      in go)
    (\(i, d) -> moveSpaceToDisplay (indexS i) d)
    (\(init, final, (i, d), success) -> case success of
      False -> debug
        ( ("Failure" , "Didn't move")
        , ("displays", stateDisplayInfos init)
        )
        (property False)
      _     ->
        let isi   = first' () ((== i) . sIndex)
                              (testStateSpaces init)
            id    = sDisplay isi
            il    = sLabel isi

            get x = dSpaces                       .
                    first' () ((== x) . dDisplay) .
                    stateDisplayInfos
            ifrom = get id init
            ito   = get d  init
            ffrom = get id final
            fto   = get d  final

            idx x = first' () ((== x) . sIndex) . testStateSpaces
            flabs = map (sLabel . (`idx` final)) fto

            dbg m = debug ( ("debug"             , m    )
                          , ("label"             , il   )
                          , ("Destination labels", flabs)
                          , ( "Spaces on displays"
                            , ( "Initially"
                              , ("Source"     , ifrom)
                              , ("Destination", ito  )
                              )
                            , ( "Result"
                              , ("Source"     , ffrom)
                              , ("Destination", fto  )
                              )
                            )
                          , ("Original index to move", i)
                          )

            noop   = dbg "No-op move shouldn't change state"
                         (init === final)

            source = dbg "Source display should lose a space"
                         (length ffrom === length ifrom - 1)

            dest   = dbg "Destination should gain a space"
                         (length fto   === length ito   + 1)

            label  = dbg "Label should now be on destination"
                         (property (il `elem` flabs))

         in label .&&. if id == d
                          then noop
                          else source .&&. dest)

  where movable' (s, (i, _)) = movable s i

prop_CanSendToOtherDisplay = forAllQC
    ((do state <- genWMState 2
         si    <- elements (testStateSpaces state)
         pure (state, si)) `suchThat` canMove)
    (\(state, si) ->
      let SIndex i = sIndex si
          getMod s = nthMod (testStateSpaces s) i
       in filter canMove               .
          map (\s' -> (s', getMod s')) .
          shrink                       $ state)
    sendToOtherDisplay
    (\(init, final, si, result) ->
      let l        = sLabel   si
          d        = sDisplay si
          isis     = testStateSpaces   init
          fsis     = testStateSpaces   final
          idis     = stateDisplayInfos init
          fdis     = stateDisplayInfos final
          spacesOn = len . dSpaces
          idiFrom  = first' () ((== d) . dDisplay) idis
          idiTo    = first' () ((/= d) . dDisplay) idis
          fdiFrom  = first' () ((== d) . dDisplay) fdis
          fdiTo    = first' () ((/= d) . dDisplay) fdis
          isisFrom = filter ((== d) . sDisplay) isis
          fsisTo   = filter ((/= d) . sDisplay) fsis
          hadLabel = debug "" (l `elem` (map sLabel isisFrom))
          hasLabel = debug "" (l `elem` (map sLabel fsisTo  ))
          oldDec   = debug "" (spacesOn fdiFrom === spacesOn idiFrom - 1)
          newInc   = debug "" (spacesOn fdiTo   === spacesOn idiTo   + 1)
       in property result .&&. hadLabel .&&. hasLabel .&&. oldDec .&&. newInc)

  where canMove (state, si) = movable state (sIndex si) &&
                              len (_stateDisplays state) == 2

prop_CanLabelByIndex = testSwappable (pure ()) (\(init, _, (), _) ->
  let got  = map sLabel (testStateSpaces init)
      want = map (Just . SLabel . ('l':) . show) [0..len got - 1]
   in got === want)

prop_CanSwapVisible = testSwappable swapVisibles (\(init, final, (), result) ->
  let raw = map (\s -> (sDisplay s, sLabel s)) .
            filter (unV . sVisible)            .
            testStateSpaces

      get :: WMState -> [(Display, Maybe SpaceLabel)]
      get s = let hasD d = (d ==) . fst
                  all    = raw s
               in [ first' () (hasD 0) all
                  , first' () (hasD 1) all
                  ]

      succeeded = debug ( ("debug", "swapVisible failed")
                        , ("final", final               )
                        )
                        (property result)
   in succeeded .&&. case (get init, get final) of
        ([(0, il0), (1, il1)], [(0, fl0), (1, fl1)]) ->
          debug
            ( ("want" , "end[0] == start[1] && end[1] == start[0]")
            , ("start", (il0, il1)                                )
            , ("end"  , (fl0, fl1)                                )
            )
            ((il0 === fl1) .&&. (il1 === fl0))

        (i, f) -> debug ( ("fail"     , "Pattern mismatch")
                        , ("get init" , i                 )
                        , ("get final", f                 )
                        )
                        (property False))

prop_swapVisibleKeepsDisplayFocus = forAllQC
  arbitrary
  shrink
  (\() -> swapVisibles)
  (\(init, final, _, _) ->
    let dOf    = focusedI . _stateDisplays
        before = dOf init
        after  = dOf final
     in before === after)

shrinkInfoToState :: SpaceInfo -> WMState -> (WMState, SpaceInfo)
shrinkInfoToState i s = (s, nthMod (testStateSpaces s) (unSIndex (sIndex i)))

prop_focusHereKeepsDisplayFocus = forAllQC
  (do s <- labelWithIndices <$> arbitrary
      i <- elements (testStateSpaces s)
      pure (s, i))
  (\(s, i) -> map (shrinkInfoToState i) .
              filter (/= s)             .
              map labelWithIndices      .
              shrink $ s)
  focusHere
  (\(init, final, _, _) ->
    let dOf = focusedI . _stateDisplays
        dI  = dOf init
        dF  = dOf final
     in dI === dF)

prop_focusHereAlwaysSucceedsWhenMovable = forAllQC
  (do s <- genWMState 2 `suchThat` hasMovableSpace
      i <- elements (testStateSpaces s)
      pure (s, i))
  (\(s, i) -> map (shrinkInfoToState i) .
              filter hasMovableSpace    .
              shrink $ s)
  focusHere
  (\(_, _, _, result) -> result)

prop_focusHereWillFocusMovableSpaces = forAllQC
    ((do s <- genWMState' 2 `suchThat` hasMovableSpace
         i <- elements (testStateSpaces s)
         pure (s, i))  `suchThat` labelled)
    (\(s, i) -> map (\s' -> (s', nthMod (testStateSpaces s')
                                        (unSIndex (sIndex i)))) .
                filter hasMovableSpace .
                filter allLabelled     .
                filter twoDisplays     .
                shrink $ s)
    focusHere
    (\(_, final, si, _) ->
      let got  = view (_currentSpace . testLabel) final
          want = sLabel si
       in got === want)

  where labelled = isJust . sLabel . snd

prop_focusHereOnUnfocusedVisibleSwapsVisibles = forAllQC
    (do s <- genLabelled
        pure (s, visibleUnfocused s))
    (\(s, _) -> map (\s' -> (s', visibleUnfocused s')) .
                shrinkLabelled $ s)
    focusHere
    (\(init, final, _, _) ->
      let vis  = filter (unV . sVisible) . testStateSpaces
          iVis = vis init
          fVis = vis final
          iLabs = map sLabel iVis
          fLabs = map sLabel fVis
       in fLabs === reverse iLabs)

  where visibleUnfocused =
          first' () ((unV . sVisible) &&& (not . unF . sFocused)) .
          testStateSpaces

f &&& g = \x -> f x && g x

prop_focusHereOnSameDisplayDoesNotSwapSpacesBetweenDisplays = forAllQC
    (do s <- genLabelled
        let d = focusedI . _stateDisplays $ s
        si <- elements . filter ((== d) . sDisplay) . testStateSpaces $ s
        pure (s, si))
    (\(s, si) -> map (\s' -> (s', nthMod (spacesOfFocusedDisplay s')
                                         (unSIndex (sIndex si)))) .
                 shrinkLabelled $ s)
    focusHere
    (\(init, final, _, _) ->
      let labelDisplays   = map (\s -> (sLabel s, sDisplay s)) . testStateSpaces
          iDisplays       = labelDisplays init
          fDisplays       = labelDisplays final
          match ds (l, d) = debug ("label", l)
                                  (snd (first' () ((== l) . fst) ds) === d)
       in conjoin (map (match iDisplays) fDisplays) .&&.
          conjoin (map (match fDisplays) iDisplays))

spacesOfFocusedDisplay s = filter ((== focusedI (_stateDisplays s)) . sDisplay)
                                  (testStateSpaces s)

prop_focusHereOnOneDisplayLeavesOtherVisibleIntact = forAllQC
  (do s  <- genLabelled
      let d = focusedI . _stateDisplays $ s
      si <- elements . filter ((== d) . sDisplay) . testStateSpaces $ s
      pure (s, si))
  (\(s, si) -> map (\s' -> (s', nthMod (spacesOfFocusedDisplay s')
                                       (unSIndex . sIndex $ si))) .
               shrinkLabelled $ s)
  focusHere
  (\(init, final, _, _) ->
    let others s = filter ((/= focusedI (_stateDisplays s)) . sDisplay)
                          (testStateSpaces s)
     in others init === others final)

infoToNat = unSIndex . sIndex

prop_focusHereOnNonVisibleSpaceFromOtherDisplaySwitchesTheTwo = forAllQC
    (do s  <- genLabelled `suchThat` hasOtherInvisible
        si <- elements (otherInvisibles s)
        pure (s, si))
    (\(s, si) -> map (\s' -> (s', nthMod (otherInvisibles s')
                                         (infoToNat si  ))) .
                 filter hasOtherInvisible                   .
                 shrinkLabelled $ s)
    focusHere
    (\(init, final, _, _) ->
      let iFocusedLabel = view (_currentSpace . testLabel) init
          iVisibleLabel = view (  _otherSpace . testLabel) init
          fVisibleLabel = view (  _otherSpace . testLabel) final
          fOtherLabels  = toList . fmap _testLabel . view _otherDisplay $ final
       in conjoin [
            debug "Other visible space remains the same"
                  (fVisibleLabel === iVisibleLabel)
          , debug ( "Initially focused space should get put on other display"
                  , ("Initial focused label", iFocusedLabel )
                  , ("Final other display"  ,   fOtherLabels)
                  )
                  (iFocusedLabel `elem` fOtherLabels)
          ])

  where hasOtherInvisible = not . null . otherInvisibles

        otherInvisibles s = let d         = focusedI (_stateDisplays s)
                                onOther   = (/= d) . sDisplay
                                invisible = not . unV . sVisible
                             in filter (onOther &&& invisible)
                                       (testStateSpaces s)

prop_focusHereNeverChangesNumberOfSpacesOnEachDisplay = forAllQC
  (do s  <- genLabelled
      si <- elements (testStateSpaces s)
      pure (s, si))
  (\(s, si) -> map (shrinkInfoToState si) . shrinkLabelled $ s)
  focusHere
  (\(init, final, _, _) ->
    let numSpaces = map length . toList . _stateDisplays
        iNums     = numSpaces init
        fNums     = numSpaces final
     in iNums === fNums)

{-

TODO: Check that populating/labelling is robust

TODO: Write quickcheck tests that IO out to Yabai, to check some of our assumptions
(validating basic functions of the test interpreter)
-}

-- Test workspace setup

testLabels = forAllQC
  ((,) <$> genWMState 2 <*> genSpaceLabels)
  (\(s, ls) ->
    let uniqLabels (_, ls) = uniq ls
        shrunkStates       = shrink s
        shrunkLabels       = transpose (map shrink ls)
        enoughLabels       = (== length ls) . length
     in filter uniqLabels
               (zip shrunkStates
                    (takeWhile enoughLabels shrunkLabels)))

prop_CanDestroySufficientSpaces = forAllQC
  (do dCount <- chooseNat (1, 10)
      state  <- genWMState dCount
      n      <- chooseNat (dCount, dCount * 2)
      pure (state, n))
  (\(s, n) -> let diff = n - len (_stateDisplays s)
                  f s  = (s, len (_stateDisplays s) + diff)
               in map f (shrink s))
  destroyDownTo
  (\(init, final, n, _) ->
    let initialSpaces = testStateSpaces init
        finalSpaces   = testStateSpaces final
        got           = len finalSpaces
        want          = if len initialSpaces >= n
                           then n
                           else len initialSpaces
     in debug ( ("initialSpaces", initialSpaces)
              , ("finalSpaces"  , finalSpaces  )
              , ("got"          , got          )
              , ("Want"         , want         )
              )
              (got === want))

prop_PopulatingMakesTheRightNumberOfSpaces = testLabels
  populateSpaces'
  (\(_, final, labels, _) ->
    let spaces :: [SpaceInfo]
        spaces = testStateSpaces final
        got    = length spaces
        want   = length labels
     in debug ( ("got"   , got   )
              , ("want"  , want  )
              , ("spaces", spaces)
              )
              (got === want))

prop_DodgyLabelsAreRemoved = testLabels
  removeDodgyLabels
  (\(_, final, labels, _) ->
    let spaces = testStateSpaces final
        got    = map sLabel spaces
        dodgy  = filter (maybe False (`notElem` labels)) got
     in debug ( ("spaces", spaces   )
              , ("got"   , sort got )
              , ("dodgy" , dodgy    )
              )
              (dodgy === []))

prop_LabellingMakesAllTheRightLabels = testLabels
  labelSpaces'
  (\(_, final, want, _) ->
    let spaces = testStateSpaces final
        got    = map sLabel spaces
     in debug ( ("spaces", spaces   )
              , ("want"  , sort want)
              , ("got"   , sort got )
              )
              (sort (map Just want) == sort got))

-- Entry point

return []  -- Required for TemplateHaskell to find the above properties
tests = testProperties "All properties" $allProperties

main = defaultMain tests
