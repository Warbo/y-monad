#!/usr/bin/env runhaskell
{-# LANGUAGE ConstraintKinds     #-}
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

import           Control.Lens
import           Data.List (intercalate, nub, transpose)
import           GHC.Exts (IsList (..))
import           Numeric.Natural (Natural)
import           Polysemy
import           Polysemy.State
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
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
  }

-- | A 'Space' in our test state is either a non-empty zipper of 'Window's (with
--   the "focused" element being the focused 'Window'), or 'Nothing' if there is
--   no 'Window' on the 'Space'.
data TestSpace = TS { _testLabel   :: Maybe SpaceLabel
                    , _testWindows :: Maybe (NEZipper Window)
                    } deriving (Eq, Show)

-- Basic instances (except for Arbitrary)

-- | Don't try to show an 'InfiniteList'
instance Show WMState where
  show s = show (_stateDisplays s)

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
  imap f (Z xs y zs) = Z (reverse (zipWith f [0 ..] xs))
                         (                 f  l     y )
                         (         zipWith f [l'..] zs)
    where l  = fromIntegral (length xs)
          l' = l + 1

instance Foldable NEZipper where
  foldMap f (Z xs y zs) = foldMap f (reverse xs) <> f y <> foldMap f zs

instance FoldableWithIndex Natural NEZipper where
  ifoldMap f (Z xs y zs) = mconcat (reverse (zipWith f [0 ..] (reverse xs))) <>
                           f l y                                            <>
                           mconcat          (zipWith f [l'..]          zs)
    where l  = fromIntegral (length xs)
          l' = l + 1

instance Traversable NEZipper where
  -- | Reverse 'xs' before traversing it, to ensure effects accumulate in the
  --   right order, then reverse the result back to how it started.
  traverse f (Z xs y zs) = Z <$> (reverse <$> traverse f (reverse xs))
                             <*> f y
                             <*> traverse f zs

instance TraversableWithIndex Natural NEZipper where
  itraverse f z = case imap f z of
    Z xs y zs -> Z <$> (reverse <$> sequenceA (reverse xs))
                   <*> y
                   <*>              sequenceA          zs

instance IsList (NEZipper a) where
  type Item (NEZipper a) = a

  fromList []     = error "NEZipper can't be empty"
  fromList (y:zs) = Z [] y zs

  toList (Z xs y zs) = reverse xs ++ [y] ++ zs

-- Template Haskell: definitions above here cannot depend on anything below

makeLenses ''WMState
makeLenses ''TestSpace

-- Optics for getting and setting inside our state

asZipper :: Prism' (Natural, [a]) (NEZipper a)
asZipper = prism fro to
  where fro :: NEZipper a -> (Natural, [a])
        fro z@(Z xs _ _) = (fromIntegral (length xs), toList z)

        to :: (Natural, [a]) -> Either (Natural, [a]) (NEZipper a)
        to (n, l) = let i = fromIntegral n
                     in if i >= 0 && i < length l && not (null l)
                           then let (xs', y':zs') = splitAt i l
                                 in Right (Z (reverse xs') y' zs')
                           else Left (n, l)

focused :: Lens' (NEZipper a) a
focused inj (Z xs y zs) = f <$> inj y
  where f y' = Z xs y' zs

--stateDisplayInfos :: Traversal' WMState DisplayInfo
--stateDisplayInfos = stateDisplays . traverse . displaysInfo

{-
stateSpaceInfos :: Traversal' WMState SpaceInfo
stateSpaceInfos = error "stateSpaceInfos"

stateWindowInfos :: Traversal' WMState WindowInfo
stateWindowInfos = error "stateWindowInfos"

displaySpaceInfos :: Traversal' TestDisplay SpaceInfo
displaySpaceInfos = error "displaySpaceInfos"

spaceWindowInfos :: Traversal' TestSpace WindowInfo
spaceWindowInfos = error "spaceWindowInfos"
-}
-- Helper functions

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
                                   [1..]

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
                                 [1..]

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

testDisplays :: WMState -> [DisplayInfo]
testDisplays s = go (DID 1) (SIndex 1) (toList (_stateDisplays s))
  where go (DID di) (SIndex si) ds = case ds of
          []    -> []
          d:ds' -> let len = length (toList d)
                    in DI { dDisplay = DID di
                          , dSpaces  = map SIndex (take len [si..])
                          } : go (DID (di+1))
                                 (SIndex (si + fromIntegral len))
                                 ds'

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

testStateSpaces :: WMState -> [SpaceInfo]
testStateSpaces s = case _stateDisplays s of
    Z xs y zs -> let l :: [[(Focused, TestDisplay)]]
                     l = [ map (unFoc,) (reverse xs)
                         , [   (isFoc,           y)]
                         , map (unFoc,)          zs
                         ]
                  in go 1 1 (concat l)
  where go di si ds = case ds of
          []               -> []
          (foc, d):ds' -> let spaces = testSpaces (SIndex si) (DID di) foc d
                           in spaces ++ go (di + 1)
                                           (si + fromIntegral (length spaces))
                                           ds'

mapIndex :: (a -> Natural -> b) -> NEZipper a -> NEZipper b
mapIndex f (Z xs y zs) = let len = fromIntegral (length xs)
                          in Z (zipWith f xs [len..1])
                               (f y (len + 1))
                               (zipWith f zs [len+1..])

testWindowInfos :: Display -> SpaceIndex -> Visible -> Focused -> [WindowInfo]
testWindowInfos _ _ _ _ {--d s vis foc-} = error "UNDEFINED testWindowInfos"

spaceIndices :: [DisplayInfo] -> [SpaceIndex]
spaceIndices = concatMap dSpaces

testStateEnvVar :: String -> (String -> a) -> WMState -> Maybe a
testStateEnvVar _ _ = error "testStateEnvVar"

-- Pure, in-memory implementation of Yabai queries and commands

queryState :: Member (State WMState) r => Sem (Query ': r) a -> Sem r a
queryState = interpret query
  where query :: Member (State WMState) r => Query m a -> Sem r a
        query q = case q of
          GetDisplays   -> testDisplays        <$> get
          GetSpaces     -> testStateSpaces     <$> get
          GetWindows    -> testStateWindows    <$> get
          GetEnvVar v f -> testStateEnvVar v f <$> get

commandState :: Member (State WMState) r => Sem (Command ': r) a -> Sem r a
commandState = undefined {-interpret command
  where command :: Command m a -> State WMState m a
        command c = case c of
          CreateSpace    -> do
            state <- get
            let (state', wi) = mkSpace state
            put state'
            pure wi
          DestroySpace   -> ["space", "--destroy"]
          LabelSpace s l -> ["space", show s, "--label", show l]

          FocusWindow  w -> ["window" , "--focus", either show show w]
          FocusSpace   s -> ["space"  , "--focus",        showSpace s]
          FocusDisplay d -> ["display", "--focus", either show show d]

          MoveWindow w s -> concat [ ["window", "--move"]
                                    , maybe [] ((:[]) . show) w
                                    , ["--space", show s]
                                    ]
          SwapWindow s   -> ["window", "--swap"   , show s]

          MoveSpace  d   -> ["space" , "--display", show d]
                         -}

-- | Make a new 'SpaceInfo', based on the given random seeds and indices of
--   spaces on existing displays. Note that the resulting 'SpaceInfo' should
--   make sense, e.g. its 'sDisplay' and 'sIndex' will be in the range allowed
--   by the arguments; but the other members of the 'WMState' will need to be
--   adjusted to take this new 'SpaceInfo' into account.
mkSpace :: WMState -> WMState
mkSpace _ = error "mkSpace undefined"
{-  where InfiniteList seeds _ = _stateSeeds state

        Z dxs dy dzs = _stateDisplays state

        dCount = length dxs + 1 + length dzs

        chosenD = dCount `mod` (seeds !! 1)

        modD :: (TestDisplay -> TestDisplay) -> NEZipper TestDisplay
        modD f =
          let i = seeds !! 0
           in case ((seeds !! 1) `mod` 3, dxs, dzs) of
                (0, x:xs, _   ) -> Z (replaceElem dxs d i) dy dzs
                (1, _   , z:zs) -> Z dxs dy (replaceElem dzs d i)
                (_, []  , []  ) -> Z dxs d dzs
-}
{-
        insertSpace s (Z sxs sy szs) = case (seeds !! 2) of
  let dCount  = length sIndices
      display = fromIntegral s1 `mod` dCount
      dSpaces = sIndices !! display
      iOffset = fromIntegral s2 `mod` (length dSpaces + 1)
      index   = let prevSpaces = concat (take display sIndices)
                 in length prevSpaces + iOffset
   in SI { sLabel   = Nothing
         , sIndex   = SIndex (fromIntegral index)
         , sDisplay = DID (fromIntegral (display + 1))
         , sWindows = []
         , sVisible = V (even s3)
         , sFocused = F (even s4)
         }
-}

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

instance Arbitrary SpaceLabel where
  arbitrary = SLabel <$> arbitrary `suchThat` (/= "")
  shrink (SLabel s) = map SLabel . filter (/= "") . shrink $ s

instance Arbitrary a => Arbitrary (NEZipper a) where
  arbitrary = Z <$> arbitrary <*> arbitrary <*> arbitrary

  shrink (Z xs y zs) = map (\(xs, y, zs) -> Z xs y zs)
                           (shrink (xs, y, zs))

instance Arbitrary TestSpace where
  arbitrary = TS <$> arbitrary
                 <*> arbitrary `suchThat` maybe True (uniq . toList)

  shrink ts =    map (\(l, mws) -> TS { _testLabel = l, _testWindows = mws }) .
              filter (\(_, mws) -> maybe True uniq (toList <$> mws))                     .
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
                     pure (State (uniqueDisplaysWindows [] ds) seeds)

  shrink s = map (\ds -> s { _stateDisplays = ds })
                 (shrink (_stateDisplays s))

-- | Like 'choose (min, max)' but for 'Natural'
chooseNat :: (Natural, Natural) -> Gen Natural
chooseNat (min, max) = fromIntegral <$> choose @Int ( fromIntegral min
                                                    , fromIntegral max
                                                    )

-- | Generate a list of labels to emulate our desired XMonad-like setup
genSpaceLabels :: Gen [SpaceLabel]
genSpaceLabels = do n <- choose (5, 15)
                    vectorOf n arbitrary `suchThat` uniq

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

tests :: TestTree
tests = testGroup "All tests" [
    testGroup "Test that stateful interpreter works" [
      testProperty "Generated displays count upwards"
        (forAllDisplays (\dis ->
          let got  = map dDisplay             dis
              want = map (DID . fromIntegral) [1..length dis]
           in got === want))

    , testProperty "Generated displays have at least one space each"
        (forAllDisplays (\dis ->
          (all (not . null . dSpaces) dis)))

    , testProperty "Generated displays have space indices counting up"
        (forAllDisplays (\dis ->
          let got  = concatMap dSpaces dis
              want = map SIndex (take (length got) [1..])
           in got === want))

    , testProperty "Generate correct number of spaces"
        (forAllDisplaySpaces (\(dis, sis) ->
          let want = sum (map (length . dSpaces) dis)
           in length sis === want))

    , testProperty "Generate correct space indices"
        (forAllDisplaySpaces (\(dis, sis) ->
          let want = concatMap dSpaces dis
              got  = map sIndex sis
           in got === want))

    , testProperty "Spaces have correct displays"
        (forAllDisplaySpaces (\(dis, sis) ->
          let onDisplay di si = sIndex si `elem` dSpaces di

              checkDisplay di = let spaces = filter (onDisplay di) sis
                                 in conjoin
                                      (map ((=== dDisplay di) . sDisplay)
                                           spaces)

           in conjoin (map checkDisplay dis)))

    , testProperty "Space indices count up"
        (forAllSpaces (\sis ->
          let got  =                    map sIndex sis
              want = take (length got) (map SIndex [1..])
           in got === want))

    , testProperty "Windows are unique"
        (forAllWindows (\ws ->
          let wids = map wWindow ws
           in uniq wids))
    ]
  , testGroup "Test queries" [
      testGroup "Primitive queries" [
        testProperty "Can get displays"
          (forAllQ (getDisplays :: Q r [DisplayInfo]) (\(i, s, displays) ->
            i === s .&&. not (null displays)))

      , testProperty "Can get spaces"
          (forAllQ (getSpaces :: Q r [SpaceInfo]) (\(i, s, spaces) ->
            i === s .&&. not (null spaces)))

      , testProperty "Can get windows"
          (forAllQ (getWindows :: Q r [WindowInfo]) (\(i, s, windows) ->
            i === s .&&. length windows >= 0))
      ]
    , testGroup "Compound queries" [
        testProperty "displayCount matches state"
          (forAllQ (displayCount :: Q r Natural) (\(_, s, dCount) ->
            dCount === (fromIntegral . length . _stateDisplays $ s)))

      , testProperty "pluggedIn counts displays"
          (forAllQ pluggedIn (\(_, s, p) ->
            let len = length (_stateDisplays s)
             in counterexample (show ("len", len, "p", p)) $
                (len == 2) === p))
      ]
    ]
  , testGroup "Test commands" [
      testGroup "Workspace setup" [
        testProperty "Populating makes the right number of spaces"
        (forAllQC' ((,) <$> genSpaceLabels <*> genWMState 2)
                   (\(ls, s) -> zip (takeWhile ((== length ls) . length)
                                               (transpose (map shrink ls)))
                                    (shrink s))
                   (snd :: ([SpaceLabel], WMState) -> WMState)
                   (populateSpaces' . fst)
                   (\(_, final, labels, _) ->
        let spaces :: [SpaceInfo]
            spaces = undefined (toList (testDisplays final))
            got    = length spaces
            want   = length labels
         in got === want))
      ]
  ]
  ]
  where forAllDisplays :: ForAll [DisplayInfo]
        forAllDisplays = forAllStateful testDisplays

        forAllSpaces :: ForAll [SpaceInfo]
        forAllSpaces = forAllStateful testStateSpaces

        forAllDisplaySpaces :: ForAll ([DisplayInfo], [SpaceInfo])
        forAllDisplaySpaces = forAllStateful
          (\s -> (testDisplays s, testStateSpaces s))

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

        forAllQ :: Sem (Query ': State WMState ': '[]) a
                -> ForAll (WMState, WMState, a)
        forAllQ q f = forAllShrink arbitrary shrink
          (\init -> let (final, result) = run (runState init (queryState q))
                     in f (init, final, result))

        {-
        forAllQC :: Sem (Command ': Query ': State WMState ': '[]) a
                 -> ForAll (WMState, WMState, a)
        forAllQC qc f = forAllQC' arbitrary shrink id (const qc)
          (\(init, final, _, result) -> f (init, final, result))
          -}

        forAllQC' :: (Show a )
                  => Gen a
                  -> (a -> [a])
                  -> (a -> WMState)
                  -> (a -> Sem (Command ': Query ': State WMState ': '[]) b)
                  -> ForAll (WMState, WMState, a, b)
        forAllQC' gen shr get qc f = forAllShrink gen shr
          (\x -> let init = get x
                     (final, result) = run (runState init
                                             (queryState
                                               (commandState
                                                 (qc x))))
                  in f (init, final, x, result))

-- Entry point

main = defaultMain tests
