#!/usr/bin/env runhaskell
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Werror -W -fno-warn-missing-methods #-}

module Main where

import Data.Maybe (isJust)
import Data.List (nub)
import Numeric.Natural (Natural)
import Polysemy
import Polysemy.State
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Yabai

main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [
    testGroup "Tests of stateful interpreter" [
      testGroup "Display generation" [
        testProperty "Generated displays count upwards"
          (forAllDisplays (\dis ->
            let got  = map dDisplay             dis
                want = map (DID . fromIntegral) [1..length dis]
             in got === want))

      , testProperty "Generated displays have at least one space each"
          (forAllDisplays
            (all (not . null . dSpaces)))

      , testProperty "Generated displays have space indices counting up"
          (forAllDisplays (\dis ->
            let got  = concatMap dSpaces dis
                want = map SIndex (take (length got) [1..])
             in got === want))
      ]
      , testGroup "Space generation" [
          testProperty "Generate correct number of spaces"
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
      ]
    ]
  ]
  where forAllDisplays :: ForAll [DisplayInfo]
        forAllDisplays = forAllShrink genDisplays shrinkDisplays

        forAllSpaces :: ForAll [SpaceInfo]
        forAllSpaces = forAllShrink (genDisplays >>= genSpaces) shrinkSpaces

        forAllDisplaySpaces :: ForAll ([DisplayInfo], [SpaceInfo])
        forAllDisplaySpaces = forAllShrink
          (do dis <- genDisplays
              sis <- genSpaces dis
              pure (dis, sis))
          shrinkSpacesAndDisplays


type ForAll a = forall prop. Testable prop => (a -> prop) -> Property

spaceIndices :: [DisplayInfo] -> [SpaceIndex]
spaceIndices = concatMap dSpaces

--prop_mkSpaceExists

-- Pure, in-memory implementation of Yabai queries and commands

-- Random numbers for use when making decisions
type Seed = Natural

instance Arbitrary Natural where
  arbitrary = fromIntegral <$> arbitrary @Int
  shrink n  = fromIntegral <$> (shrink @Int (fromIntegral n))

-- | The state of our window manager: displays, spaces and windows
data WMState = State { stateDisplays :: [DisplayInfo]
                     , stateSpaces   :: [  SpaceInfo]
                     , stateWindows  :: [ WindowInfo]
                     , stateSeeds    :: InfiniteList Seed
                     }

chooseNat :: (Natural, Natural) -> Gen Natural
chooseNat (min, max) = fromIntegral <$> choose @Int ( fromIntegral min
                                                    , fromIntegral max )

genWMState :: Natural -> Gen WMState
genWMState dCount = do
    displays    <- genNDisplays dCount
    spaces      <- genSpaces displays
    windows     <- error "NO WINDOWS IMPLEMENTED"
    seeds       <- arbitrary
    pure (State { stateDisplays = displays
                , stateSpaces   = spaces
                , stateWindows  = windows
                , stateSeeds    = seeds
                })

-- | Generate a list of consistent 'DisplayInfo' entries (i.e. their 'dSpaces'
--   indices count up from 1)
genNDisplays :: Natural -> Gen [DisplayInfo]
genNDisplays = go []
  where go :: [DisplayInfo] -> Natural -> Gen [DisplayInfo]
        go acc 0 = pure (reverse acc)
        go acc n = do
          newSpaces <- choose (1, 10)  -- Must be at least 1
          let index      = length acc + 1
              spaces     = concatMap dSpaces acc
              newIndices = take newSpaces [(length spaces + 1)..]
              display    = DI { dDisplay = DID (fromIntegral index)
                              , dSpaces  = map (SIndex . fromIntegral)
                                               newIndices
                              }
          go (display:acc) (n-1)

genDisplays :: Gen [DisplayInfo]
genDisplays = chooseNat (1, 20) >>= genNDisplays

-- | Shrink function to simplify counterexamples, used with 'forAllShrink'. Note
--   that we don't make an 'Arbitrary' instance since it would overlap with the
--   generic 'Arbitrary a => Arbitrary [a]' instance.
shrinkDisplays :: [DisplayInfo] -> [[DisplayInfo]]
shrinkDisplays dis = concat [
      if length dis > 1 then [take 1 dis, fixIndices (drop 1 dis)] else []
    , if dropped == dis then [] else [dropped]
    ]
  where dropped = fixIndices (dropSpaces [] dis)

        fixIndices = let nextDID       = DID . fromIntegral . (+1) . length
                         nextDI acc ss = DI { dDisplay = nextDID acc
                                            , dSpaces  = map (inc acc) [1..ss]
                                            }
                         inc acc       = SIndex       .
                                         fromIntegral .
                                         (+ length (concatMap dSpaces acc))

                         fixDI acc d   = nextDI acc (length (dSpaces d))

                         go acc []     = reverse acc
                         go acc (d:ds) = go (fixDI acc d:acc) ds
                      in go []

        dropSpaces acc []     = reverse acc
        dropSpaces acc (d:ds) =
          let spaces    = dSpaces d
              sCount    = length spaces
              newCount  = sCount `div` 2
              newSpaces = if null spaces
                             then spaces
                             else if even (length acc)
                                     then take newCount spaces
                                     else drop newCount spaces
           in dropSpaces (d { dSpaces = newSpaces }:acc) ds

-- | Generate a list of 'SpaceInfo' entries, consistent with the given
--   'DisplayInfo' entries.
genSpaces :: [DisplayInfo] -> Gen [SpaceInfo]
genSpaces = go []
  where go :: [SpaceInfo] -> [DisplayInfo] -> Gen [SpaceInfo]
        go acc []       = do n <- arbitrary
                             pure (focusAndVisible n (reverse acc))
        go acc (di:dis) = do spaces <- genForDisplay di
                             go (reverse spaces ++ acc) dis

        genForDisplay :: DisplayInfo -> Gen [SpaceInfo]
        genForDisplay di = mapM (genSpace (dDisplay di)) (dSpaces di)

        genSpace :: Display -> SpaceIndex -> Gen SpaceInfo
        genSpace d i = do
          label   <- genMaybeLabel
          windows <- map WID <$> arbitrary
          pure $
            SI { sLabel   = label
               , sIndex   = i
               , sDisplay = d
               , sWindows = windows
               , sVisible = False  -- Default; may be replaced before returning
               , sFocused = False  -- Default; may be replaced before returning
               }

shrinkSpaces :: [SpaceInfo] -> [[SpaceInfo]]
shrinkSpaces sis = concat [
    -- Try to discard a 'Display', since that's a big simplification
    if length displays > 1 then map dropDisplay displays else []

    -- Try to discard a 'Space', since that's pretty major
  , if length sis > length displays then concatMap dropSpace sis else []

    -- Last resort, see if we can discard a 'Window'
  , if null windows then [] else map dropWindow windows
  ]
  where dropDisplay :: Display -> [SpaceInfo]
        dropDisplay d = makeConsistent (filter ((/= d) . sDisplay) sis)

        -- We might be unable to remove a space (if it's the only one on a
        -- display); we return [] in that case
        dropSpace :: SpaceInfo -> [[SpaceInfo]]
        dropSpace s =
          let i   = sIndex s
              d   = sDisplay s
              onD = filter ((== d) . sDisplay) sis
           in if null onD
                 then []
                 else [makeConsistent (filter ((/= i) . sIndex) sis)]

        dropWindow :: Window -> [SpaceInfo]
        dropWindow w =
          let dropW w si = si { sWindows = filter (/= w) (sWindows si) }
           in map (dropW w) sis

        displays = nub (map sDisplay sis)

        windows  = concatMap sWindows sis

        makeConsistent =
          focusAndVisible (length sis + 123) .
          zipWith (\i si -> si { sIndex = SIndex i }) [1..]


-- | Shrink '([DisplayInfo], [SpaceInfo])' whilst maintaining their consistency.
--   In particular:
--    - We keep at least one 'Display'
--    - We keep at least one 'Space' per 'Display'
--    - When a 'DisplayInfo' is discarded, so are any corresponding 'SpaceInfo'
--    - There will always be exactly one visible 'Space' per 'Display'
--    - There will always be exactly one focused 'Space' among the visible
shrinkSpacesAndDisplays (dis, sis) =
    map pickVis
        (filter nonEmpty
                (map (\dis' -> (dis', fixSpaces dis'))
                     (shrinkDisplays dis)))
  where fixSpaces :: [DisplayInfo] -> [SpaceInfo]
        fixSpaces dis' = filter
          (\si -> sIndex   si `elem` concatMap dSpaces  dis' &&
                  sDisplay si `elem`       map dDisplay dis')
          sis

        nonEmpty :: ([a], [b]) -> Bool
        nonEmpty (dis', sis') = not (null dis') && not (null sis')

        pickVis (dis', sis') =
          let arbitraryNum = product [length dis + 1, length sis + 1] +
                             (length dis' * length sis')
           in (dis', focusAndVisible arbitraryNum sis')

focusAndVisible :: Int -> [SpaceInfo] -> [SpaceInfo]
focusAndVisible _ []  = error "Can't have no spaces"
focusAndVisible n sis = map modify sis
  where displays   = nub (map sDisplay sis)

        spacesOn d = map sIndex (filter ((== d) . sDisplay) sis)

        visible    = map (\d -> let spaces = spacesOn d
                                 in spaces !! (n `mod` length spaces))
                         displays

        focus      = visible !! (n `mod` length displays)

        modify si  = si { sFocused = sIndex si ==     focus
                        , sVisible = sIndex si `elem` visible
                        }

genMaybeLabel :: Gen (Maybe SpaceLabel)
genMaybeLabel = do s <- arbitrary
                   pure (if s == "" then Nothing else Just (SLabel s))

shrinkMaybeLabel :: Maybe SpaceLabel -> [Maybe SpaceLabel]
shrinkMaybeLabel Nothing           = []
shrinkMaybeLabel (Just (SLabel l)) = Nothing : filter isJust (map go (shrink l))
  where go "" = Nothing
        go x  = Just (SLabel x)

instance Arbitrary WMState where
  arbitrary = do
    dCount <- chooseNat (1, 10)
    genWMState dCount

queryState :: Member (State WMState) r => Sem (Query ': r) a -> Sem r a
queryState = interpret query
  where query :: Member (State WMState) r => Query m a -> Sem r a
        query GetDisplays = stateDisplays <$> get
        query GetSpaces   = stateSpaces   <$> get
        query GetWindows  = stateWindows  <$> get

{-
commandState :: Member (State WMState) r => Sem (Command ': r) a -> Sem r a
commandState = interpret command
  where command :: Command m a -> State WMState m a
        command c = case c of
          CreateSpace    -> do state <- get
                               let space = SI { sLabel = Nothing
                                              , sIndex = count + 1

                                   }
                                   modify (stateSpaces)["space", "--create" ]
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
mkSpace :: (Seed, Seed, Seed, Seed) -> [[SpaceIndex]] -> SpaceInfo
mkSpace (s1, s2, s3, s4) sIndices =
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
         , sVisible = even s3
         , sFocused = even s4
         }
