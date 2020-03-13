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

import Numeric.Natural (Natural)
import Polysemy
import Polysemy.State
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Yabai

main = defaultMain tests

tests :: TestTree
tests = testGroup "State tests" [
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
  where forAllDisplays :: Testable prop => ([DisplayInfo] -> prop) -> Property
        forAllDisplays = forAllShrink (chooseNat (1, 20) >>= genDisplays)
                                      shrinkDisplays

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
    displays    <- genDisplays dCount
    extraSpaces <- chooseNat (0, 20)
    spaces      <- genSpaces (map dSpaces displays) extraSpaces
    windows     <- error "NO WINDOWS IMPLEMENTED"
    seeds       <- arbitrary
    pure (State { stateDisplays = displays
                , stateSpaces   = spaces
                , stateWindows  = windows
                , stateSeeds    = seeds
                })

-- | Generate a list of consistent 'DisplayInfo' entries (i.e. their 'dSpaces'
--   indices count up from 1)
genDisplays :: Natural -> Gen [DisplayInfo]
genDisplays = go []
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
genSpaces :: [[SpaceIndex]] -> Natural -> Gen [SpaceInfo]
genSpaces _ = go []
  where go :: [SpaceInfo] -> Natural -> Gen [SpaceInfo]
        go acc 0 = pure (reverse acc)
        go _ _ = do
          error "NOT IMPLEMENTED"

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
