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
--import Test.QuickCheck
import Test.Tasty
import Yabai

main = defaultMain tests

tests :: TestTree
tests = testGroup "State tests" [
  ]

--prop_

-- Pure, in-memory implementation of Yabai queries and commands

-- | The state of our window manager: displays, spaces and windows
data WMState = State { stateDisplays :: [DisplayInfo]
                     , stateSpaces   :: [  SpaceInfo]
                     , stateWindows  :: [ WindowInfo]

                     -- Random numbers for use when making decisions
                     , stateSeeds :: [Natural]
                     }

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

        mkSpace :: Int -> SpaceInfo
        mkSpace sIndices =
          let dCount  = length sIndices
              display = s1 `mod` dCount
              dSpaces = sIndices !! display
              iOffset = s2 `mod` (length dSpaces + 1)
              index   = let prevSpaces = concat (take display dSpaces)
                         in length prevSpaces + iOffset
           in SI { sLabel   = Nothing
                 , sIndex   = index
                 , sDisplay = display + 1
                 , sWindows = []
                 , sVisible = even s3
                 , sFocused = even s4
                 }
-}
