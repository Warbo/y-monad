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
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Yabai where

import Data.Aeson ((.:), decode, FromJSON(..), withObject)
import Data.ByteString.Lazy (ByteString)
import Data.Char (isDigit)
import Data.String
import Data.Void
import Numeric.Natural (Natural)
import Polysemy
import System.Exit (ExitCode(..))
import System.Process.ByteString.Lazy as PBS
import System.Process.ListLike (proc)

-- A type-safe interface for interacting with the Yabai window manager for macOS

-- | Dump out error messages
err :: Show a => a -> b
err = error . show

-- Yabai (and macOS) manage Windows in a 3-level hierarchy. We don't try to keep
-- track of these relationships ourselves; we just query Yabai as needed.

-- | A 'Display' corresponds to a monitor, e.g. one for a laptop's built-in
--   monitor, another for a plugged-in external monitor, etc. Each 'Display'
--   contains at least one 'Space', with consecutive 'SpaceIndex' values.
newtype Display = DID Natural deriving (Eq)

instance Num Display where
  fromInteger = DID . fromInteger

-- | A 'Space' can contain any number of 'Window' values. A 'Space' can always
--   be identified by a 'SpaceIndex'. We can associate a 'Space' with a
--   'SpaceLabel' and use that instead in most situations.
type Space = Either SpaceIndex SpaceLabel

index :: SpaceIndex -> Space
index = Left

label :: SpaceLabel -> Space
label = Right

showSpace :: Space -> String
showSpace = either show show

-- | A 'Space' always has a 'SpaceIndex', which gives its position in Mission
--   Control. When a 'Space' is moved, the 'SpaceIndex' of every 'Space' may
--   change, making this an unreliable way to keep track. For this reason, it is
--   usually preferable to use a 'SpaceLabel' instead.
newtype SpaceIndex = SIndex Natural deriving (Eq)

instance Num SpaceIndex where
  fromInteger = SIndex . fromInteger

instance Show SpaceIndex where
  show (SIndex i) = show i

-- | A 'SpaceLabel' can be assigned to a 'Space', and subsequently used to refer
--   to that 'Space'. The 'SpaceLabel' will remain consistent event if we move
--   a 'Space' around; although they will be lost if Yabai crashes or restarts.
newtype SpaceLabel = SLabel String deriving (Eq)

-- | A 'SpaceLabel' can't be numeric, or it will be confused with a 'SpaceIndex'
mkLabel :: String -> SpaceLabel
mkLabel s = if all isDigit s
               then err ("SpaceLabel can't be numeric", s)
               else SLabel s

instance IsString SpaceLabel where
  fromString = mkLabel

instance Show SpaceLabel where
  show (SLabel l) = l

-- | For our purposes, a 'Window' is just an ID which we can move from one
--   'Space' to another.
newtype Window  = WID Natural deriving (Eq)

instance Num Window where
  fromInteger = WID . fromInteger

instance Show Window where
  show (WID w) = show w

-- Yabai is controlled by running shell commands, for example:
--
--     yabai -m query  --spaces
--     yabai -m window --focus next
--
-- The 'query' mode is always safe to run, and has no side-effects. The other
-- modes can cause effects, like moving windows around. We represent these using
-- different effects.

-- We define an effect for querying Yabai

-- | We only permit three, parameterless queries: looking up all of the
--   displays, spaces or windows. Yabai provides more options, but we prefer to
--   pluck things from the JSON ourselves.
data Query m a where
  GetDisplays :: Query m [DisplayInfo]
  GetSpaces   :: Query m [  SpaceInfo]
  GetWindows  :: Query m [ WindowInfo]

-- | A 'Display' can't be changed, but we can get its ID and any 'Space' values
--   it contains.
data DisplayInfo = DI { dDisplay :: Display
                      , dSpaces  :: [SpaceIndex]
                      }

instance FromJSON DisplayInfo where
  parseJSON = withObject "DisplayInfo" $ \o -> do
    i  <- o .: "index"
    ss <- o .: "spaces"
    pure (DI { dDisplay = DID i
             , dSpaces  = map SIndex ss
             })

-- | A 'Space' always has a 'SpaceIndex' and a 'Display', may be assigned a
--   'SpaceLabel' and can contain any number of 'Window' values. Each 'Display'
--   contains exactly one visible 'Space', and there is always exactly one
--   'Space' that is focused.
data SpaceInfo = SI { sLabel   :: Maybe SpaceLabel
                    , sIndex   :: SpaceIndex
                    , sDisplay :: Display
                    , sWindows :: [Window]
                    , sVisible :: Bool
                    , sFocused :: Bool
                    }

instance FromJSON SpaceInfo where
  parseJSON = withObject "SpaceInfo" $ \o -> do
    l  <- o .: "label"
    i  <- o .: "index"
    d  <- o .: "display"
    ws <- o .: "windows"
    v  <- o .: "visible"
    f  <- o .: "focused"
    pure (SI { sLabel   = if l == "" then Nothing else Just (mkLabel l)
             , sIndex   = SIndex i
             , sDisplay = DID d
             , sWindows = map WID ws
             , sVisible = v == (1 :: Int)
             , sFocused = f == (1 :: Int)
             })

-- | A 'Window' lives on a 'Space', and the 'Display' of that 'Space' is
--   included for convenience, as well as its visibility. Exactly one 'Window'
--   is focused, an it lives on the focused 'Space'.
data WindowInfo = WI { wWindow  :: Window
                     , wDisplay :: Display
                     , wSpace   :: SpaceIndex
                     , wVisible :: Bool
                     , wFocused :: Bool
                     }

instance FromJSON WindowInfo where
  parseJSON = withObject "WindowInfo" $ \o -> do
    i <- o .: "index"
    d <- o .: "display"
    s <- o .: "space"
    v <- o .: "visible"
    f <- o .: "focused"
    pure (WI { wWindow  = WID i
             , wDisplay = DID d
             , wSpace   = SIndex s
             , wVisible = v == (1 :: Int)
             , wFocused = f == (1 :: Int)
             })

-- Run polysemy's boilerplate generator to wrap the constructors of 'Query',
-- giving us an 'ask' function with the appropriate effect type.
makeSem ''Query

-- | Shorthand for values using the 'Query' effect
type Q a = forall r. Member Query r => Sem r a

-- | Common queries

displayCount :: Q Natural
displayCount = fromIntegral . length <$> getDisplays

pluggedIn :: Q Bool
pluggedIn = (== 2) <$> displayCount

lookupSpace :: Space -> Q (Maybe SpaceInfo)
lookupSpace s = head' . filter f <$> getSpaces
  where (f, debug) = case s of
          Left  i -> ((==      i) . sIndex, ("index", show i))
          Right l -> ((== Just l) . sLabel, ("label", show l))

        head' (x:_) = Just x
        head' _     = Nothing

currentDisplay :: Q Display
currentDisplay = sDisplay . head . filter sFocused <$> getSpaces

displayOfSpace :: Space -> Q (Maybe Display)
displayOfSpace s = fmap sDisplay <$> lookupSpace s

spaceOnDisplay :: Space -> Display -> Q Bool
spaceOnDisplay s d = (== Just d) <$> displayOfSpace s

indexOfSpace :: SpaceLabel -> Q (Maybe SpaceIndex)
indexOfSpace l = fmap sIndex <$> lookupSpace (Right l)

labelAtIndex :: SpaceIndex -> Q (Maybe SpaceLabel)
labelAtIndex i = (>>= sLabel) <$> lookupSpace (Left i)

spaceExists :: Space -> Q Bool
spaceExists s = not . null . filter f <$> getSpaces
  where f = case s of
          Left  i -> (==      i) . sIndex
          Right l -> (== Just l) . sLabel

currentSpace :: Q SpaceIndex
currentSpace = sIndex . head . filter sFocused <$> getSpaces

spaceIsVisible :: Space -> Q Bool
spaceIsVisible s = f <$> lookupSpace s
  where f Nothing  = False
        f (Just x) = sVisible x

numberFromLabel :: SpaceLabel -> SpaceIndex
numberFromLabel (SLabel l) = SIndex (read (filter isDigit l))

spaceHasIndex :: SpaceLabel -> SpaceIndex -> Q Bool
spaceHasIndex l i = f <$> lookupSpace (Right l)
  where f Nothing  = False
        f (Just x) = sIndex x == i

spaceIndexMatches :: SpaceLabel -> Q Bool
spaceIndexMatches l = f <$> lookupSpace (Right l)
  where f Nothing  = False
        f (Just x) = sIndex x == numberFromLabel l

currentWindow :: Q Window
currentWindow = wWindow . head . filter wFocused <$> getWindows

lookupWindow :: Window -> Q WindowInfo
lookupWindow w = head . filter ((== w) . wWindow) <$> getWindows

spaceOfWindow :: Window -> Q SpaceIndex
spaceOfWindow w = wSpace <$> lookupWindow w

-- | Get the visible state of the system, so we can restore (parts of) it later.
--   If the visible or focused spaces aren't labelled, we return Nothing, under
--   the assumption that it's not worth storing an improper setup.
visibleState :: Q (Maybe ([(SpaceLabel, Display)], SpaceLabel))
visibleState = do spaces <- getSpaces
                  let visible =   map visInfo $ filter sVisible spaces
                      focused = sLabel . head $ filter sFocused spaces
                  pure $ (,) <$> sequence visible <*> focused
  where visInfo s = (,sDisplay s) <$> sLabel s

-- | Run 'Query' effects in 'IO' by sending them to Yabai.
queryYabai :: Member (Embed IO) r => Sem (Query ': r) a -> Sem r a
queryYabai = interpret (embed . query)
  where query :: Query m a -> IO a
        query GetDisplays = run "--displays"
        query GetSpaces   = run "--spaces"
        query GetWindows  = run "--windows"

        run :: FromJSON a => String -> IO a
        run arg = do
          let cmd = proc "yabai" ["-m", "query", arg]
          (code, sout, serr) <- readCreateProcessWithExitCode cmd ""
          case code of
            ExitFailure c -> err ("Query failed", c, serr)
            ExitSuccess   -> case decode sout of
              Nothing -> err ("Unparseable query result", sout)
              Just x  -> pure x

-- Define the effect for commanding Yabai

-- | The commands we want to send to Yabai
data Command m a where
  FocusWindow :: Either Sentinel Window   -> Command m Bool
  FocusSpace  :: Space                    -> Command m Bool
  MoveWindow  :: Maybe Window -> Space    -> Command m Bool
  MoveSpace   :: Maybe Space  -> Display  -> Command m Bool
  ShiftSpace  :: Maybe Space  -> Sentinel -> Command m Bool

-- | Special directions or places to move a 'Space' or 'Window'
data Sentinel = Previous | Next | First | Last

instance Show Sentinel where
  show Previous = "prev"
  show Next     = "next"
  show First    = "first"
  show Last     = "last"

-- Run polysemy's boilerplate generator to wrap the constructors of 'Command',
-- giving us corresponding definitions, of the appropriate effect type, with a
-- lowercase initial (focus, moveWindow, etc.)
makeSem ''Command

-- | Shorthand for values using the Command' effect, returning success/failure
type C = forall r. Member Command r => Sem r Bool

-- Useful actions

-- | Moves each previously-visible 'Space' back to the 'Display' it was on
restoreVisibleToDisplays :: [(SpaceLabel, Display)] -> C
restoreVisibleToDisplays xs = all id <$> mapM f xs
  where f :: (SpaceLabel, Display) -> C
        f (l, d) = moveSpace (Just (Right l)) d

-- | Focuses the 'Space' that was previously focused.
restoreFocusedSpace :: SpaceLabel -> C
restoreFocusedSpace = focusSpace . label

-- | Bring every previously-visible 'Space' into focus, one at a time. We do not
--   guarantee which 'Space' will be focused after we're finished.
restoreVisibleSpaces :: [(SpaceLabel, Display)] -> C
restoreVisibleSpaces xs = all id <$> mapM (focusSpace . label . fst) xs

-- | Restores which 'Space' is visible on each 'Display', and which is focused.
restoreVisibleState :: ([(SpaceLabel, Display)], SpaceLabel) -> C
restoreVisibleState (visible, focused) = restoreVisibleToDisplays visible >>
                                         restoreVisibleSpaces     visible >>
                                         restoreFocusedSpace      focused

-- | Focus the 'Next' 'Window', cycling around to the 'First' if we run out
nextWindow :: C
nextWindow = do worked <- focusWindow (Left Next)
                if worked
                   then pure worked
                   else focusWindow (Left First)

-- | Focus the 'Previous' 'Window', cycling around to the 'Last' if we run out
prevWindow :: C
prevWindow = do worked <- focusWindow (Left Previous)
                if worked
                   then pure worked
                   else focusWindow (Left Last)

-- | Shorthand for combinations of 'Query' and 'Command'
type QC a = forall r. (Member Query r, Member Command r) => Sem r a

focusDisplay :: Display -> QC Bool
focusDisplay d = do spaces <- getSpaces
                    let find s = sVisible s && sDisplay s == d
                        lspace = filter find spaces
                    case lspace of
                      [space] -> focusSpace (index (sIndex space))
                      _       -> pure False


shiftSpaceToIndex :: Space -> SpaceIndex -> QC ()
shiftSpaceToIndex s want = ql >>= go
  where go :: SpaceLabel -> QC ()
        go l = do c <- done l
                  if c
                     then pure ()
                     else shiftSpace (Just (label l)) Next >> go l

        ql :: Q SpaceLabel
        ql = case s of
          Right x -> pure x
          Left  i -> maybe (err ("No label for space", i)) id <$> labelAtIndex i

        done :: SpaceLabel -> Q Bool
        done l = do mi <- indexOfSpace l
                    case mi of
                      Nothing -> err ("No index for space", l)
                      Just i  -> pure (i == want)

commandToYabaiArgs :: Command m a -> [String]
commandToYabaiArgs c = case c of
  FocusWindow  w -> ["-m", "window", "--focus", either show show w]
  FocusSpace   s -> ["-m", "space" , "--focus", either show show s]
  MoveWindow w s -> concat [ ["-m", "window", "--move"]
                           , maybe [] ((:[]) . show) w
                           , ["--space", show s]
                           ]

-- | Run 'Command' effects in 'IO' by sending them to Yabai
commandYabai :: Member (Embed IO) r => Sem (Command ': r) a -> Sem r a
commandYabai = interpret (embed . command)
  where command :: Command m a -> IO a
        command c = let go = run (commandToYabaiArgs c) in case c of
          FocusWindow _  -> go
          FocusSpace _   -> go
          MoveWindow _ _ -> go

        run :: [String] -> IO Bool
        run args = do
          let cmd = proc "yabai" args
          (code, _, _) <- readCreateProcessWithExitCode cmd ""
          pure $ case code of
            ExitFailure c -> False
            ExitSuccess   -> True

-- | Runner for generated Main modules
mkMain f = runM (commandYabai (queryYabai f)) >>= print
