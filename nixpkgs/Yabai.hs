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

module Yabai where

import Data.Aeson ((.:), decode, FromJSON(..), withObject)
import Data.Char (isDigit)
import Data.List (delete)
import Data.Maybe (mapMaybe)
import Data.String
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

instance Show Display where
  show (DID d) = show d

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
                      } deriving (Show)

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
                    } deriving (Show)

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
  where f = case s of
          Left  i -> (==      i) . sIndex
          Right l -> (== Just l) . sLabel

        head' (x:_) = Just x
        head' _     = Nothing

currentDisplay :: Q Display
currentDisplay = sDisplay . head . filter sFocused <$> getSpaces

displayOfSpace :: Space -> Q (Maybe Display)
displayOfSpace s = fmap sDisplay <$> lookupSpace s

spaceOnDisplay :: Space -> Display -> Q Bool
spaceOnDisplay s d = (== Just d) <$> displayOfSpace s

spacesOnDisplay :: Display -> Q [SpaceIndex]
spacesOnDisplay d = map sIndex . filter ((== d) . sDisplay) <$> getSpaces

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
  CreateSpace  ::                            Command m Bool
  DestroySpace ::                            Command m Bool
  LabelSpace   :: SpaceIndex -> SpaceLabel -> Command m Bool
  FocusWindow  :: Either Sentinel Window   -> Command m Bool
  FocusSpace   :: Space                    -> Command m Bool
  FocusDisplay :: Either Sentinel Display  -> Command m Bool
  MoveWindow   :: Maybe Window -> Space    -> Command m Bool
  SwapWindow   :: Sentinel                 -> Command m Bool
  MoveSpace    :: Display                  -> Command m Bool

-- | The 'Command' effect is polymorphic, but its constructors only use 'Bool'.
--   To handle a 'Command' we may need to use 'Bool' functions, but they're not
--   polymorphic enough for a generic handler. This function casts them, taking
--   advantage of the fact that 'Command m a' is always 'Command m Bool'.
castCommand :: Command m a -> Bool -> a
castCommand c b = case c of
  CreateSpace      -> b
  DestroySpace     -> b
  LabelSpace   _ _ -> b
  FocusWindow  _   -> b
  FocusSpace   _   -> b
  FocusDisplay _   -> b
  MoveWindow   _ _ -> b
  SwapWindow   _   -> b
  MoveSpace    _   -> b

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
restoreVisibleToDisplays :: [(SpaceLabel, Display)] -> QC Bool
restoreVisibleToDisplays xs = all id <$> mapM f xs
  where f :: (SpaceLabel, Display) -> QC Bool
        f (l, d) = moveSpaceToDisplay (Right l) d

-- | Focuses the 'Space' that was previously focused.
restoreFocusedSpace :: SpaceLabel -> C
restoreFocusedSpace = focusSpace . label

-- | Bring every previously-visible 'Space' into focus, one at a time. We do not
--   guarantee which 'Space' will be focused after we're finished.
restoreVisibleSpaces :: [(SpaceLabel, Display)] -> C
restoreVisibleSpaces xs = all id <$> mapM (focusSpace . label . fst) xs

-- | Restores which 'Space' is visible on each 'Display', and which is focused.
restoreVisibleState :: ([(SpaceLabel, Display)], SpaceLabel) -> QC Bool
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

moveWindowNext :: C
moveWindowNext = do worked <- swapWindow Next
                    if worked
                       then pure worked
                       else swapWindow First

moveWindowPrev :: C
moveWindowPrev = do worked <- swapWindow Previous
                    if worked
                       then pure worked
                       else swapWindow Last

displayNext :: C
displayNext = do worked <- focusDisplay (Left Next)
                 if worked
                    then pure worked
                    else focusDisplay (Left First)

displayPrev :: C
displayPrev = do worked <- focusDisplay (Left Previous)
                 if worked
                    then pure worked
                    else focusDisplay (Left Last)

moveSpaceToDisplay :: Space -> Display -> QC Bool
moveSpaceToDisplay s d = do vis     <- visibleState
                            focused <- focusSpace s
                            moved   <- if focused
                                          then moveSpace d
                                          else pure False
                            -- Restore the previous state, except for displays,
                            -- if we managed to store it. If not, oh well.
                            case (moved, vis) of
                              (False, _          ) -> pure moved
                              (True, Nothing     ) -> pure moved
                              (True, Just (vs, f)) -> do restoreVisibleSpaces vs
                                                         restoreFocusedSpace  f
                                                         pure moved

-- | Shorthand for combinations of 'Query' and 'Command'
type QC a = forall r. (Member Query r, Member Command r) => Sem r a

populateSpaces :: QC ()
populateSpaces = destroyBloat >> populate
  where -- Keep destroying 'Space' values until there are as many as spaceLabels
        destroyBloat :: QC ()
        destroyBloat = do spaces <- getSpaces
                          if length spaces > length spaceLabels
                             then do s <- destroyableSpace
                                     focusSpace (index s)
                                     destroySpace
                                     destroyBloat
                             else pure ()

        -- Whether a 'Space' contains no 'Window' (less impact if destroyed)
        emptySpaces :: [SpaceInfo] -> [SpaceIndex]
        emptySpaces = map sIndex . filter (null . sWindows)

        -- We can only destroy a 'Space' if there are others on its 'Display'
        destroyable :: [DisplayInfo] -> SpaceIndex -> Bool
        destroyable displays s = let info = head                          .
                                            filter ((s `elem`) . dSpaces) $
                                            displays
                                  in not (null (dSpaces info))

        -- Pick a 'Space' which can be destroyed, preferring empty ones
        destroyableSpace :: Q SpaceIndex
        destroyableSpace = do spaces   <- getSpaces
                              displays <- getDisplays
                              let preferable = filter (destroyable displays)
                                                      (emptySpaces spaces)
                                  fallbacks  = filter (destroyable displays)
                                                      (map sIndex spaces)
                              pure (head (preferable ++ fallbacks))

        populate :: QC ()
        populate = do spaces <- getSpaces
                      if length spaces < length spaceLabels
                         then createSpace >> populate
                         else pure ()

{-
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
-}

labelSpaces :: QC ()
labelSpaces = do spaces <- getSpaces
                 removeDodgyLabels spaces
                 mapM_ ensureLabelled spaceLabels

  where removeDodgyLabels :: [SpaceInfo] -> C
        removeDodgyLabels spaces = let f info = (,sIndex info) <$> sLabel info
                                       labels = mapMaybe f spaces
                                    in removeHelper spaces labels spaceLabels

        removeHelper :: [SpaceInfo]
                     -> [(SpaceLabel, SpaceIndex)]
                     -> [SpaceLabel]
                     -> C
        removeHelper _      []          _      = pure True
        removeHelper spaces ((l, i):ls) labels =
          if l `elem` labels
             then removeHelper spaces ls (delete l labels)
             else do labelSpace i (SLabel "")
                     removeHelper spaces ls labels

        ensureLabelled :: SpaceLabel -> QC Bool
        ensureLabelled l = do spaces <- getSpaces
                              case filter ((== Just l) . sLabel) spaces of
                                [_] -> pure True
                                []  -> let unlabelled = (== Nothing) . sLabel
                                           s = head . filter unlabelled $ spaces
                                        in labelSpace (sIndex s) l
                                ss  -> err ("Duplicate labels", ss)

-- The space labels we want will get spliced in here during the build
spaceLabels :: [SpaceLabel]
spaceLabels = LABELS_GO_HERE

commandToYabaiArgs :: Command m a -> [String]
commandToYabaiArgs c = "-m" : case c of
  CreateSpace    -> ["space", "--create" ]
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

-- | Run 'Command' effects in 'IO' by sending them to Yabai
commandYabai :: Member (Embed IO) r => Sem (Command ': r) a -> Sem r a
commandYabai = interpret (embed . command)
  where command :: Command m a -> IO a
        command c = castCommand c <$> run (commandToYabaiArgs c)

        run :: [String] -> IO Bool
        run args = do
          let cmd = proc "yabai" args
          (code, _, _) <- readCreateProcessWithExitCode cmd ""
          pure $ case code of
            ExitFailure _ -> False
            ExitSuccess   -> True

-- | Runner for generated Main modules
mkMain f = runM (commandYabai (queryYabai f)) >>= print
