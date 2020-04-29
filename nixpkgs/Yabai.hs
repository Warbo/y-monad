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
import Data.Maybe (fromJust, mapMaybe)
import Data.String
import Debug.Trace (trace)
import Numeric.Natural (Natural)
import Polysemy
import Prelude hiding (log)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process.ByteString.Lazy as PBS
import System.Process.ListLike (proc)

-- A type-safe interface for interacting with the Yabai window manager for macOS

-- | Dump out error messages
err :: Show a => a -> b
err = error . show

tr x = if True then id else trace (show x)

-- | Generalises the numeric result of 'length', so we can use it in arithmetic
--   involving Naturals.
len :: (Num n, Foldable f) => f a -> n
len = fromIntegral . length

-- | Show one-more-than the given numeric value. This is useful since Yabai
--   starts counting from 1, but Natural starts from 0. Internally we use the
--   0-based Natural, and use this when outputting commands to Yabai.
incShow :: (Num a, Show a) => a -> String
incShow = show . (1+)

-- | Like 'head' but with a more informative error message
head' :: Show b => b -> [a] -> a
head' info []    = err info
head' _    (x:_) = x

first' :: Show a => a -> (b -> Bool) -> [b] -> b
first' info pred = head' info . filter pred

-- Truncates a list into a Maybe
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing

-- Yabai (and macOS) manage Windows in a 3-level hierarchy. We don't try to keep
-- track of these relationships ourselves; we just query Yabai as needed.

-- | A 'Display' corresponds to a monitor, e.g. one for a laptop's built-in
--   monitor, another for a plugged-in external monitor, etc. Each 'Display'
--   contains at least one 'Space', with consecutive 'SpaceIndex' values.
newtype Display = DID { unDID :: Natural } deriving (Eq, Ord)

instance Num Display where
  fromInteger = DID . fromInteger
  (DID x) + (DID y) = DID (x + y)

instance Show Display where
  show (DID d) = show d

-- | A 'Space' can contain any number of 'Window' values. A 'Space' can always
--   be identified by a 'SpaceIndex'. We can associate a 'Space' with a
--   'SpaceLabel' and use that instead in most situations.
type Space = Either SpaceIndex SpaceLabel

indexS :: SpaceIndex -> Space
indexS = Left

labelS :: SpaceLabel -> Space
labelS = Right

-- | A 'Space' always has a 'SpaceIndex', which gives its position in Mission
--   Control. When a 'Space' is moved, the 'SpaceIndex' of every 'Space' may
--   change, making this an unreliable way to keep track. For this reason, it is
--   usually preferable to use a 'SpaceLabel' instead.
newtype SpaceIndex = SIndex { unSIndex :: Natural } deriving (Eq, Ord)

instance Num SpaceIndex where
  fromInteger = SIndex . fromInteger
  (SIndex x) + (SIndex y) = SIndex (x + y)

instance Real SpaceIndex where

instance Integral SpaceIndex where
  toInteger (SIndex x) = toInteger x

instance Show SpaceIndex where
  show (SIndex i) = show i

instance Enum SpaceIndex where
  toEnum   = fromIntegral
  fromEnum = fromIntegral . toInteger

-- | A 'SpaceLabel' can be assigned to a 'Space', and subsequently used to refer
--   to that 'Space'. The 'SpaceLabel' will remain consistent event if we move
--   a 'Space' around; although they will be lost if Yabai crashes or restarts.
newtype SpaceLabel = SLabel String deriving (Eq, Ord)

-- | A 'SpaceLabel' can't be numeric, or it will be confused with a 'SpaceIndex'
mkLabel :: String -> Maybe SpaceLabel
mkLabel s = case s of
  ""  -> Nothing
  c:_ -> if isDigit c
            then Nothing
            else Just (SLabel s)

instance IsString SpaceLabel where
  fromString s = case mkLabel s of
    Just s  -> s
    Nothing -> err ( ("error" , "Invalid string for SpaceLabel")
                   , ("source", "fromString"                   )
                   , ("string", s                              )
                   )

instance Show SpaceLabel where
  show (SLabel l) = l

-- | For our purposes, a 'Window' is just an ID which we can move from one
--   'Space' to another.
newtype Window  = WID Natural deriving (Eq, Ord)

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
  GetDisplays ::                            Query m [DisplayInfo]
  GetSpaces   ::                            Query m [  SpaceInfo]
  GetWindows  ::                            Query m [ WindowInfo]

-- | A 'Display' can't be changed, but we can get its ID and any 'Space' values
--   it contains.
data DisplayInfo = DI { dDisplay :: Display
                      , dSpaces  :: [SpaceIndex]
                      } deriving (Eq, Show)

instance FromJSON DisplayInfo where
  parseJSON = withObject "DisplayInfo" $ \o -> do
    i  <- o .: "index"
    ss <- o .: "spaces"
    pure (DI { dDisplay = DID (i-1)
             , dSpaces  = map (SIndex . (subtract 1)) ss
             })

newtype Visible = V { unV :: Bool } deriving (Eq, Show)
isVis = V True
invis = V False

newtype Focused = F { unF :: Bool } deriving (Eq, Show)
isFoc = F True
unFoc = F False

-- | A 'Space' always has a 'SpaceIndex' and a 'Display', may be assigned a
--   'SpaceLabel' and can contain any number of 'Window' values. Each 'Display'
--   contains exactly one visible 'Space', and there is always exactly one
--   'Space' that is focused.
data SpaceInfo = SI { sLabel   :: Maybe SpaceLabel
                    , sIndex   :: SpaceIndex
                    , sDisplay :: Display
                    , sWindows :: [Window]
                    , sVisible :: Visible
                    , sFocused :: Focused
                    } deriving (Eq, Show)

instance FromJSON SpaceInfo where
  parseJSON = withObject "SpaceInfo" $ \o -> do
    l  <- o .: "label"
    i  <- o .: "index"
    d  <- o .: "display"
    ws <- o .: "windows"
    v  <- o .: "visible"
    f  <- o .: "focused"
    pure (SI { sLabel   = mkLabel l
             , sIndex   = SIndex (i-1)
             , sDisplay = DID (d-1)
             , sWindows = map WID ws
             , sVisible = V (v == (1 :: Int))
             , sFocused = F (f == (1 :: Int))
             })

-- | A 'Window' lives on a 'Space', and the 'Display' of that 'Space' is
--   included for convenience, as well as its visibility. Exactly one 'Window'
--   is focused, an it lives on the focused 'Space'.
data WindowInfo = WI { wWindow  :: Window
                     , wDisplay :: Display
                     , wSpace   :: SpaceIndex
                     , wVisible :: Visible
                     , wFocused :: Focused
                     } deriving (Eq, Show)

instance FromJSON WindowInfo where
  parseJSON = withObject "WindowInfo" $ \o -> do
    i <- o .: "index"
    d <- o .: "display"
    s <- o .: "space"
    v <- o .: "visible"
    f <- o .: "focused"
    pure (WI { wWindow  = WID i
             , wDisplay = DID (d-1)
             , wSpace   = SIndex (s-1)
             , wVisible = V (v == (1 :: Int))
             , wFocused = F (f == (1 :: Int))
             })

-- Run polysemy's boilerplate generator to wrap the constructors of 'Query',
-- giving us an 'ask' function with the appropriate effect type.
makeSem ''Query

-- | Shorthand for values using the 'Query' effect
type Q r a = Member Query r => Sem r a

-- | Common queries

displayCount :: Q r Natural
displayCount = fromIntegral . length <$> getDisplays

pluggedIn :: Q r Bool
pluggedIn = (== 2) <$> displayCount

lookupSpace :: Space -> Q r (Maybe SpaceInfo)
lookupSpace s = safeHead . filter f <$> getSpaces
  where f = case s of
          Left  i -> (==      i) . sIndex
          Right l -> (== Just l) . sLabel

lookupDisplay :: Display -> Q r (Maybe DisplayInfo)
lookupDisplay d = safeHead . filter f <$> getDisplays
  where f = (== d) . dDisplay

currentDisplay :: Q r Display
currentDisplay = sDisplay <$> focusedSpace

displayOfSpace :: Space -> Q r (Maybe Display)
displayOfSpace s = fmap sDisplay <$> lookupSpace s

spaceOnDisplay :: Space -> Display -> Q r Bool
spaceOnDisplay s d = (== Just d) <$> displayOfSpace s

spacesOnDisplay :: Display -> Q r [SpaceIndex]
spacesOnDisplay d = map sIndex . filter ((== d) . sDisplay) <$> getSpaces

indexOfSpace :: SpaceLabel -> Q r (Maybe SpaceIndex)
indexOfSpace l = fmap sIndex <$> lookupSpace (labelS l)

labelAtIndex :: SpaceIndex -> Q r (Maybe SpaceLabel)
labelAtIndex i = (>>= sLabel) <$> lookupSpace (Left i)

spaceExists :: Space -> Q r Bool
spaceExists s = not . null . filter f <$> getSpaces
  where f = case s of
          Left  i -> (==      i) . sIndex
          Right l -> (== Just l) . sLabel

displayExists :: Display -> Q r Bool
displayExists d = not . null . filter ((== d) . dDisplay) <$> getDisplays

focusedSpace :: Q r SpaceInfo
focusedSpace = first' ( ("error" , "No focused space")
                      , ("source", "focusedSpace"    )
                      )
                      (unF . sFocused)
                      <$> getSpaces

currentSpace :: Q r SpaceIndex
currentSpace = sIndex <$> focusedSpace

spaceIsVisible :: Space -> Q r Visible
spaceIsVisible s = f <$> lookupSpace s
  where f Nothing  = invis
        f (Just x) = sVisible x

spaceIsFocused :: Space -> Q r Focused
spaceIsFocused s = f <$> lookupSpace s
  where f Nothing  = unFoc
        f (Just x) = sFocused x

spaceIsMovable :: Space -> Q r Bool
spaceIsMovable s = do
  si <- fromJust <$> lookupSpace s
  di <- fromJust <$> lookupDisplay (sDisplay si)
  pure (case dSpaces di of
         []  -> False
         [_] -> False
         _   -> True)

numberFromLabel :: SpaceLabel -> SpaceIndex
numberFromLabel (SLabel l) = SIndex (read (filter isDigit l))

infoToSpace :: SpaceInfo -> Space
infoToSpace si = maybe (indexS (sIndex si)) labelS (sLabel si)

spaceHasIndex :: SpaceLabel -> SpaceIndex -> Q r Bool
spaceHasIndex l i = f <$> lookupSpace (labelS l)
  where f Nothing  = False
        f (Just x) = sIndex x == i

spaceIndexMatches :: SpaceLabel -> Q r Bool
spaceIndexMatches l = f <$> lookupSpace (labelS l)
  where f Nothing  = False
        f (Just x) = sIndex x == numberFromLabel l

focusedWindow :: Q r WindowInfo
focusedWindow = do
  windows <- getWindows
  let allFocused = filter (unF . wFocused) windows
      focused    = head' ( ("error"  , "No focused window")
                         , ("source" , "focusedWindow"    )
                         , ("windows", windows            )
                         )
                         allFocused
  pure focused

currentWindow :: Q r Window
currentWindow = wWindow <$> focusedWindow

lookupWindow :: Window -> Q r WindowInfo
lookupWindow w = do
  windows <- getWindows
  let matches = filter ((== w) . wWindow) windows
  pure (head' ( ("error"  , "No window with given ID")
              , ("window" , w                        )
              , ("windows", windows                  )
              )
              matches)

spaceOfWindow :: Window -> Q r SpaceIndex
spaceOfWindow w = wSpace <$> lookupWindow w

-- | Run 'Query' effects in 'IO' by sending them to Yabai.
queryYabai :: Member (Embed IO) r => Sem (Query ': r) a -> Sem r a
queryYabai = interpret (embed . query)
  where query :: Query m a -> IO a
        query GetDisplays     = run "--displays"
        query GetSpaces       = run "--spaces"
        query GetWindows      = run "--windows"

        run :: FromJSON a => String -> IO a
        run arg = do
          let cmd = proc "yabai" ["-m", "query", arg]
          (code, sout, serr) <- readCreateProcessWithExitCode cmd ""
          case code of
            ExitFailure c -> err ("Query failed", c, serr)
            ExitSuccess   -> case decode sout of
              Nothing -> err ("Unparseable query result", sout)
              Just x  -> pure x

-- | Simple logging effect
data Logger m a where
  Log' :: String -> Logger m ()

-- | Effect to get env vars, which can be passed in by calling scripts
data Environment m a where
  Env :: String -> Environment m (Maybe String)

-- Define the effect for commanding Yabai

-- | The commands we want to send to Yabai
data Command m a where
  CreateSpace  ::                             Command m Bool
  DestroySpace ::                             Command m Bool
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
makeSem ''Logger
makeSem ''Environment

log :: (Show a, Member Logger r) => a -> Sem r ()
log x = log' (show x) >> pure ()

consoleLogger :: Member (Embed IO) r => Sem (Logger ': r) a -> Sem r a
consoleLogger = interpret (embed . go)
  where go :: Logger m a -> IO a
        go (Log' s) = hPutStrLn stderr s

sysEnvironment :: Member (Embed IO) r => Sem (Environment ': r) a -> Sem r a
sysEnvironment = interpret (embed . go)
  where go :: Environment m a -> IO a
        go (Env name) = lookupEnv name

-- | Shorthand for values using the Command' effect, returning success/failure
type C = forall r. Member Command r => Sem r Bool

-- Useful actions

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
moveSpaceToDisplay s d = do
  msi <- lookupSpace s
  case msi of
    Nothing -> do log ("Space not found", s)
                  pure False
    Just si -> if sDisplay si == d
                  then pure True
                  else do
      focused <- focusSpace s
      if focused
         then do success <- moveSpace d
                 if success
                    then do mdi <- lookupDisplay d
                            case mdi of
                              Nothing -> do log ("No destination info", d)
                                            pure False
                              Just di -> do
                                let onD = (`elem` dSpaces di) . sIndex
                                labs <- map sLabel . filter onD <$> getSpaces
                                if sLabel si `elem` labs
                                   then pure True
                                   else do log ( "Label not on destination"
                                               , si
                                               , di
                                               )
                                           pure False
                    else do log ("Failed to move", s, d)
                            pure False
         else do log ("Failed to focus for moving", s, d)
                 pure False

{-
focusSpaceOnThisDisplay :: Space -> QC Bool
focusSpaceOnThisDisplay s = do
  moveSpaceToDisplay
-}

visibleSpaces :: Q r [SpaceIndex]
visibleSpaces = map sIndex <$> visibleSpaceInfos

visibleSpaceInfos :: Q r [SpaceInfo]
visibleSpaceInfos = filter (unV . sVisible) <$> getSpaces

-- Whether a 'Space' contains no 'Window' (less impact if destroyed)
emptySpaces :: Q r [SpaceIndex]
emptySpaces = do
  spaces <- getSpaces
  pure . map sIndex . filter (null . sWindows) $ spaces

-- | Shorthand for combinations of 'Query' and 'Command'
type QC a = forall r. (Member Query r, Member Command r, Member Logger r) => Sem r a

sendToOtherDisplay :: SpaceInfo -> QC Bool
sendToOtherDisplay si = do
  c  <- displayCount
  let space = maybe (indexS (sIndex si)) labelS (sLabel si)
  md <- displayOfSpace space
  case (c, md) of
    (2, Just (DID d)) -> do
      let dest = (d + 1) `mod` 2
      moveSpaceToDisplay space (DID dest)

    (_, _     ) -> do log ( ("warning" , "Can't send to other display")
                          , ("space"   , si                           )
                          , ("displays", c                            )
                          )
                      pure False

swapVisibles :: QC Bool
swapVisibles = retainDisplay . retainUserVisible $ do
    visible <- visibleSpaceInfos
    case visible of
      [v1, v2] -> do
        m1 <- spaceIsMovable (indexS (sIndex v1))
        m2 <- spaceIsMovable (indexS (sIndex v2))
        case (m1, m2) of
          (True, _) -> go v1 v2
          (_, True) -> go v2 v1
          _         -> do log "Neither space is movable"
                          pure False

      _ -> do log "Can only swap visibles with two displays"
              pure False

  where go x y = sendToOtherDisplay x >> sendToOtherDisplay y

retainDisplay :: (Member Command r, Member Logger r, Member Query r) => Sem r a -> Sem r a
retainDisplay cmd = do
  d      <- currentDisplay
  result <- cmd
  focusDisplay (Right d)
  pure result

retainFocused :: (Member Command r, Member Logger r, Member Query r) => Sem r a -> Sem r a
retainFocused cmd = do
  s      <- focusedSpace
  result <- cmd
  focusOn s
  pure result

retainVisible :: (Member Command r, Member Logger r, Member Query r) => Sem r a -> Sem r a
retainVisible cmd = do
  vs     <- visibleSpaceInfos
  result <- cmd
  mapM_ focusOn vs
  pure result

retainUserVisible :: (Member Command r, Member Logger r, Member Query r) => Sem r a -> Sem r a
retainUserVisible = retainFocused . retainVisible

focusOn :: SpaceInfo -> QC Bool
focusOn si = focusSpace (case sLabel si of
                          Nothing -> indexS (sIndex si)
                          Just l  -> labelS l)

focusHere :: SpaceInfo -> QC Bool
focusHere si = do
  c <- displayCount
  case c of
    1 -> focusOn si  -- No need to juggle displays, since there's only one
    2 -> do
      d <- currentDisplay
      log ("On display", d)
      if sDisplay si == d
         then log ("Correct display, focusing", si) >> focusOn si
         else do
           result <- onOtherDisplay . withFocused si $ (do pre <- getSpaces
                                                           log ("pre", pre)
                                                           result <- swapVisibles
                                                           post <- getSpaces
                                                           log ("post", post)
                                                           pure result)
           newD <- currentDisplay
           log ("Now on display", newD)
           focusDisplay (Right d)
           pure result
    _ -> failed "focusHere only works with 2 displays"

focusHereEnv :: ( Member Command     r
                , Member Environment r
                , Member Logger      r
                , Member Query       r
                )
             => Sem r Bool
focusHereEnv = do
  ms <- env "LABEL"
  case ms of
    Nothing -> failed "No LABEL env var to focus on"
    Just s  -> do
      case mkLabel s of
        Nothing -> failed ("Invalid LABEL to focus on", s)
        Just l  -> go l False

  where go l giveUp = do
          msi <- lookupSpace (labelS l)
          case msi of
            Nothing -> if giveUp
                          then failed ("Didn't find space with given LABEL", l)
                          else do
                            log ("No space with LABEL, trying to relabel"  , l)
                            labelSpaces
                            go l True

            Just si -> focusHere si

onOtherDisplay :: (Member Command r, Member Logger r, Member Query r) => Sem r a -> Sem r a
onOtherDisplay cmd = retainDisplay (displayNext >> cmd)

withFocused :: (Member Command r, Member Logger r, Member Query r)
            => SpaceInfo
            -> Sem r Bool
            -> Sem r Bool
withFocused new cmd = do
  old <- focusedSpace
  pre <- focusOn new
  if not pre then failed "Failed to focus" else do
     result <- cmd
     focusOn old
     pure result

failed :: Show a => Member Logger r => a -> Sem r Bool
failed msg = log msg >> pure False

-- Keep destroying 'Space' values until there are as many as labels
destroyDownTo :: Natural -> QC ()
destroyDownTo n = do
    spaces <- getSpaces
    if len spaces > n
       then do ms <- destroyableSpace
               case ms of
                 Nothing -> pure ()
                 Just s  -> do focusSpace (indexS s)
                               destroySpace
                               destroyDownTo n
       else pure ()

-- Pick a 'Space' which can be destroyed, preferring empty ones
destroyableSpace :: Q r (Maybe SpaceIndex)
destroyableSpace = do spaces   <- getSpaces
                      empty    <- emptySpaces
                      displays <- getDisplays
                      let preferable = filter (destroyable displays)
                                              empty
                          fallbacks  = filter (destroyable displays)
                                              (map sIndex spaces)
                      pure (case preferable ++ fallbacks of
                             []  -> Nothing
                             x:_ -> Just x)
  where -- We can only destroy a 'Space' if there are others on its 'Display'
        destroyable :: [DisplayInfo] -> SpaceIndex -> Bool
        destroyable displays s =
          let matches = filter ((s `elem`) . dSpaces) displays
              info    = head' ( ("error"   , "No display containing space")
                              , ("source"  , "destroyableSpace"           )
                              , ("displays", displays                     )
                              , ("space"   , s                            )
                              )
                              matches
           in length (dSpaces info) > 1

populateSpaces' :: [SpaceLabel] -> QC ()
populateSpaces' labels = destroyDownTo (len labels) >> populate
  where populate :: QC ()
        populate = do spaces <- getSpaces
                      if length spaces < length labels
                         then createSpace >> populate
                         else pure ()

labelSpaces :: QC ()
labelSpaces = labelSpaces' spaceLabels

labelSpaces' :: [SpaceLabel] -> QC ()
labelSpaces' labels = do populateSpaces'      labels
                         removeDodgyLabels    labels
                         mapM_ ensureLabelled labels

  where ensureLabelled :: SpaceLabel -> QC Bool
        ensureLabelled l = do spaces <- getSpaces
                              case filter ((== Just l) . sLabel) spaces of
                                [_] -> pure True
                                []  -> let isUnlabelled = (== Nothing) . sLabel
                                           unlabelled   = filter isUnlabelled
                                                                 spaces
                                           s = head'
                                             ( ("error", "No unlabelled spaces")
                                             , ("source", "labelSpaces'"       )
                                             , ("spaces", spaces               )
                                             , ("location", "ensureLabelled"   )
                                             , ("label", l                     )
                                             , ("labels", labels               )
                                             )
                                             unlabelled
                                        in labelSpace (sIndex s) l
                                ss  -> err ("Duplicate labels", ss)

removeDodgyLabels :: [SpaceLabel] -> QC ()
removeDodgyLabels labels = do
    spaces <- getSpaces
    removeHelper (mkMapping spaces) labels

  where mkEntry :: SpaceInfo -> Maybe (SpaceLabel, SpaceIndex)
        mkEntry info = (, sIndex info) <$> sLabel info

        mkMapping :: [SpaceInfo] -> [(SpaceLabel, SpaceIndex)]
        mkMapping = mapMaybe mkEntry

        removeHelper :: [(SpaceLabel, SpaceIndex)]
                     -> [SpaceLabel]
                     -> QC ()
        removeHelper []          _      = pure ()
        removeHelper ((l, i):ls) labels = do
          if l `elem` labels
             -- Allow this label, but pop it from the allowed list from now on
             then removeHelper ls (delete l labels)
             -- This label isn't allowed; remove it before continuing
             else do labelSpace i (SLabel "")
                     removeHelper ls labels

-- The space labels we want will get spliced in here during the build
spaceLabels :: [SpaceLabel]
spaceLabels = undefined -- LABELS_GO_HERE

commandToYabaiArgs :: Command m a -> [String]
commandToYabaiArgs c = "-m" : case c of
  CreateSpace    -> ["space", "--create" ]
  DestroySpace   -> ["space", "--destroy"]
  LabelSpace s l -> ["space", show (s+1), "--label", show l]

  FocusWindow  w -> ["window" , "--focus", either    show    show w]
  FocusSpace   s -> ["space"  , "--focus", either incShow    show s]
  FocusDisplay d -> ["display", "--focus", either    show incShow d]

  MoveWindow w s -> concat [ ["window", "--move"]
                           , maybe [] ((:[]) . show) w
                           , ["--space", show s]
                           ]
  SwapWindow s   -> ["window", "--swap"   , show s]

  MoveSpace  d   -> ["space" , "--display", incShow d]

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
mkMain f = go f >>= print
  where go = runM . commandYabai . consoleLogger . queryYabai . sysEnvironment
