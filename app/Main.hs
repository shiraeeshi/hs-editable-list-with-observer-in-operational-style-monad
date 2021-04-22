{-# LANGUAGE  GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Main where

import Control.Monad (when, forM_)
import Control.Exception (try)
import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import Control.Monad.State.Strict (StateT, get, modify, runStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import ViewUtils (clearScreen, showInRectangle, clearRectangle, showInGrid, drawGrid, highlightCell, printFromBottom)
import Prelude hiding (log)

data RowData = Row { smth :: String } deriving Eq

initialRows = [
  Row "something a"
  , Row "something b"
  , Row "something c"
  , Row "something d"
  , Row "something e"
  ]

data AppStateData m = AppState
  { rows :: [RowData]
  , activeCellY :: Maybe Int
  , debugMessages :: [String]
  , listeners :: AppStateListenersData m
  }

data AppStateListenersData m = AppStateListeners
  { rowsListeners :: [[RowData] -> m ()]
  , activeCellYListeners :: [Maybe Int -> m ()]
  , debugMessagesListeners :: [[String] -> m ()]
  }

addRowsListener :: Monad m => ([RowData] -> m ()) -> AppStateListenersData m -> AppStateListenersData m
addRowsListener listener (AppStateListeners rowsListeners _activeCellYListeners _debugMessagesListeners) =
  AppStateListeners (listener:rowsListeners) _activeCellYListeners _debugMessagesListeners

addActiveCellYListener :: Monad m => (Maybe Int -> m ()) -> AppStateListenersData m -> AppStateListenersData m
addActiveCellYListener listener (AppStateListeners _rowsListeners activeCellYListeners _debugMessagesListeners) =
  AppStateListeners _rowsListeners (listener:activeCellYListeners) _debugMessagesListeners

addDebugMessagesListener :: Monad m => ([String] -> m ()) -> AppStateListenersData m -> AppStateListenersData m
addDebugMessagesListener listener (AppStateListeners _rowsListeners _activeCellYListeners debugMessagesListeners) =
  AppStateListeners _rowsListeners _activeCellYListeners (listener:debugMessagesListeners)

data EditableListApp a where
  GetList :: EditableListApp [RowData]
  GetActiveCellY :: EditableListApp (Maybe Int)
  GetLogs :: EditableListApp [String]

  UpdateList :: [RowData] -> EditableListApp ()
  UpdateActiveCellY :: (Maybe Int) -> EditableListApp ()
  Log :: String -> EditableListApp ()

  Done :: a -> EditableListApp a
  Bind :: EditableListApp a -> (a -> EditableListApp b) -> EditableListApp b

getList :: EditableListApp [RowData]
getList = GetList

getActiveCellY :: EditableListApp (Maybe Int)
getActiveCellY = GetActiveCellY

getLogs :: EditableListApp [String]
getLogs = GetLogs

updateList :: [RowData] -> EditableListApp ()
updateList = UpdateList

updateActiveCellY :: (Maybe Int) -> EditableListApp ()
updateActiveCellY = UpdateActiveCellY

log :: String -> EditableListApp ()
log = Log

instance Functor EditableListApp where
  fmap f x = x `Bind` (\x' -> Done (f x'))

instance Applicative EditableListApp where
  pure = Done
  f <*> x = f `Bind` (\f' -> x `Bind` (\x' -> Done (f' x')))

instance Monad EditableListApp where
  return = Done
  (>>=) = Bind

newtype DictStateHolder a = Dict (StateT (AppStateData DictStateHolder) IO a) deriving (Functor, Applicative, Monad, MonadIO)

interpret :: EditableListApp a -> DictStateHolder a
interpret GetList = Dict $ rows <$> get
interpret GetActiveCellY = Dict $ activeCellY <$> get
interpret GetLogs = Dict $ debugMessages <$> get

interpret (UpdateList l) = do
  Dict $ modify $ \s -> s { rows = l }
  reacts <- Dict $ (rowsListeners . listeners) <$> get
  forM_ reacts ($ l)
interpret (UpdateActiveCellY y) = do
  Dict $ modify $ \s -> s { activeCellY = y }
  s <- Dict $ get
  let reacts = activeCellYListeners (listeners s)
  forM_ reacts ($ y)
interpret (Log msg) = do
  Dict $ modify $ \s -> s { debugMessages = take debugLinesCount (msg:(debugMessages s)) }
  logs <- Dict $ debugMessages <$> get
  reacts <- Dict $ (debugMessagesListeners . listeners) <$> get
  forM_ reacts ($ logs)

dictStateAction :: AppStateData DictStateHolder -> DictStateHolder a -> IO ()
dictStateAction state (Dict action) = do
  runStateT action state
  return ()

debugLinesCount = 20

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  dictStateAction initialState $ do
    interpret initRows
    loop
  where
    xUpperLeft = 0
    yUpperLeft = 0
    columnCount = 1
    columnWidth = 14
    rowCount = length initialRows

    initialState :: AppStateData DictStateHolder
    initialState = AppState [] Nothing [] initListeners

    initRows :: EditableListApp ()
    initRows = updateList initialRows

    initListeners =
        addRowsListener mainRowsListener
        $ addActiveCellYListener activeCellYListener
        $ addDebugMessagesListener debugMessagesListener
        $ empty
      where
        empty = AppStateListeners [] [] []

    mainRowsListener rows = do
      activeCellCoords <- fmap (\y -> (0, y)) <$> (interpret getActiveCellY)
      liftIO $ showInGrid
                 xUpperLeft
                 yUpperLeft
                 columnCount
                 columnWidth
                 activeCellCoords
                 (map (\row -> [smth row]) rows)
      interpret $ log "updated rows"

    activeCellYListener activeCellY = do
      let activeCellCoords = fmap (\y -> (0, y)) activeCellY
      liftIO $ drawGrid xUpperLeft yUpperLeft columnWidth columnCount rowCount
      case activeCellCoords of
        Nothing -> return ()
        Just coordsPair -> do
          liftIO $ highlightCell xUpperLeft yUpperLeft columnWidth columnCount rowCount coordsPair
          interpret $ log "highlighted cell"

    debugMessagesListener debugMessages = do
      liftIO $ printFromBottom
                 xUpperLeft
                 (yUpperLeft+12+debugLinesCount)
                 debugMessages

    loop = do
      key <- liftIO $ getKey
      when (key /= "\ESC") $ do
        case key of
          "\ESC[A" -> do -- up
              activeCellY <- interpret $ getActiveCellY
              let
                newActiveCellY =
                  case activeCellY of
                    Just y -> Just $ max 0 (y-1)
                    Nothing -> Just 0
              interpret $ updateActiveCellY newActiveCellY
              interpret $ log $ "up, " ++ show(newActiveCellY)
              loop
          "\ESC[B" -> do -- down
              activeCellY <- interpret $ getActiveCellY
              let
                newActiveCellY =
                  case activeCellY of
                    Just y -> Just $ min (rowCount-1) (y+1)
                    Nothing -> Just 0
              interpret $ updateActiveCellY newActiveCellY
              interpret $ log $ "down, " ++ show(newActiveCellY)
              loop
          "\n" -> do -- enter
              activeCellY <- interpret $ getActiveCellY
              rows <- interpret $ getList
                
              let eitherValue =
                    case activeCellY of
                      Nothing -> Left "there's no selected cell"
                      Just cellIndex ->
                        if cellIndex < 0 || cellIndex >= (length rows)
                          then Left $ "index out of bounds: " ++ (show cellIndex)
                          else Right $ smth $ rows !! cellIndex

              let showEditField value = do
                    let
                      txt = "edit cell value:"
                      lentxt = length txt
                      yPos = 0
                      xPos = (columnCount * (columnWidth + 1)) + 3
                      replaceNth lst idx val = if idx < 1 then val:(tail lst) else (head lst) : (replaceNth (tail lst) (idx - 1) val)
                    liftIO $ showInRectangle xPos yPos lentxt [txt, value]
                    key <- liftIO $ getKey
                    case key of
                      "\n" -> do
                        case activeCellY of
                          Nothing -> return ()
                          Just cellIndex -> do
                            liftIO $ clearRectangle xPos yPos lentxt 2
                            rows <- interpret $ getList
                            interpret $ updateList $ replaceNth rows cellIndex (Row value)
                            loop
                      "\DEL" -> showEditField (if (length value) == 0 then value else init value)
                      c -> showEditField (value ++ c)
              case eitherValue of
                Left e -> do
                  interpret $ log $ "error: " ++ (show e)
                  loop
                Right v -> do
                  showEditField v
          "q" -> return ()
          _ -> return ()

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
