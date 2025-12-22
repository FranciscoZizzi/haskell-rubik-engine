module UI.CMD (mainLoop, printCube) where

import System.Console.ANSI
import Cube.Data
import Cube.Movements
import qualified Data.Map as Map

colorMap :: Map.Map Int SGR
colorMap = Map.fromList [
  (0, SetColor Foreground Vivid Red), 
  (1, SetColor Foreground Vivid Green), 
  (2, SetPaletteColor Foreground 166), 
  (3, SetColor Foreground Vivid Blue), 
  (4, SetColor Foreground Vivid White), 
  (5, SetColor Foreground Vivid Yellow)]

movementsMap :: Map.Map String (Cube -> Either String Cube)
movementsMap = Map.fromList [
  ("u", upClockwise),
  ("u'", upCounterClockwise),
  ("f", frontClockwise),
  ("f'", frontCounterClockwise),
  ("r", rightClockwise),
  ("r'", rightCounterClockwise),
  ("b", backClockwise),
  ("b'", backCounterClockwise),
  ("l", leftClockwise),
  ("l'", leftCounterClockwise),
  -- ("d", downClockwise), TODO implement
  -- ("d'", downCounterClockwise), TODO implement
  ("m", middleClockwise),
  ("m'", middleCounterClockwise),
  ("x", xClockwise),
  ("x'", xCounterClockwise),
  ("y", yClockwise),
  ("y'", yCounterClockwise),
  ("z", zClockwise),
  ("z'", zCounterClockwise)]

mainLoop :: Cube -> IO ()
mainLoop cube = do
  clearScreen
  printCube cube
  input <- getLine
  maybe 
    (do
      putStrLn "Invalid Movement"
      mainLoop cube)
    (\movement -> case movement cube of
      Right newCube -> mainLoop newCube
      Left message -> do 
        putStrLn message
        mainLoop cube)
    (Map.lookup input movementsMap)

-- PLEASE IGNORE THIS IT'S TEMPORAL I WILL REFACTOR IT I KNOW IT'S UGLY PLEASE PLEASE PLEASE PLEASE PLEAS DON'T MAKE FUN OF ME
printCube :: Cube -> IO ()
printCube cube = do
  putStrLn "            ###################"

  putStr "          #"
  colorPrint "%%%%%" 36
  putStr "#"
  colorPrint "%%%%%" 37
  putStr "#"
  colorPrint "%%%%%" 38
  putStr "#"
  colorPrint "%" 11

  putStrLn "#"
  putStr "        ###################"
  colorPrint "%%%" 11
  putStrLn "#"

  putStr "      #"
  colorPrint "%%%%%" 39
  putStr "#"
  colorPrint "%%%%%" 40
  putStr "#"
  colorPrint "%%%%%" 41
  putStr "#"
  colorPrint "%" 10
  putStr "#"
  colorPrint "%%%" 11
  putStrLn "#"

  putStr "    ###################"
  colorPrint "%%%" 10
  putStr "#"
  colorPrint "%%" 11
  putStrLn "##"

  putStr "  #"
  colorPrint "%%%%%" 42
  putStr  "#"
  colorPrint  "%%%%%" 43
  putStr  "#"
  colorPrint  "%%%%%" 44
  putStr  "#"
  colorPrint  "%" 9
  putStr  "#"
  colorPrint  "%%%" 10
  putStr  "###"
  colorPrint  "%" 14
  putStrLn  "#"

  putStr "###################"
  colorPrint "%%%" 9
  putStr "#"
  colorPrint "%%" 10
  putStr "##"
  colorPrint "%%%" 14
  putStrLn "#"

  putStr "#"
  colorPrint "%%%%%" 0
  putStr "#"
  colorPrint "%%%%%" 1
  putStr "#"
  colorPrint "%%%%%" 2
  putStr "#"
  colorPrint "%%%" 9
  putStr "###"
  colorPrint "%" 13
  putStr "#"
  colorPrint "%" 14
  putStrLn "###"

  putStr "#"
  colorPrint "%%%%%" 0
  putStr "#"
  colorPrint "%%%%%" 1
  putStr "#"
  colorPrint "%%%%%" 2
  putStr "#"
  colorPrint "%%" 9
  putStr "##"
  colorPrint "%%%" 13
  putStr "##"
  colorPrint "%%" 17
  putStrLn "#"

  putStr "#####################"
  colorPrint "%" 12
  putStr "#"
  colorPrint "%" 13
  putStr "###"
  colorPrint "%%%" 17
  putStrLn "#"

  putStr "#"
  colorPrint "%%%%%" 3
  putStr "#"
  colorPrint "%%%%%" 4
  putStr "#"
  colorPrint "%%%%%" 5
  putStr "#"
  colorPrint "%%%" 12
  putStr "##"
  colorPrint "%%" 16
  putStr "#"
  colorPrint "%" 17
  putStrLn "##"

  putStr "#"
  colorPrint "%%%%%" 3
  putStr "#"
  colorPrint "%%%%%" 4
  putStr "#"
  colorPrint "%%%%%" 5
  putStr "#"
  colorPrint "%%" 12
  putStr "##"
  colorPrint "%%%" 16
  putStrLn "##"

  putStr "#####################"
  colorPrint "%" 15
  putStr "#"
  colorPrint "%" 16
  putStrLn "##"

  putStr "#"
  colorPrint "%%%%%" 6
  putStr "#"
  colorPrint "%%%%%" 7
  putStr "#"
  colorPrint "%%%%%" 8
  putStr "#"
  colorPrint "%%%" 15
  putStrLn "##"

  putStr "#"
  colorPrint "%%%%%" 6
  putStr "#"
  colorPrint "%%%%%" 7
  putStr "#"
  colorPrint "%%%%%" 8
  putStr "#"
  colorPrint "%" 15
  putStrLn "##"

  putStrLn "####################"

  putStrLn ""
  where
    colorPrint string = printInColor string cube

setColor :: Cube -> Int -> IO ()
setColor cube stickerPos = 
  case Map.lookup (getStickerColor cube stickerPos) colorMap of
  Just sgr -> setSGR [sgr]
  Nothing -> setSGR [SetColor Foreground Vivid Black]

printInColor :: String -> Cube -> Int -> IO ()
printInColor string cube stickerIndex = do
  setColor cube stickerIndex
  putStr string
  setSGR [Reset]


--             ###################
--           #%%%%%#%%%%%#%%%%%#%#
--         ###################%%%#
--       #%%%%%#%%%%%#%%%%%#%#%%%#
--     ###################%%%#%%##
--   #%%%%%#%%%%%#%%%%%#%#%%%###%#
-- ###################%%%#%%##%%%#
-- #%%%%%#%%%%%#%%%%%#%%%###%#%###
-- #%%%%%#%%%%%#%%%%%#%%##%%%##%%#
-- #####################%#%###%%%#
-- #%%%%%#%%%%%#%%%%%#%%%##%%#%##
-- #%%%%%#%%%%%#%%%%%#%%##%%%##
-- #####################%#%##
-- #%%%%%#%%%%%#%%%%%#%%%##
-- #%%%%%#%%%%%#%%%%%#%##
-- ####################  

--             ###################
--           #     #     #     # #
--         ###################   #
--       #     #     #     # #   #
--     ###################   #  ##
--   #     #     #     # #   ### #
-- ###################   #  ##   #
-- #     #     #     #   ### # ###
-- #     #     #     #  ##   ##  #
-- ##################### # ###   #
-- #     #     #     #   ##  # ##
-- #     #     #     #  ##   ##
-- ##################### # ##
-- #     #     #     #   ##
-- #     #     #     # ##
-- ####################  
