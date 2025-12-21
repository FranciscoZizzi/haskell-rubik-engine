module UI.CMD (printCube) where

import System.Console.ANSI
import Cube.Data
import qualified Data.Map as Map

colorMap :: Map.Map Int SGR
colorMap = Map.fromList [
  (0, SetColor Foreground Vivid Red), 
  (1, SetColor Foreground Vivid Green), 
  (2, SetPaletteColor Foreground 166), 
  (3, SetColor Foreground Vivid Blue), 
  (4, SetColor Foreground Vivid White), 
  (5, SetColor Foreground Vivid Yellow)]

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


-- PLEASE IGNORE THIS IT'S TEMPORAL I WILL REFACTOR IT I KNOW IT'S UGLY PLEASE PLEASE PLEASE PLEASE PLEAS DON'T MAKE FUN OF ME
printCube :: Cube -> IO ()
printCube cube = do
  putStrLn "            ###################"

  putStr "          #"
  colorPrint "%%%%%" 51
  putStr "#"
  colorPrint "%%%%%" 52
  putStr "#"
  colorPrint "%%%%%" 53
  putStr "#"
  colorPrint "%" 11

  putStrLn "#"
  putStr "        ###################"
  colorPrint "%%%" 11
  putStrLn "#"

  putStr "      #"
  colorPrint "%%%%%" 48
  putStr "#"
  colorPrint "%%%%%" 49
  putStr "#"
  colorPrint "%%%%%" 50
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
  colorPrint "%%%%%" 45
  putStr  "#"
  colorPrint  "%%%%%" 46
  putStr  "#"
  colorPrint  "%%%%%" 47
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
    colorPrintLn string = printInColor (string ++ "\n") cube


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
