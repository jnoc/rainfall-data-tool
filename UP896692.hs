--
-- MATHFUN
-- Template for the Haskell assignment program UP896692
--
import Prelude
import Data.Char
import Data.List
import Text.Read

--
-- Types (define Place type here)
--
type Place = (String, (Float, Float), [Int])

testData :: [Place]
testData = [("London",      (51.1, -0.1), [0, 0, 5, 8, 8, 0, 0]),
            ("Cardiff",     (51.5, -3.2), [12, 8, 15, 0, 0, 0, 2]),
            ("Norwich",     (52.6, 1.3),  [0, 6, 5, 0, 0, 0, 3]),
            ("Birmingham",  (52.5, -1.9), [0, 2, 10, 7, 8, 2, 2]),
            ("Liverpool",   (53.4, -3.0), [8, 16, 20, 3, 4, 9, 2]),
            ("Hull",        (53.8, -0.3), [0, 6, 5, 0, 0, 0, 4]),
            ("Newcastle",   (55.0, -1.6), [0, 0, 8, 3, 6, 7, 5]),
            ("Belfast",     (54.6, -5.9), [10, 18, 14, 0, 6, 5, 2]),
            ("Glasgow",     (55.9, -4.3), [7, 5, 3, 0, 6, 5, 0]),
            ("Plymouth",    (50.4, -4.1), [4, 9, 0, 0, 0, 6, 5]),
            ("Aberdeen",    (57.1, -2.1), [0, 0, 6, 5, 8, 2, 0]),
            ("Stornoway",   (58.2, -6.4), [15, 6, 15, 0, 0, 4, 2]),
            ("Lerwick",     (60.2, -1.1), [8, 10, 5, 5, 0, 0, 3]),
            ("St Helier",   (49.2, -2.1), [0, 0, 0, 0, 6, 10, 0])
            ]
            -- ("Portsmouth", (50.8, -1.1), [0, 0, 3, 2, 5, 2, 1])
--
--  Your functional code goes here
--

{-* returnNames - filter out locations
  * returnNorth - filter out just north coords
  * returnEast - filter out just east coords
  * returnRain - filter out just rainfall
  * returnCoordToName - filter out north east coords to name
  * returnRainToName - filter out rainfall to name
  *-}
returnNames :: [Place] -> [String]
returnNames pData = [name | (name, _, _) <- pData]

returnNorth :: [Place] -> [Float]
returnNorth pData = [n | (_, (n, _), _) <- pData]

returnEast :: [Place] -> [Float]
returnEast pData = [e | (_, (_, e), _) <- pData]

returnRain :: [Place] -> [[Int]]
returnRain pData = [rain | (_, _, rain) <- pData]

returnCoordToName :: [Place] -> String -> [(Float, Float)]
returnCoordToName pData x = [(n, e) | (name, (n, e), _) <- pData, x == name]

returnRainToName :: [Place] -> String -> [[Int]]
returnRainToName pData x = [rain | (name, _, rain) <- pData, x == name]

g2 :: String
g2 = "_,\n\t\t  \\_\t(oo)____\n\t\t\t(__)    )\\\n\t\t\t   ||--|| *\n\n"

{-* avg - average function
  * roundTwo - rounds a float to 2 decimal place
  * returnAvgRName - uses avg and roundTwo to take in a location and return the
  * average rainfall of that place
  * retirnAvgRain - returns all places with average rainfall
  *-}
avg :: (Real a, Fractional b) => [a] -> b
avg num = realToFrac (sum num) / (fromIntegral $ length num)

roundTwo :: Float -> Float
roundTwo xs = (fromIntegral (floor (xs * 100))) / 100

returnAvgRName :: [Place] -> String -> [(String, Float)]
returnAvgRName pData x = [(name, roundTwo (avg rain)) | (name, _, rain) <- pData, x == name]

returnAvgPlace :: [Place] -> [(String, (Float, Float), Float)]
returnAvgPlace pData = [(name, ne, roundTwo (avg rain)) | (name, ne, rain) <- pData]

{-* fSpace - works out the space needed to be added to the right of each value taken in
  * returnPlacesToStr - this formats the list into a string which can then be read by putStrLn()
  * returnStrLtoStr - String list to Strings on new lines
  * returnNCLtoStr - Converts a returnAvgRName to string for the points to map
  *-}
fSpace :: Char -> Int -> [Char] -> [Char]
fSpace character spaceAdd value = value ++ replicate (spaceAdd - length value) character

returnPlacesToStr :: [Place] -> String
returnPlacesToStr pData = unlines [intercalate "\t" [fSpace ' ' 10 name, fSpace ' ' 0 (show r1),
                                                     fSpace ' ' 0 (show r2), fSpace ' ' 0 (show r3), fSpace ' ' 0 (show r4),
                                                     fSpace ' ' 0 (show r5), fSpace ' ' 0 (show r6), fSpace ' ' 0 (show r7)]
                                                     | (name, _, [r1, r2, r3, r4, r5, r6, r7]) <- pData]

returnStrLtoStr :: [String] -> String
returnStrLtoStr strL = intercalate ", " strL

returnNCLtoStr :: [(String, Float)] -> String
returnNCLtoStr ncL = unlines [intercalate ", " [fSpace ' ' (length name - 1) name, fSpace ' ' 4 (show number)] | (name, number) <- ncL]

g1 :: String
g1 = "e rain cow!>    \\  \\  \\\n\t\t\\\t\t\\  \\\n\t\t \\\t,_"

{-* returnDryPlaces - takes in the list and day which gives
  * back the places which have had no rainfall on that day
  *-}
returnDryPlaces :: [Place] -> Int -> [String]
returnDryPlaces pData day = [name | (name, _, rain) <- pData, (rain !! (day - 1) ) == 0]

{-* updateRainVals - manipulates only the list of rainfall
  * updateRain - takes each element of [Place] and [newR] adds element to head and removes from end
  * updatePlaceRain - uses Zipwith which applieds updateRain with newR and Place list
  *-}
updateRainVals :: [Int] -> [[Int]]
updateRainVals newR = zipWith (:) newR [init rain | (_, _, rain) <- testData]

updateRain :: Int -> Place -> Place
updateRain newR (name, ne, rain) = (name, ne, newR : init rain)

updatePlaceRain :: [Place] -> [Int] -> [Place]
updatePlaceRain pData newR = zipWith updateRain newR pData

{-* updatePlace - checks if the filtered location
  * matches what to be changed then replaces it with new record
  *-}
updatePlace :: [Place] -> Place -> String -> [Place]
updatePlace pData newP oldP = [if name == oldP then newP else (name, ne, rain) | (name, ne, rain) <- pData ]

{-* dist - distance between two points
  * getMinDist - works out the smallest distance between 2 points from the list
  * returnMinDistDry - filters when it was dry with output of (name, coords)
  * returnClosestDry - filters name to min distance away from the specifc point
  * returnPlaceExist - returns True or False if a place exists or not
  *-}
dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1, y1) (x2, y2) = sqrt((y1 - y2) ^ 2 + (x1 - x2) ^ 2)

getMinDist :: [(String, Float)] -> Float
getMinDist valIn = minimum [dist | (_, dist) <- valIn]

returnMinDistDry :: [Place] -> Int -> (Float, Float) -> Float
returnMinDistDry pData day point = getMinDist [(name, dist point ne) | (name, ne, rain) <- pData, (rain !! (day - 1) ) == 0]

returnClosestDry :: [Place] -> Int -> (Float, Float) -> [String]
returnClosestDry pData day point = [name | (name, ne, _) <- pData, (returnMinDistDry pData day point ) == (dist point ne)]

returnPlaceExist :: String -> [Place] -> Bool
returnPlaceExist place ((name, _, _):xs)
    | name == place   = True
    | xs == []        = False
    | otherwise       = returnPlaceExist place xs

placeExist :: String -> [Place] -> [Bool]
placeExist place pD = [if place == name then True else False | (name, _, _) <- pD]
--
--  Demo
--
demo :: Int -> IO ()
demo 1 = print (returnNames testData)
demo 2 = print (returnAvgRName testData "Cardiff")
demo 3 = putStrLn (returnPlacesToStr testData)
demo 4 = print (returnDryPlaces testData 2)
demo 5 = putStrLn(returnPlacesToStr (updatePlaceRain testData [0,8,0,0,5,0,0,3,4,2,0,8,0,0]))
demo 6 = putStrLn(returnPlacesToStr (updatePlace testData ("Portsmouth", (50.8, -1.1), [0,0,3,2,5,2,1]) "Plymouth"))
demo 7 = print (returnClosestDry testData 1 (50.9, -1.3))
demo 8 = do
  clearScreen
  placesToMap testData 0
{-* 1 - display the names of all the places
  * 2 - display, to two decimal places, the average rainfall in Cardiff
  * 3 - dsplay all place names and their 7-day rainfall figures formatted to 8 neat columns
  * 4 - display the names of all places that were dry two days ago
  * 5 - update the data with most recent rainfall [0,8,0,0,5,0,0,3,4,2,0,8,0,0] (and remove oldest rainfall figures)
  * 6 - replace "Plymouth" with "Portsmouth" which has location 50.8 (N), -1.1 (E) and rainfall 0, 0, 3, 2, 5, 2, 1
  * 7 - display the name of the place closest to 50.9 (N), -1.3 (E) that was dry yesterday
  * 8 - display the rainfall map
  *-}

--
-- Screen Utilities (use these to do the rainfall map - note that these do
-- not work in WinGHCi on Windows, so use GHCi.)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
cs = clearScreen

clearScreen :: IO ()
clearScreen = do
    goTo (0, 0)
    putStr("\ESC[2J")

e1 ::String
e1 = "\t\t\t       _____\n\t\t\t     _(     )_\n\t\t\t    (_________)\n\t<A rar"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show x ++ ";" ++ show y ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr(text)


--
-- Your rainfall map code goes here
--

{-* minA - returns the smallest x or y value
  * maxA - returns the largest x or y value
  * scaleEq - this is the scaling equation for point to console 80w * 50h
  * neToPos - this takes (x,y) and applies the scaling equation to them
  *-}
minA :: [Place] -> [Char] -> Float
minA pData val
    | val == "y"    = minimum (returnNorth pData)
    | val == "x"    = minimum (returnEast pData)
    | otherwise     = 0

maxA :: [Place] -> [Char] -> Float
maxA pData val
    | val == "y"    = maximum (returnNorth pData)
    | val == "x"    = maximum (returnEast pData)
    | otherwise     = 0

scaleEq :: [Place] -> [Char] -> Float -> Integer -> Int
scaleEq pD a b c = floor ((b - minA pD a) * (fromInteger c / (maxA pD a - (minA pD a))))

neToPos :: [Place] -> [(Float, Float)] -> (Int, Int)
neToPos pData [(y1, x1)] = (50 - (scaleEq pData "y" y1 50), scaleEq pData "x" x1 80)

{-* placeToPos - puts a single place to the console
  * placesToMap - runs though the list length and outputs each place to console using placeToPos
  *-}
placeToPos :: String -> [Place] -> IO ()
placeToPos name pData = do
    goTo (neToPos pData (returnCoordToName pData name))
    putStr("[+] " ++ returnNCLtoStr (returnAvgRName pData name))

placesToMap :: [Place] -> Int -> IO ()
placesToMap pD x
    | x < length pD    = do placeToPos (returnNames pD !! x) pD
                            placesToMap pD (x + 1)
                            return ()
    | otherwise        = do goTo (51, 0)

--
-- Your user interface (and loading/saving) code goes here
--

{-* main - initiates the start of the program, loads the places.txt file and hands it to the menu
  * menuGUI - a GUI that shows all the options for a user to pick and takes in an option
  * optionErr - asks the user to enter a valid option until it is satisfied
  *-}
main :: IO ()
main = do
  file <- readFile "places.txt"
  putStrLn(show(length (read file :: [Place])) ++ " Places loaded! Which are:\n" ++ intercalate ", "(returnNames (read file :: [Place])))
  menuGUI (read file)

menuGUI :: [Place] -> IO ()
menuGUI file = do
    putStrLn("[Menu]")
    putStrLn("+===+====================================================================+")
    putStrLn("|   | Places.hs! Here are your options...                                |")
    putStrLn("+===+====================================================================+")
    putStrLn("| 1 | Display all place names                                            |")
    putStrLn("| 2 | Display a specific place average rainfall                          |")
    putStrLn("| 3 | Display all places with 7 day rainfall                             |")
    putStrLn("| 4 | Display specific places that are dry within specified day(s)       |")
    putStrLn("| 5 | Update places with 7 day rainfall data                             |")
    putStrLn("| 6 | Replace an old place with a new place                              |")
    putStrLn("| 7 | Display a place closest to a location that was dry yesterday       |")
    putStrLn("| 8 | Display a map of the UK with place locations and average rainfaill |")
    putStrLn("| Q | Quit the program and save any changes                              |")
    putStrLn("| X | Quit the program without saving                                    |")
    putStrLn("+===+====================================================================+")
    putStr("[Enter] an option value: ")
    opt <- getLine
    putStr("\n")
    display opt file

optionErr :: [Place] -> IO ()
optionErr file = do
    putStrLn("-> Invalid input please retry")
    putStr("[Enter] a correct input option: ")
    opt <- getLine
    putStr("\n")
    display opt file
{-* wait - 'waits' for user input so the menu can be called, also used to pass updated [Place] into menu
  * display - asks for specific input otherwise optionErr is called, off these specific inputs different
  * simulations are run
  *-}
wait :: [Place] -> IO ()
wait file = do
    putStrLn("[Enter] any key to return to menu")
    getLine
    clearScreen
    menuGUI file

display :: String -> [Place] -> IO ()
display "1" file = do
    putStrLn("[1]")
    putStrLn(returnStrLtoStr(returnNames file) ++ "\n")
    wait file
display "2" file = do
    putStrLn("[2]")
    putStrLn(returnStrLtoStr(returnNames file) ++ "\n")
    dispTwo file
    wait file
display "3" file = do
    putStrLn("[3]")
    putStrLn("Name\t\tRainfall days 1 to 7")
    putStrLn(returnPlacesToStr file)
    wait file
display "4" file = do
    putStrLn("[4]")
    dispFour file
    wait file
display "5" file = do
    putStrLn("[5]")
    putStrLn(returnStrLtoStr(returnNames file))
    dispFive file
display "6" file = do
    putStrLn("[6]")
    putStrLn(returnStrLtoStr(returnNames file))
    dispSix file
display "7" file = do
    putStrLn("[7]")
    dispSeven file
    wait file
display "8" file = do
    clearScreen
    putStrLn("[8]")
    placesToMap file 0
    wait file
display "Q" file = do
    leaveProg file
display "X" file = putStrLn("-> No changes were made! Goodbye\n")
display "^v<>" file = do
    putStr(e1 ++ g1 ++ g2)
    wait file
display _ file = do
    optionErr file

{-* leaveProg - saves the updated [Place] to places.txt file and exits the IO ()
  * dispTwo, dispFour, dispFive, dispSix, dispSeven - all run the demo functions
  * corresponding to their number, also check for valid inputs to execute
  *-}
leaveProg :: [Place] -> IO ()
leaveProg file = do
    writeFile "places.txt" (show file)
    putStrLn("-> File saved! Goodbye\n")

dispTwo :: [Place] -> IO ()
dispTwo file = do
    putStr("[Enter] a place name from the places: ")
    name <- getLine
    putStr("\n")
    case returnPlaceExist name file == True of
        True -> do
          putStrLn(returnNCLtoStr(returnAvgRName file name))
        False -> do
          putStrLn("-> Invalid place or input please retry\n")
          dispTwo file

dispFour :: [Place] -> IO ()
dispFour file = do
    putStr("[Enter] a day (1 to 7): ")
    day <- getLine
    putStr("\n")
    case (readMaybe day :: Maybe Int) of
        Just day -> do
            case day <= 0 || day > 7 of
                True -> do
                    putStrLn("-> Invalid day please retry\n")
                    dispFour file
                False -> do
                    putStrLn(returnStrLtoStr(returnDryPlaces file day) ++ "\n")
        Nothing -> do
            putStrLn("-> Invalid input please retry\n")
            dispFour file

dispFive :: [Place] -> IO ()
dispFive file = do
  putStrLn("\n[Enter] rainfall in corresponding order to place,")
  putStr("format as [a,b,c, ... ,z]: ")
  rain <- getLine
  putStr("\n")
  case (readMaybe rain :: Maybe [Int]) of
      Just rain -> do
        case length rain == length file of
            True -> do
                putStr("Your update -> ")
                print(rain)
                putStr("Are you sure you want to submit this list? (y / n) ")
                check <- getLine
                case check == "n" of
                    True -> do
                        dispFive file
                    False -> do
                        case check == "y" || check == "yes" of
                            True -> do
                                let update = (updatePlaceRain file rain)
                                putStrLn("\n-> Update success!\n")
                                wait update
                            False -> do
                                putStrLn("\n-> Invalid input please retry")
                                dispFive file
            False -> do
                putStrLn("-> Invalid length please retry")
                dispFive file
      Nothing -> do
        putStrLn("-> Invalid input please retry")
        dispFive file

dispSix :: [Place] -> IO ()
dispSix file = do
    putStr("\n[Enter] a place you would like to replace: ")
    name <- getLine
    putStr("\n")
    case returnPlaceExist name file == True of
        True -> do
            putStrLn("\n[Enter] the place data you would like to overwrite this location with,")
            putStr("format as (\"name\", (north, east), [rain]): ")
            check <- getLine
            putStr("\n")
            case (readMaybe check :: Maybe Place) of
                Just check -> do
                    let update = (updatePlace file check name)
                    putStrLn("-> Update success!\n")
                    wait update
                Nothing -> do
                    putStrLn("-> Invalid place or input please retry")
                    dispSix file
        False -> do
            putStrLn("-> Invalid place or input please retry\n")
            dispSix file

dispSeven :: [Place] -> IO ()
dispSeven file = do
    putStrLn("[Enter] the coordinates you want to check against,")
    putStr("format as (north, east): ")
    coords <- getLine
    case (readMaybe coords :: Maybe (Float, Float)) of
        Just coords -> do
            putStrLn("\n" ++ returnStrLtoStr(returnClosestDry file 1 coords) ++ "\n")
        Nothing -> do
            putStrLn("\n-> Invalid input please retry\n")
            dispSeven file
