import System.IO
import Data.Time
import Data.List
import Control.Monad

data Vehicle = Vehicle
  { licensePlate :: String
  , entryTime    :: UTCTime
  , exitTime     :: Maybe UTCTime
  } deriving (Show, Read)

type ParkingLot = [Vehicle]

fileName :: String
fileName = "Parqueadero.txt"

main :: IO ()
main = do
  parkingLot <- loadParkingLot
  menu parkingLot

menu :: ParkingLot -> IO ()
menu parkingLot = do
  putStrLn "\nParking Lot Management System"
  putStrLn "1. Check In"
  putStrLn "2. Check Out"
  putStrLn "3. Search by License Plate"
  putStrLn "4. List Vehicles"
  putStrLn "5. Exit"
  putStr "Choose an option: "
  hFlush stdout
  option <- getLine
  case option of
    "1" -> do
      parkingLot' <- checkIn parkingLot
      menu parkingLot'
    "2" -> do
      parkingLot' <- checkOut parkingLot
      menu parkingLot'
    "3" -> do
      searchByLicensePlate parkingLot
      menu parkingLot
    "4" -> do
      listVehicles parkingLot
      menu parkingLot
    "5" -> saveParkingLot parkingLot
    _   -> do
      putStrLn "Invalid option. Please try again."
      menu parkingLot

checkIn :: ParkingLot -> IO ParkingLot
checkIn parkingLot = do
  putStr "Enter license plate: "
  hFlush stdout
  plate <- getLine
  currentTime <- getCurrentTime
  let newVehicle = Vehicle plate currentTime Nothing
  let updatedParkingLot = newVehicle : parkingLot
  putStrLn $ "Vehicle " ++ plate ++ " checked in at " ++ show currentTime
  return updatedParkingLot

checkOut :: ParkingLot -> IO ParkingLot
checkOut parkingLot = do
  putStr "Enter license plate: "
  hFlush stdout
  plate <- getLine
  currentTime <- getCurrentTime
  let (found, rest) = partition ((== plate) . licensePlate) parkingLot
  case found of
    [] -> do
      putStrLn "Vehicle not found."
      return parkingLot
    (v:vs) -> do
      let updatedVehicle = v { exitTime = Just currentTime }
      let updatedParkingLot = updatedVehicle : rest
      putStrLn $ "Vehicle " ++ plate ++ " checked out at " ++ show currentTime
      putStrLn $ "Total time parked: " ++ show (calculateTime v currentTime)
      return updatedParkingLot

searchByLicensePlate :: ParkingLot -> IO ()
searchByLicensePlate parkingLot = do
  putStr "Enter license plate: "
  hFlush stdout
  plate <- getLine
  let found = filter ((== plate) . licensePlate) parkingLot
  case found of
    [] -> putStrLn "Vehicle not found."
    (v:_) -> putStrLn $ "Vehicle found: " ++ show v

calculateTime :: Vehicle -> UTCTime -> NominalDiffTime
calculateTime vehicle exit = diffUTCTime exit (entryTime vehicle)

listVehicles :: ParkingLot -> IO ()
listVehicles parkingLot = do
  putStrLn "Vehicles in the parking lot:"
  mapM_ print parkingLot

saveParkingLot :: ParkingLot -> IO ()
saveParkingLot parkingLot = do
  writeFile fileName (show parkingLot)
  putStrLn "Parking lot data saved."

loadParkingLot :: IO ParkingLot
loadParkingLot = do
  fileExists <- doesFileExist fileName
  if fileExists
    then do
      content <- readFile fileName
      return (read content)
    else return []
