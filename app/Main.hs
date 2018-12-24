{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

module Main where

import Lib

import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson (decode, eitherDecode)
import Data.Maybe (fromJust)
import Data.Vector (Vector, toList, (!))
import Data.Text (unpack, Text)
import Data.Time (UTCTime)
import Data.List (sortOn)

import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import Records

main :: IO ()
main = do
  bs <- simpleHttp jsonURLKalihi
  --print bs
  --writeToFile bs
  --bs <- readFromFile
  parseWaterData bs
  return ()

writeToFile :: BL.ByteString -> IO()
writeToFile bs = BL.writeFile "jsonfile"  bs

readFromFile :: IO BL.ByteString
readFromFile = BL.readFile "kalihidata"

parseWaterData :: BL.ByteString -> IO ()
parseWaterData jsonbytestring =
  let mvalue :: Either String Record1
      mvalue = eitherDecode jsonbytestring in
    case mvalue of
      Left err -> print err
      Right val -> let ys = map getX $ toList $ queryTimeSeries0 $ val
                       chunks = splitWith (> 5000) ys
                       topchunks = take 6 $ reverse $ sortOn length chunks
                       conv = (toRational(15*60)/1000)
                       diffs = zipWith (\y1 y2 -> toRational (y2-y1) / (15*60)) ys (tail ys)
                       ints = map (\y -> toRational y * conv) (scanl (+) 0 ys)
                       totalDischargeCuFt = toRational (last ints) * conv
                   in
        do
          _ <- GP.plotDefault $ plotdata $ ys
          _ <- GP.plotDefault $ plotdata $ map truncate diffs -- truncate to ints for gnuplot
          _ <- GP.plotDefault $ plotdata $ map truncate ints -- truncate to ints for gnuplot
          print $ "Total discharge Cu Ft " ++ (show totalDischargeCuFt)
          --print $ map length topchunks
          return ()

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f [] = []
splitWith f xs = let dropped = dropWhile (not . f) xs
                     ys = takeWhile f $ dropped in
    ys :  (splitWith f $ dropWhile f $ dropped)

-- TODO: Use Lens
queryTimeSeries0 :: Record1 -> Vector RecordValue2
queryTimeSeries0 r = let
--Note: Due to DuplicateRecordFields we need explicit type signatures for value and values 
  v :: RecordValue
  v = (value :: Record1 -> RecordValue) r
  t :: Vector RecordTimeSeries
  t = timeSeries v
  s :: RecordTimeSeries
  s = t ! 0
  v2 :: Vector RecordValues
  v2 = (values :: RecordTimeSeries -> Vector RecordValues) s
  v3 :: RecordValues
  v3 = v2 ! 0
  vals = (value :: RecordValues -> Vector RecordValue2) v3
  in
  vals

jsonURLKalihi :: String
jsonURLKalihi = "http://waterservices.usgs.gov/nwis/iv/?format=json&sites=16229000&startDT=2018-08-09&endDT=2018-08-10&parameterCd=00060,00065&siteStatus=all"

jsonURLKauKonaHuaNF :: String
jsonURLKauKonaHuaNF = "http://waterservices.usgs.gov/nwis/iv/?format=json&sites=16200000&startDT=2018-08-09&endDT=2018-08-10&parameterCd=00060,00065&siteStatus=all"

jsonURLWaikele :: String
jsonURLWaikele = "http://waterservices.usgs.gov/nwis/iv/?format=json&sites=16213000&startDT=2018-08-09&endDT=2018-08-10&parameterCd=00060,00065&siteStatus=all"

getX :: RecordValue2 -> Integer
getX rv = let dbl = (read :: String -> Double) . unpack . fst . getXY $ rv in
  truncate (dbl * 1000)

getXY :: RecordValue2 -> (Text, UTCTime)
getXY r = ((value :: RecordValue2 -> Text) r, dateTime r)

plotdata :: [Integer] -> Plot2D.T Int Integer
plotdata ys = Plot2D.list Graph2D.listPoints ys
