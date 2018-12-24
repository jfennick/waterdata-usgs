{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module Records where

import Data.Aeson (FromJSON)
import Data.Vector (Vector)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Record1 = Record1 {
  name :: !Text
, declaredType :: !Text
, scope :: !Text
, value :: RecordValue
, nil :: Bool
, globalScope :: Bool
, typeSubstituted :: Bool} deriving (Show,Generic)
instance FromJSON Record1

data RecordValue = RecordValue {
  queryInfo :: RecordQueryInfo
, timeSeries :: Vector RecordTimeSeries} deriving (Show, Generic)
instance FromJSON RecordValue

data RecordQueryInfo = RecordQueryInfo {
  queryURL :: !Text
, criteria :: RecordCriteria
, note :: Vector RecordNote} deriving (Show, Generic)
instance FromJSON RecordQueryInfo

data RecordCriteria = RecordCriteria {
  locationParam :: !Text
, variableParam :: !Text
, timeParam :: RecordTimeParam
, parameter :: Vector ()} deriving (Show, Generic)
instance FromJSON RecordCriteria

data RecordTimeParam = RecordTimeParam {
  beginDateTime :: !Text
, endDateTime :: !Text} deriving (Show, Generic)
instance FromJSON RecordTimeParam 

data RecordNote = RecordNote {
  value :: !Text
, title :: !Text} deriving (Show, Generic)
instance FromJSON RecordNote

data RecordTimeSeries = RecordTimeSeries {
  sourceInfo :: RecordSourceInfo
, variable :: RecordVariable
, values :: Vector RecordValues
, name :: !Text} deriving (Show, Generic)
instance FromJSON RecordTimeSeries

data RecordSourceInfo = RecordSourceInfo {
  siteName :: !Text
, siteCode :: Vector RecordSiteCode
, timeZoneInfo :: RecordTimeZoneInfo
, geoLocation :: RecordGeoLocation
, note :: Vector ()
, siteType :: Vector ()
, siteProperty :: Vector RecordSiteProperty} deriving (Show, Generic)
instance FromJSON RecordSourceInfo

data RecordSiteCode = RecordSiteCode {
  value :: !Text
, network :: !Text
, agencyCode :: !Text} deriving (Show, Generic)
instance FromJSON RecordSiteCode

data RecordTimeZoneInfo = RecordTimeZoneInfo {
  defaultTimeZone :: RecordTimeZone
, daylightSavingsTimeZone :: RecordTimeZone
, siteUsesDaylightSavingsTime :: Bool} deriving (Show, Generic)
instance FromJSON RecordTimeZoneInfo

data RecordTimeZone = RecordTimeZone {
  zoneOffset :: !Text
, zoneAbbreviation :: !Text} deriving (Show, Generic)
instance FromJSON RecordTimeZone

data RecordGeoLocation = RecordGeoLocation {
  geogLocation :: RecordGeogLocation -- Spelling is correct. (It is misspelled on waterdata.usgs.gov)
, localSiteXY :: Vector ()} deriving (Show, Generic)
instance FromJSON RecordGeoLocation

data RecordGeogLocation = RecordGeoglocation {
  srs :: !Text
, latitude :: Double
, longitude :: Double} deriving (Show, Generic)
instance FromJSON RecordGeogLocation

data RecordSiteProperty = RecordSiteProperty {
  value :: !Text,
  name :: !Text} deriving (Show, Generic)
instance FromJSON RecordSiteProperty

data RecordVariable = RecordVariable {
  variableCode :: Vector RecordVariableCode
, variableName :: !Text
, variableDescription :: !Text
, valueType :: !Text
, unit :: RecordUnit
, options :: RecordOptions
, note :: Vector ()
, noDataValue :: Double
, variableProperty :: Vector ()
, oid :: !Text} deriving (Show, Generic)
instance FromJSON RecordVariable

data RecordVariableCode = RecordVariableCode {
  value :: !Text
, network :: !Text
, vocabulary :: !Text
, variableID :: Double
--, default :: Bool --default is a keyword...?
} deriving (Show, Generic)
instance FromJSON RecordVariableCode

data RecordUnit = RecordUnit {
  unitCode :: !Text} deriving (Show, Generic)
instance FromJSON RecordUnit

data RecordOptions = RecordOptions {
  option :: Vector RecordOption} deriving (Show, Generic)
instance FromJSON RecordOptions

data RecordOption = RecordOption {
  name :: !Text
, optionCode :: !Text} deriving (Show, Generic)
instance FromJSON RecordOption

data RecordValues = RecordValues {
  value :: Vector RecordValue2
, qualifier :: Vector RecordQualifier
, qualityControlLevel :: Vector ()
, method :: Vector RecordMethod
, source :: Vector ()
, offset :: Vector ()
, sample :: Vector ()
, censorCode :: Vector ()} deriving (Show, Generic)
instance FromJSON RecordValues

data RecordValue2 = RecordValue2 {
  value :: !Text
, qualifiers :: !(Vector Text)
, dateTime :: !UTCTime} deriving (Show, Generic)
instance FromJSON RecordValue2

data RecordQualifier = RecordQualifier {
  qualifierCode :: !Text
, qualifierDescription :: !Text
, qualifierID :: Integer
, network :: !Text
, vocabulary :: !Text} deriving (Show, Generic)
instance FromJSON RecordQualifier

data RecordMethod = RecordMethod {
  methodDescription :: !Text
, methodID :: Integer} deriving (Show, Generic)
instance FromJSON RecordMethod
