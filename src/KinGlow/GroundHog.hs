module KinGlow.GroundHog 
  where

import Database.Groundhog.TH

data PlaceName = PlaceName 
    { _placeNameValue :: ByteString 
    } deriving (Eq,Show)

data PlaceNameTag =
      Country
    | State
    | County
    | Zip
    | City
    | District
    | Street
    | HouseNumber
    deriving (Eq,Show)

data PlaceNamePart = PlaceNamePart
    { _placeNameTag  :: PlaceNameTag
    , _placeNamePart :: ByteString
    , _placeName :: DefaultKey PlaceName
    } deriving (Eq,Show)

mkPersist defaultCodegenConfig [groundhog|
- entity: PlaceName
- entity: PlaceNamePart
|]
