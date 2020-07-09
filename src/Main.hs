{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}


module Main where


import           Data.CountryCodes
import           Data.Set ( Set )
import qualified Data.Set as S
import           Data.Text
import           Shpadoinkle hiding (name)
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html
import           Shpadoinkle.Widgets.Table
import           Test.QuickCheck


data Sex = Male | Female
  deriving (Eq, Ord)


data Person = Person
            { name   :: Text
            , age    :: Int
            , sex    :: Sex
            , origin :: CountryCode }


data TableFilters = TableFilters
                  { bySex :: Maybe Sex
                  , byCountry :: Set CountryCode }


data FilteredTable = FilteredTable
                   { contents :: [Person]
                   , filters  :: TableFilters }


data instance Row FilteredTable = PersonRow { unRow :: Person }


data instance Column FilteredTable = Name | Age | Sex | Origin


instance Tabular FilteredTable where
  type Effect FilteredTable m = Monad m

  toRows = fmap PersonRow . contents

  toFilter (filters -> TableFilters {..}) (unRow -> p) = sexFilter && countryFilter
    where sexFilter = case bySex of
            Just s  -> sex p == s
            Nothing -> True
          countryFilter = if S.null byCountry then True else S.member (origin p) byCountry

  sortTable (SortCol c s) (unRow -> a) (unRow -> b) =
    case c of
      Name   -> compareOn s (name a)   (name b)
      Age    -> compareOn s (age a)    (age b)
      Sex    -> compareOn s (sex a)    (sex b)
      Origin -> compareOn s (origin a) (origin b)


view :: () -> Html ()
view _ = "hello world"


main :: IO ()
main = runJSorWarp 8080 $
  simple runParDiff () (constly' . Main.view) getBody
