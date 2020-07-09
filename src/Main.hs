{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}


module Main where


import           Data.CountryCodes
import           Data.Proxy
import           Data.Set ( Set )
import qualified Data.Set as S
import           Data.Text hiding (span)
import           Prelude hiding (span)
import           Shpadoinkle hiding (name)
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html
import           Shpadoinkle.Widgets.Types.Core
import           Shpadoinkle.Widgets.Table
import           StockName
import           Test.QuickCheck


data Sex = Male | Female
  deriving (Eq, Ord, Show)


data Person = Person
            { name   :: Text
            , age    :: Int
            , sex    :: Sex
            , origin :: CountryCode }
  deriving (Eq, Show)


data TableFilters = TableFilters
                  { bySex :: Maybe Sex
                  , byOrigin :: Set CountryCode }
  deriving (Eq, Show)


data FilteredTable = FilteredTable
                   { contents :: [Person]
                   , filters  :: TableFilters }
  deriving (Eq, Show)


data instance Row FilteredTable = PersonRow { unRow :: Person }


data instance Column FilteredTable = Name | Age | Sex | Origin
  deriving (Eq, Ord, Show, Bounded, Enum)


instance Humanize (Column FilteredTable) where
  humanize = humanize . show


instance Tabular FilteredTable where
  type Effect FilteredTable m = Monad m

  toRows = fmap PersonRow . contents

  toFilter (filters -> TableFilters {..}) (unRow -> p) = sexFilter && countryFilter
    where sexFilter = case bySex of
            Just s  -> sex p == s
            Nothing -> True
          countryFilter = if S.null byOrigin then True else S.member (origin p) byOrigin

  sortTable (SortCol c s) (unRow -> a) (unRow -> b) =
    case c of
      Name   -> compareOn s (name a)   (name b)
      Age    -> compareOn s (age a)    (age b)
      Sex    -> compareOn s (sex a)    (sex b)
      Origin -> compareOn s (origin a) (origin b)

  toCell tab (unRow -> p) Name   = [text (name p)]
  toCell tab (unRow -> p) Age    = [text (pack (show (age p)))]
  toCell tab (unRow -> p) Sex    = [text (pack (show (sex p)))]
  toCell tab (unRow -> p) Origin = [text (toName (origin p))]


instance Arbitrary Sex where
  arbitrary = oneof [return Male, return Female]


instance Arbitrary CountryCode where
  arbitrary = elements (fst <$> allNames)


instance Arbitrary Person where
  arbitrary = Person <$> (unStockName <$> arbitrary) <*> choose (0,120) <*> arbitrary <*> arbitrary


instance Arbitrary (Row FilteredTable) where
  arbitrary = PersonRow <$> arbitrary


genTable :: IO FilteredTable
genTable = do
  rows <- sequence .  Prelude.take 10000 $ repeat (generate arbitrary)
  return (FilteredTable rows (TableFilters Nothing S.empty))


type Model = (FilteredTable, SortCol FilteredTable)


filterView :: Monad m => Model -> HtmlM m Model
filterView m =
  div_ [
    div_ [
      text "Filter by sex: ",
      span [ onClickE (pur (setSexFilter (Just Male))) ] [ "Male" ],
      text " / ",
      span [ onClickE (pur (setSexFilter (Just Female))) ] [ "Female" ],
      text " / ",
      span [ onClickE (pur (setSexFilter Nothing)) ] [ "Either" ]
    ],
    div_ [
      text "Filter by country of origin:",
      div_ $ originWidget m <$> allNames
    ]
  ]

  where
    setSexFilter :: Maybe Sex -> Model -> Model
    setSexFilter f (tab, sc) = (tab { filters = (filters tab) { bySex = f } }, sc)

    originWidget :: Monad m => Model -> (CountryCode, Text) -> HtmlM m Model
    originWidget (tab, sc) (cc, cName) = div_ [
      input' [ ("type", "checkbox")
             , checked $ S.member cc (byOrigin (filters tab))
             , onClickE (pur (toggleOriginFilter cc)) ],
      text cName ]

    toggleOriginFilter :: CountryCode -> Model -> Model
    toggleOriginFilter cc (tab, sc) =
      if S.member cc (byOrigin (filters tab))
      then ( tab { filters = (filters tab) { byOrigin = S.delete cc (byOrigin (filters tab)) } }
           , sc )
      else ( tab { filters = (filters tab) { byOrigin = S.insert cc (byOrigin (filters tab)) } }
           , sc )


mainView :: Monad m => Model -> HtmlM m Model
mainView m = div_ [
    filterView m,
    uncurry (view Proxy) m
  ]


main :: IO ()
main = do
  tab <- genTable
  runJSorWarp 8080 $
    simple runParDiff (tab, SortCol Name ASC) mainView getBody
