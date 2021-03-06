module Main where

import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List ((:), toUnfoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.String (fromCharArray)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude ((<<<), ($), (<>), (<$>), (*>), class Show, Unit, bind, pure, show)
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.Combinators (lookAhead, many, many1)
import Text.Parsing.StringParser.String (anyDigit, anyLetter, char, eof)

--------------------------------------------------------------------------------

data NumberOrString =
    NOSS String
  | NOSI Int

instance showNOS :: Show NumberOrString where
  show (NOSS s) = "NOSS " <> s -- alternatively `show s`
  show (NOSI n) = "NOSI " <> show n

--------------------------------------------------------------------------------

type Major = Int
type Minor = Int
type Patch = Int

type Release  = Array NumberOrString
type Metadata = Array NumberOrString

--------------------------------------------------------------------------------

data SemVer =
  SemVer Major Minor Patch Release Metadata

instance showSemVer :: Show SemVer where
  show (SemVer maj min pat rel met) =
    "SemVer " <> show maj <> " "
              <> show min <> " "
              <> show pat <> " "
              <> show rel <> " "
              <> show met

--------------------------------------------------------------------------------

parseSemVer :: Parser SemVer
parseSemVer = do
  major /\ minor /\ patch <- parseMajMinPat

  rel <- (char '-' *> parseNOSBlock) <|>
         (lookAhead $ char '+' *> pure []) <|>
         (eof *> pure [])

  met <- (char '+' *> parseNOSBlock) <|>
         (eof *> pure [])

  pure $ SemVer major minor patch rel met

  where
    maybeArr :: forall a b. (a -> Parser (Array b)) -> Maybe a -> Parser (Array b)
    maybeArr = maybe (pure [])

--------------------------------------------------------------------------------

parseMajMinPat :: Parser (Major /\ Minor /\ Patch)
parseMajMinPat = do
  major <- anyInt
  _     <- char '.'
  minor <- anyInt
  _     <- char '.'
  patch <- anyInt
  pure $ major /\ minor /\ patch

--------------------------------------------------------------------------------

parseNOSBlock :: Parser (Array NumberOrString)
parseNOSBlock = do
  rel  <- parseNOS
  rels <- many helper
  pure <<< toUnfoldable $ rel : rels

  where
    helper :: Parser NumberOrString
    helper = do
      _ <- char '.'
      parseNOS

parseNOS :: Parser NumberOrString
parseNOS =
      NOSS <<< fromCharArray <<< toUnfoldable <$> many1 anyLetter
  <|> NOSI <$> anyInt

--------------------------------------------------------------------------------

anyInt :: Parser Int
anyInt = try do
  d <- fromCharArray <<< toUnfoldable <$> many1 anyDigit
  case (fromString d) of
    Nothing  -> fail $ "Character " <> show d <> " is not an int"
    Just int -> pure int

--------------------------------------------------------------------------------

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let parsed = runParser parseSemVer "1.1.1+1"
  case parsed of
    Left  e -> logShow e
    Right s -> logShow s
