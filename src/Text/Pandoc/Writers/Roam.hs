{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{- |
   Module      : Text.Pandoc.Writers.Roam
   Copyright   : Copyright (C) 2021 Maciek Makowski
   License     : GNU GPL, version 2 or above

   Maintainer  : Maciek Makowski <maciek@mmakowski.com>
   Stability   : alpha
   Portability : portable

Roam Research JSON writer for pandoc.

-}
module Text.Pandoc.Writers.Roam ( writeRoam )
where
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import GHC.Generics
import Text.Pandoc.Options
import Text.Pandoc.Definition
    ( lookupMeta,
      nullMeta,
      Block(..),
      Format(Format),
      Inline(Image, Str),
      Meta(Meta),
      MetaValue(MetaInlines, MetaMap),
      Pandoc(..) )
import Data.Ipynb as Ipynb
import Text.Pandoc.Walk (walkM)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad
import Text.Pandoc.Logging
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson as Aeson
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Shared (safeRead, isURI)
import Text.Pandoc.Writers.Shared (metaToContext')
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Types as AT
import Data.Aeson.Encode.Pretty (Config(..), defConfig,
           encodePretty', keyOrder, Indent(Spaces))
import Text.DocLayout (literal)

writeRoam :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeRoam opts d = do
  page <- pandocToRoam opts d
  return $ TE.decodeUtf8 . BL.toStrict . encodePretty' defConfig{
             confIndent  = Spaces 1,
             confTrailingNewline = True,
             confCompare = keyOrder [ "title", "string", "heading", "text-align", "children" ]
             }
         $ [page]


-- data types ---------------------------------------------------------------------

data RoamPage = RoamPage {
      title :: Text
    , pChildren :: [RoamBlock]
    } deriving (Generic, Show)

data RoamBlock = RoamBlock {
      string :: Text
    , text_align :: Maybe TextAlign
    , heading :: Maybe Int
    , children :: Maybe [RoamBlock]
    } deriving (Generic, Show)

data TextAlign = Justify
  deriving (Generic, Show)

instance ToJSON RoamPage where
  toJSON (RoamPage title children) =
    AT.object [
      "title" .= title
    , "children" .= children
    ]

instance ToJSON RoamBlock where
  toJSON = genericToJSON defaultOptions {
    omitNothingFields = True,
    fieldLabelModifier = T.unpack . T.replace "_" "-" . T.pack
  }

instance ToJSON TextAlign where
  toJSON Justify = AT.String "justify"

mkBlock :: RoamBlock
mkBlock = RoamBlock {
  string = "",
  text_align = Nothing,
  heading = Nothing,
  children = Nothing
}

mkRaw :: Text -> RoamBlock
mkRaw txt = mkBlock {
  string = txt
}

mkPara :: Text -> RoamBlock
mkPara txt = mkBlock {
  string = txt,
  text_align = Just Justify
}

mkHead :: Text -> Int -> RoamBlock
mkHead txt lvl = mkBlock {
  string = txt,
  heading = Just lvl
}

mkCode :: [Text] -> Text -> RoamBlock
mkCode lang code = mkBlock {
  string = T.concat $ ["```"] ++ lang ++ ["\n", code, "```"]
}


-- conversion ---------------------------------------------------------------------

pandocToRoam :: PandocMonad m
             => WriterOptions -> Pandoc -> m RoamPage
pandocToRoam opts (Pandoc meta blocks) = do
  -- let blockWriter bs = literal <$> writeMarkdown
  --          opts{ writerTemplate = Nothing } (Pandoc nullMeta bs)
  let inlineWriter ils = T.stripEnd <$> writeMarkdown
           opts{ writerTemplate = Nothing } (Pandoc nullMeta [Plain ils])
  title <- inlineWriter (B.docTitle meta)  -- TODO: in most cases title will not be present in meta, just in the file name; fill out later
  blks <- roamBlocks inlineWriter blocks
  return (RoamPage title blks)

roamBlocks :: PandocMonad m
           => ([Inline] -> m Text) -> [Block] -> m [RoamBlock]
roamBlocks _ [] = return []
roamBlocks inlineWriter (b : bs) = do
  rbs <- case b of
    Plain ils -> textBlock inlineWriter ils
    Para ils -> textBlock inlineWriter ils
    LineBlock ilss -> textBlock inlineWriter (concat ilss)
    CodeBlock (_, lang, _) txt -> return [mkCode lang txt]
    RawBlock fmt txt -> return [mkRaw txt, mkRaw (T.pack $ show fmt)]  -- TODO: what's in the format?
    BlockQuote xs -> roamBlocks inlineWriter xs  -- TODO: wrap in quote
    OrderedList lattr xss -> do
      flatList <- roamBlocks inlineWriter (concat xss)  -- TODO: do not flatten
      return $ flatList ++ [mkRaw (T.pack $ show lattr)]
    BulletList xss -> roamBlocks inlineWriter (concat xss)  -- TODO: make each xs a child of an empty block
    DefinitionList _ -> return [mkRaw "TODO CONV: definition list"]
    Header level _ ils -> do  -- Attr only contains the anchor id
      txt <- inlineWriter ils
      return [mkHead txt level]
    HorizontalRule -> return [mkRaw "---"]
    Table {} -> return [mkRaw "TODO CONV: table"]
    Div _ xs -> roamBlocks inlineWriter xs
    Text.Pandoc.Definition.Null -> return []
  rest <- roamBlocks inlineWriter bs
  return $ rbs ++ rest

textBlock :: PandocMonad m
          => ([Inline] -> m Text) -> [Inline] -> m [RoamBlock]
textBlock inlineWriter ils = do
  txt <- inlineWriter ils
  return [mkPara txt]
