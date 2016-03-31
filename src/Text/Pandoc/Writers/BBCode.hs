{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables #-}
{-
Copyright (C) 2006-2015 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.BBCode
   Copyright   : Copyright (C) 2006-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to bbcode.

BBCode: <https://forum.paradoxplaza.com/forum/index.php?help/bb-codes>
-}
module Text.Pandoc.Writers.BBCode (writeBBCode) where
import Text.Pandoc.Definition

import Text.Pandoc.Walk
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, blanklines, char, space)
import Data.Maybe (fromMaybe)
import Data.List ( group, stripPrefix, find, intersperse, intercalate, transpose, sortBy )
import Data.Char ( isSpace, isPunctuation, ord, chr )
import Control.Monad.State
import Data.Default
{-
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Readers.TeXMath (texMathToInlines)
import Text.HTML.TagSoup (parseTags, isTagText, Tag(..))
import Network.URI (isURI)
import Data.Yaml (Value(Object,String,Array,Bool,Number))
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Set as Set
-}

data WriterState = WriterState { currentSize :: Int
                              }
instance Default WriterState where
    def = WriterState { currentSize = 4 }

writeBBCode :: WriterOptions -> Pandoc -> String
writeBBCode opts document@(Pandoc meta blocks) =
    evalState (blocksToBBCode opts blocks) def

type BBWriter = State WriterState String

-- TODO use concatMapM more often?
concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM f xs = concat <$> mapM f xs

blocksToBBCode      :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> BBWriter
blocksToBBCode opts blocks = intercalate "\n" <$> mapM blockToBBCode blocks where
    blockToBBCode (Header n attr ils)  = relSize 1 $ inlinesToBBCode opts ils
    blockToBBCode (Plain ils)          = do x <- inlinesToBBCode opts ils; return $ x ++ "\n"
    blockToBBCode (Para ils)           = do x <- inlinesToBBCode opts ils; return $ x ++ "\n"
    blockToBBCode (CodeBlock attr s)   = return (bb "code" s)
    blockToBBCode (RawBlock  attr s)   = return (bb "code" s)
    blockToBBCode (BlockQuote blocks)  = bb "quote" <$> blocksToBBCode opts blocks
    blockToBBCode (OrderedList listAttr blockss) =
        (bbo "list" "=1") . concat <$> mapM (\blocks -> ("[*]"++) <$> blocksToBBCode opts blocks) blockss
    blockToBBCode (BulletList blockss) =
        (bb  "list"     ) . concat <$> mapM (\blocks -> ("[*]"++) <$> blocksToBBCode opts blocks) blockss
    blockToBBCode (DefinitionList xs)  = formatDefinitionList opts xs
    blockToBBCode (HorizontalRule)     = return "\n\n--------------------------------\n\n"
    blockToBBCode (Table _ _ _ _ _)    = return "Implement tables yourself if you want them."
    blockToBBCode (Div attr blocks)    = blocksToBBCode opts blocks
    blockToBBCode Null                 = return []

formatDefinitionList :: WriterOptions -> [([Inline],[[Block]])] -> BBWriter
formatDefinitionList opts xs = bb "list" . concat <$> forM xs (\(ils,blockss) ->
    do  def <- bb "b" . bb "u" <$> inlinesToBBCode opts ils
        explanation <- concatMapM (blocksToBBCode opts) blockss
        return $ '[':'*':']':def ++ ' ':explanation)

inlinesToBBCode :: WriterOptions -> [Inline] -> BBWriter
inlinesToBBCode opts ils = concat <$> mapM (inlineToBBCode opts) ils

inlineToBBCode :: WriterOptions -> Inline -> BBWriter
inlineToBBCode opts (Str string)      = return string
inlineToBBCode opts (Emph        ils) = bb "i" <$> inlinesToBBCode opts ils
inlineToBBCode opts (Strong      ils) = bb "b" <$> inlinesToBBCode opts ils
inlineToBBCode opts (Strikeout   ils) = bb "s" <$> inlinesToBBCode opts ils
inlineToBBCode opts (Superscript ils) = inlinesToBBCode opts ils -- not supported
inlineToBBCode opts (Subscript   ils) = relSize (-2) $ inlinesToBBCode opts ils
-- TODO: emulate with font size!
--SmallCaps [Inline]
--Small caps text (list of inlines)
inlineToBBCode opts (Quoted _ ils)    = do x <- inlinesToBBCode opts ils ; return ('"':x++"\"")
inlineToBBCode opts (Cite _ ils)      = bb "quote" <$> inlinesToBBCode opts ils
inlineToBBCode opts (Code attr s)     = return (bb "b" s) -- [code] is for blocks, not inline
inlineToBBCode opts (Space)           = return " "
inlineToBBCode opts (SoftBreak)       = return " "
inlineToBBCode opts (LineBreak)       = return "\n"
inlineToBBCode opts (Math mt s)       = return ('$' : bb "i" s ++ "$")
inlineToBBCode opts (RawInline fmt s) = return s
inlineToBBCode opts (Link  attr ils (url,title)) = bbo "url" ('=':url) <$> inlinesToBBCode opts ils
inlineToBBCode opts (Image attr ils (url,title)) = bbo "img" ('=':url) <$> inlinesToBBCode opts ils
inlineToBBCode opts (Note blocks)     = relSize (-1) $ blocksToBBCode opts blocks
inlineToBBCode opts (Span attr ils)   = inlinesToBBCode opts ils


bb :: String -> String -> String
bb tag = bbo tag ""

bbo ::String -> String -> String -> String
bbo tag opt x = '[':tag ++ opt ++ ']':x ++ '[':'/':tag ++ "]"

relSize :: Int -> BBWriter -> BBWriter
relSize d wr = do st <- get
                  let size  = currentSize st
                  let size' = clamp (1,7) (size + d)
                  put $ st { currentSize = size' }
                  result <- wr
                  put st
                  return (bbo "size" ('=':show size') result)

clamp :: Ord a => (a,a) -> a -> a
clamp (lo,hi) x = max lo (min hi x)
