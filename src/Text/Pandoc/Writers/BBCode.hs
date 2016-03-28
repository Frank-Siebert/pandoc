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
   Module      : Text.Pandoc.Writers.Markdown
   Copyright   : Copyright (C) 2006-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to markdown-formatted plain text.

Markdown:  <http://daringfireball.net/projects/markdown/>
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
    "Let's face it, writeBBCode is not implemented at all" ++ show document ++ "\n" ++
    evalState (blocksToBBCode opts blocks) def

type BBWriter = State WriterState String

blocksToBBCode      :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> BBWriter
blocksToBBCode opts blocks = intercalate "\n" <$> mapM blockToBBCode blocks where
    blockToBBCode (Header n _ xs)      = return $ bb "b" (show xs)
    blockToBBCode (Plain ils)          = do x <- inlinesToBBCode ils; return $ x ++ "\n"
    blockToBBCode (Para ils)           = do x <- inlinesToBBCode ils; return $ x ++ "\n"
    blockToBBCode (CodeBlock attr s)   = return (bb "code" s)
    blockToBBCode (RawBlock  attr s)   = return (bb "code" s)
    blockToBBCode (BlockQuote blocks)  = bb "quote" <$> blocksToBBCode opts blocks
    blockToBBCode (OrderedList listAttr blockss) =
        (bbo "list" "=1") . concat <$> mapM (\blocks -> ("[*]"++) <$> blocksToBBCode opts blocks) blockss
    blockToBBCode (BulletList blockss) =
        (bb  "list"     ) . concat <$> mapM (\blocks -> ("[*]"++) <$> blocksToBBCode opts blocks) blockss
    blockToBBCode (DefinitionList xs)  = return ":-("
    blockToBBCode (HorizontalRule)     = return "\n\n--------------------------------\n\n"
    blockToBBCode (Table _ _ _ _ _)    = return "Implement tables yourself if you want them."
    blockToBBCode (Div attr blocks)    = blocksToBBCode opts blocks
    blockToBBCode Null                 = return []

inlinesToBBCode :: [Inline] -> BBWriter
inlinesToBBCode ils = concat <$> mapM inlineToBBCode ils

inlineToBBCode :: Inline -> BBWriter
inlineToBBCode (Str string) = return string
inlineToBBCode (Emph        ils) = bb "i" <$> inlinesToBBCode ils
inlineToBBCode (Strong      ils) = bb "b" <$> inlinesToBBCode ils
inlineToBBCode (Strikeout   ils) = bb "s" <$> inlinesToBBCode ils
inlineToBBCode (Superscript ils) = inlinesToBBCode ils -- not supported
inlineToBBCode (Subscript   ils) = relSize (-2) $ inlinesToBBCode ils
-- TODO: emulate with font size!
--SmallCaps [Inline]
--Small caps text (list of inlines)
inlineToBBCode (Quoted _ ils)    = do x <- inlinesToBBCode ils ; return ('"':x++"\"")
inlineToBBCode (Cite _ ils)      = bb "quote" <$> inlinesToBBCode ils
inlineToBBCode (Code attr s)     = return (bb "b" s) -- [code] is for blocks, not inline
inlineToBBCode (Space)           = return " "
inlineToBBCode (SoftBreak)       = return " "
inlineToBBCode (LineBreak)       = return "\n"
inlineToBBCode (Math mt s)       = return ('$' : bb "i" s ++ "$")
inlineToBBCode (RawInline fmt s) = return s
inlineToBBCode (Link  attr ils (url,title)) = bbo "url" ('=':url) <$> inlinesToBBCode ils
inlineToBBCode (Image attr ils (url,title)) = bbo "img" ('=':url) <$> inlinesToBBCode ils
inlineToBBCode (Note blocks)     = relSize (-1) $ blocksToBBCode undefined blocks
inlineToBBCode (Span attr ils)   = inlinesToBBCode ils


bb :: String -> String -> String
bb tag = bbo tag ""

bbo ::String -> String -> String -> String
bbo tag opt x = '[':tag ++ opt ++ ']':x ++ '[':'/':tag ++ "]"

relSize :: Int -> BBWriter -> BBWriter
relSize d wr = do st <- get
                  let size  = currentSize st
                  let size' = clamp (1,7) (size - 1)
                  put $ st { currentSize = size' }
                  result <- wr
                  put st
                  return (bbo "size" ('=':show size') result)

clamp :: Ord a => (a,a) -> a -> a
clamp (lo,hi) x = max lo (min hi x)
