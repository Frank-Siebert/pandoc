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
    evalState (blockListToMarkdown opts blocks) def

type BBWriter = State WriterState String
    
blockListToMarkdown :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> BBWriter
blockListToMarkdown opts blocks = intercalate "\n" <$> mapM blockToBBCode blocks where
    blockToBBCode (Header n _ xs) = return $ bb "b" (show xs)
    blockToBBCode (Para ils)      = return $ inlinesToBBCode ils ++ "\n"
    blockToBBCode (BulletList blockss) = return ":-("

inlinesToBBCode :: [Inline] -> String
inlinesToBBCode = concatMap inlineToBBCode
    
inlineToBBCode :: Inline -> String
inlineToBBCode (Str string) = string
inlineToBBCode (Emph        ils) = bb "i" $ inlinesToBBCode ils
inlineToBBCode (Strong      ils) = bb "b" $ inlinesToBBCode ils
inlineToBBCode (Strikeout   ils) = bb "s" $ inlinesToBBCode ils
inlineToBBCode (Superscript ils) = inlinesToBBCode ils -- not supported
inlineToBBCode (Subscript   ils) = bbo "size" "=2" $ inlinesToBBCode ils
-- TODO: emulate with font size!
--SmallCaps [Inline]	
--Small caps text (list of inlines)
{-Quoted QuoteType [Inline]	
Quoted text (list of inlines)
Cite [Citation] [Inline]	
Citation (list of inlines)
Code Attr String	
Inline code (literal)
-}
inlineToBBCode (Space) = " "
inlineToBBCode (SoftBreak) = " "
inlineToBBCode (LineBreak) = "\n"
{-
Math MathType String	
TeX math (literal)
RawInline Format String	
Raw inline
Link Attr [Inline] Target	
Hyperlink: alt text (list of inlines), target
Image Attr [Inline] Target	
Image: alt text (list of inlines), target
Note [Block]	
Footnote or endnote
Span Attr [Inline]	
Generic inline container with attributes
-}


bb :: String -> String -> String
bb = bbo ""

bbo ::String -> String -> String -> String
bbo opt tag x = '[':tag ++ opt ++ ']':x ++ '[':'/':tag ++ "]"

