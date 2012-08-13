{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Ansi
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to ansi.

Note that some information may be lost in conversion, due to
expressive limitations of ansi.  Footnotes and table cells with
paragraphs (or other block items) are not possible in ansi.
If pandoc encounters one of these, it will insert a message indicating
that it has omitted the construct.

Ansi:  <http://www.methods.co.nz/ansi/>
-}
module Text.Pandoc.Writers.Ansi (writeAnsi) where
import Text.Pandoc.Definition
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Shared
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, space)
import Data.List ( isPrefixOf, intersperse, intercalate )
import Text.Pandoc.Pretty
import Control.Monad.State

data WriterState = WriterState { defListMarker :: String
                               , orderedListLevel :: Int
                               , bulletListLevel  :: Int
                               }

-- | Convert Pandoc to Ansi.
writeAnsi :: WriterOptions -> Pandoc -> String
writeAnsi opts document =
  evalState (pandocToAnsi opts document) WriterState{
      defListMarker = "::"
    , orderedListLevel = 1
    , bulletListLevel = 1
    }

-- | Return ansi representation of document.
pandocToAnsi :: WriterOptions -> Pandoc -> State WriterState String
pandocToAnsi opts (Pandoc (Meta title authors date) blocks) = do
  title' <- inlineListToAnsi opts title
  let title'' = title' $$ text (replicate (offset title') '=')
  authors' <- mapM (inlineListToAnsi opts) authors
  -- ansi only allows a singel author
  date' <- inlineListToAnsi opts date
  let titleblock = not $ null title && null authors && null date
  body <- blockListToAnsi opts blocks
  let colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
  let main = render colwidth body
  let context  = writerVariables opts ++
                 [ ("body", main)
                 , ("title", render colwidth title'')
                 , ("date", render colwidth date')
                 ] ++
                 [ ("toc", "yes") | writerTableOfContents opts &&
                                    writerStandalone opts ] ++
                 [ ("titleblock", "yes") | titleblock ] ++
                 [ ("author", render colwidth a) | a <- authors' ]
  if writerStandalone opts
     then return $ renderTemplate context $ writerTemplate opts
     else return main

-- | Escape special characters for Ansi.
escapeString :: String -> String
escapeString = escapeStringUsing escs
  where escs = backslashEscapes "{"

-- | Ordered list start parser for use in Para below.
olMarker :: Parser [Char] ParserState Char
olMarker = do (start, style', delim) <- anyOrderedListMarker
              if delim == Period &&
                          (style' == UpperAlpha || (style' == UpperRoman &&
                          start `elem` [1, 5, 10, 50, 100, 500, 1000]))
                          then spaceChar >> spaceChar
                          else spaceChar

-- | True if string begins with an ordered list marker
beginsWithOrderedListMarker :: String -> Bool
beginsWithOrderedListMarker str =
  case runParser olMarker defaultParserState "para start" (take 10 str) of
         Left  _  -> False
         Right _  -> True

-- | Convert Pandoc block element to ansi.
blockToAnsi :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState Doc
blockToAnsi _ Null = return empty
blockToAnsi opts (Plain inlines) = do
  contents <- inlineListToAnsi opts inlines
  return $ contents <> cr
blockToAnsi opts (Para inlines) = do
  contents <- inlineListToAnsi opts inlines
  -- escape if para starts with ordered list marker
  let esc = if beginsWithOrderedListMarker (render Nothing contents)
               then text "\\"
               else empty
  return $ esc <> contents <> blankline
blockToAnsi _ (RawBlock _ _) = return empty
blockToAnsi _ HorizontalRule =
  return $ blankline <> text "'''''" <> blankline
blockToAnsi opts (Header level inlines) = do
  contents <- inlineListToAnsi opts inlines
  let len = offset contents
  return $ "\ESC[1m" <> contents <> "\ESC[0m" <> cr <>
         (case level of
               1  -> text $ replicate len '-'
--             2  -> text $ replicate len '~'
--             3  -> text $ replicate len '^'
--             4  -> text $ replicate len '+'
               _  -> empty) <> blankline
blockToAnsi _ (CodeBlock (_,classes,_) str) = return $
  flush (attrs <> dashes <> space <> attrs <> cr <> text str <>
           cr <> dashes) <> blankline
     where dashes  = text $ replicate (maximum $ map length $ lines str) '-'
           attrs = if null classes
                      then empty
                      else text $ intercalate "," $ "code" : classes
blockToAnsi opts (BlockQuote blocks) = do
  contents <- blockListToAnsi opts blocks
  let isBlock (BlockQuote _) = True
      isBlock _              = False
  -- if there are nested block quotes, put in an open block
  let contents' = if any isBlock blocks
                     then "--" $$ contents $$ "--"
                     else contents
  let cols = offset contents'
  let bar = text $ replicate cols '_'
  return $ bar $$ chomp contents' $$ bar <> blankline
blockToAnsi opts (Table caption aligns widths headers rows) =  do
  caption' <- inlineListToAnsi opts caption
  let caption'' = if null caption
                     then empty
                     else "." <> caption' <> cr
  let isSimple = all (== 0) widths
  let relativePercentWidths = if isSimple
                                 then widths
                                 else map (/ (sum widths)) widths
  let widths'' :: [Integer]
      widths'' = map (floor . (* 100)) relativePercentWidths
  -- ensure that the widths sum to 100
  let widths' = case widths'' of
                     _ | isSimple -> widths''
                     (w:ws) | sum (w:ws) < 100
                               -> (100 - sum ws) : ws
                     ws        -> ws
  let totalwidth :: Integer
      totalwidth = floor $ sum widths * 100
  let colspec al wi = (case al of
                         AlignLeft    -> "<"
                         AlignCenter  -> "^"
                         AlignRight   -> ">"
                         AlignDefault -> "") ++
                      if wi == 0 then "" else (show wi ++ "%")
  let headerspec = if all null headers
                      then empty
                      else text "options=\"header\","
  let widthspec = if totalwidth == 0
                     then empty
                     else text "width="
                          <> doubleQuotes (text $ show totalwidth ++ "%")
                          <> text ","
  let tablespec = text "["
         <> widthspec
         <> text "cols="
         <> doubleQuotes (text $ intercalate ","
             $ zipWith colspec aligns widths')
         <> text ","
         <> headerspec <> text "]"
  let makeCell [Plain x] = do d <- blockListToAnsi opts [Plain x]
                              return $ text "|" <> chomp d
      makeCell [Para x]  = makeCell [Plain x]
      makeCell _         = return $ text "|" <> "[multiblock cell omitted]"
  let makeRow cells = hsep `fmap` mapM makeCell cells
  rows' <- mapM makeRow rows
  head' <- makeRow headers
  let head'' = if all null headers then empty else head'
  let colwidth = if writerWrapText opts
                    then writerColumns opts
                    else 100000
  let maxwidth = maximum $ map offset (head':rows')
  let body = if maxwidth > colwidth then vsep rows' else vcat rows'
  let border = text $ "|" ++ replicate ((min maxwidth colwidth) - 1) '='
  return $
    caption'' $$ tablespec $$ border $$ head'' $$ body $$ border $$ blankline
blockToAnsi opts (BulletList items) = do
  contents <- mapM (bulletListItemToAnsi opts) items
  return $ cat contents <> blankline
blockToAnsi opts (OrderedList (_start, sty, _delim) items) = do
  let sty' = case sty of
                  UpperRoman -> UpperAlpha
                  LowerRoman -> LowerAlpha
                  x          -> x
  let markers  = orderedListMarkers (1, sty', Period)  -- start num not used
  let markers' = map (\m -> if length m < 3
                               then m ++ replicate (3 - length m) ' '
                               else m) markers
  contents <- mapM (\(item, num) -> orderedListItemToAnsi opts item num) $
              zip markers' items
  return $ cat contents <> blankline
blockToAnsi opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToAnsi opts) items
  return $ cat contents <> blankline

-- | Convert bullet list item (list of blocks) to ansi.
bulletListItemToAnsi :: WriterOptions -> [Block] -> State WriterState Doc
bulletListItemToAnsi opts blocks = do
  let addBlock :: Doc -> Block -> State WriterState Doc
      addBlock d b | isEmpty d    = chomp `fmap` blockToAnsi opts b
      addBlock d b@(BulletList _) = do x <- blockToAnsi opts b
                                       return $ d <> cr <> chomp x
      addBlock d b@(OrderedList _ _) = do x <- blockToAnsi opts b
                                          return $ d <> cr <> chomp x
      addBlock d b = do x <- blockToAnsi opts b
                        return $ d <> cr <> text "+" <> cr <> chomp x
  lev <- bulletListLevel `fmap` get
  modify $ \s -> s{ bulletListLevel = lev + 1 }
  contents <- foldM addBlock empty blocks
  modify $ \s -> s{ bulletListLevel = lev }
  let marker = text (replicate lev '*')
  return $ marker <> space <> contents <> cr

-- | Convert ordered list item (a list of blocks) to ansi.
orderedListItemToAnsi :: WriterOptions -- ^ options
                          -> String        -- ^ list item marker
                          -> [Block]       -- ^ list item (list of blocks)
                          -> State WriterState Doc
orderedListItemToAnsi opts marker blocks = do
  let addBlock :: Doc -> Block -> State WriterState Doc
      addBlock d b | isEmpty d    = chomp `fmap` blockToAnsi opts b
      addBlock d b@(BulletList _) = do x <- blockToAnsi opts b
                                       return $ d <> cr <> chomp x
      addBlock d b@(OrderedList _ _) = do x <- blockToAnsi opts b
                                          return $ d <> cr <> chomp x
      addBlock d b = do x <- blockToAnsi opts b
                        return $ d <> cr <> text "+" <> cr <> chomp x
  lev <- orderedListLevel `fmap` get
  modify $ \s -> s{ orderedListLevel = lev + 1 }
  contents <- foldM addBlock empty blocks
  modify $ \s -> s{ orderedListLevel = lev }
  return $ text marker <> space <> contents <> cr

-- | Convert definition list item (label, list of blocks) to ansi.
definitionListItemToAnsi :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState Doc
definitionListItemToAnsi opts (label, defs) = do
  labelText <- inlineListToAnsi opts label
  marker <- defListMarker `fmap` get
  if marker == "::"
     then modify (\st -> st{ defListMarker = ";;"})
     else modify (\st -> st{ defListMarker = "::"})
  let divider = cr <> text "+" <> cr
  let defsToAnsi :: [Block] -> State WriterState Doc
      defsToAnsi ds = (vcat . intersperse divider . map chomp)
           `fmap` mapM (blockToAnsi opts) ds
  defs' <- mapM defsToAnsi defs
  modify (\st -> st{ defListMarker = marker })
  let contents = nest 2 $ vcat $ intersperse divider $ map chomp defs'
  return $ labelText <> text marker <> cr <> contents <> cr

-- | Convert list of Pandoc block elements to ansi.
blockListToAnsi :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState Doc
blockListToAnsi opts blocks = cat `fmap` mapM (blockToAnsi opts) blocks

-- | Convert list of Pandoc inline elements to ansi.
inlineListToAnsi :: WriterOptions -> [Inline] -> State WriterState Doc
inlineListToAnsi opts lst =
  mapM (inlineToAnsi opts) lst >>= return . cat

-- | Convert Pandoc inline element to ansi.
inlineToAnsi :: WriterOptions -> Inline -> State WriterState Doc
inlineToAnsi opts (Emph lst) = do
  contents <- inlineListToAnsi opts lst
  return $ "\ESC[3m" <> contents <> "\ESC[0m"
inlineToAnsi opts (Strong lst) = do
  contents <- inlineListToAnsi opts lst
  return $ "\ESC[1m" <> contents <> "\ESC[0m"
inlineToAnsi opts (Strikeout lst) = do
  contents <- inlineListToAnsi opts lst
  return $ "\ESC[9m" <> contents <> "\ESC[0m"
inlineToAnsi opts (Superscript lst) = do
  contents <- inlineListToAnsi opts lst
  return $ "^" <> contents <> "^"
inlineToAnsi opts (Subscript lst) = do
  contents <- inlineListToAnsi opts lst
  return $ "~" <> contents <> "~"
inlineToAnsi opts (SmallCaps lst) = inlineListToAnsi opts lst
inlineToAnsi opts (Quoted SingleQuote lst) = do
  contents <- inlineListToAnsi opts lst
  return $ "`" <> contents <> "'"
inlineToAnsi opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToAnsi opts lst
  return $ "``" <> contents <> "''"
inlineToAnsi _ (Code _ str) = return $
  text "`" <> text (escapeStringUsing (backslashEscapes "`") str) <> "`"
inlineToAnsi _ (Str str) = return $ text $ escapeString str
inlineToAnsi _ (Math InlineMath str) =
  return $ "latexmath:[$" <> text str <> "$]"
inlineToAnsi _ (Math DisplayMath str) =
  return $ "latexmath:[\\[" <> text str <> "\\]]"
inlineToAnsi _ (RawInline _ _) = return empty
inlineToAnsi _ (LineBreak) = return $ " +" <> cr
inlineToAnsi _ Space = return space
inlineToAnsi opts (Cite _ lst) = inlineListToAnsi opts lst
inlineToAnsi opts (Link txt (src, _tit)) = do
-- relative:  link:downloads/foo.zip[download foo.zip]
-- abs:  http://google.cod[Google]
-- or my@email.com[email john]
  linktext <- inlineListToAnsi opts txt
  let isRelative = ':' `notElem` src
  let prefix = if isRelative
                  then text "link:"
                  else empty
  let srcSuffix = if isPrefixOf "mailto:" src then drop 7 src else src
  let useAuto = case txt of
                      [Code _ s] | s == srcSuffix -> True
                      _                           -> False
  return $ if useAuto
              then text srcSuffix
              else prefix <> text src <> "[" <> linktext <> "]"
inlineToAnsi opts (Image alternate (src, tit)) = do
-- image:images/logo.png[Company logo, title="blah"]
  let txt = if (null alternate) || (alternate == [Str ""])
               then [Str "image"]
               else alternate
  linktext <- inlineListToAnsi opts txt
  let linktitle = if null tit
                     then empty
                     else text $ ",title=\"" ++ tit ++ "\""
  return $ "image:" <> text src <> "[" <> linktext <> linktitle <> "]"
inlineToAnsi opts (Note [Para inlines]) =
  inlineToAnsi opts (Note [Plain inlines])
inlineToAnsi opts (Note [Plain inlines]) = do
  contents  <- inlineListToAnsi opts inlines
  return $ text "footnote:[" <> contents <> "]"
-- ansi can't handle blank lines in notes
inlineToAnsi _ (Note _) = return "[multiblock footnote omitted]"
