{-# LANGUAGE OverloadedStrings #-}

module Landing.Markdown (parseMarkdown) where

import Text.Pandoc
import Text.Pandoc.Options
import Data.Set (Set, fromList)
import qualified Data.ByteString.Lazy.Char8 as C

parseMarkdown :: C.ByteString -> C.ByteString
parseMarkdown = C.pack .
                writeHtmlString writeOptions .
                readMarkdown readOptions .
                C.unpack

writeOptions = def
  { writerExtensions  = extensions
  , writerReferenceLinks = True
  , writerSectionDivs = True
  , writerHighlight = True }

readOptions = def
  { readerExtensions = extensions }

extensions :: Set Extension
extensions = fromList
  [ Ext_pipe_tables
  , Ext_raw_html
  , Ext_tex_math_single_backslash
  , Ext_fenced_code_blocks
  , Ext_fenced_code_attributes
  , Ext_auto_identifiers
  , Ext_ascii_identifiers
  , Ext_backtick_code_blocks
  , Ext_autolink_bare_uris
  , Ext_intraword_underscores
  , Ext_strikeout
  , Ext_lists_without_preceding_blankline ]
