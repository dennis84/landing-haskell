{-# LANGUAGE OverloadedStrings #-}

module Landing.Markdown (parseMarkdown) where

import Text.Pandoc
import Text.Pandoc.Options
import Data.Set (Set, fromList)
import Network.URI (isAbsoluteURI)
import Landing.Repo (Repo, joinPath)
import qualified Data.ByteString.Lazy.Char8 as C

parseMarkdown :: Repo -> C.ByteString -> C.ByteString
parseMarkdown repo = C.pack .
                     writeHtmlString writeOptions .
                     changeURIs repo .
                     readMarkdown readOptions .
                     C.unpack

changeURIs :: Repo -> Pandoc -> Pandoc
changeURIs repo = bottomUp (map $ convertURIs repo)

convertURIs :: Repo -> Inline -> Inline
convertURIs repo (Image a (b, c))
  | isAbsoluteURI b = Image a (b, c)
  | otherwise       = Image a (generateGitHubURI repo b, c)
convertURIs _ x = x

generateGitHubURI :: Repo -> String -> String
generateGitHubURI repo path = concat
  [ "https://raw.githubusercontent.com/", joinPath repo, "/", path ]

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
