module Hblogger
  ( convertSingle
  , convertDirectory
  , process
  )
  where

import qualified Hblogger.Markup as Markup
import qualified Hblogger.Html.Internal as Html ( Title, render )
import Hblogger.Convert (convert)

import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not implemented"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

-- buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
-- buildIndex files =
--   let
--     previews =
--       map
--         ( \(file, doc) ->
--           case doc of
--             Markup.Heading 1 heading : article ->
--               Html.h_ 3 (Html.link_ file (Html.txt_ heading))
--                 <> foldMap convertStructure (take 3 article)
--                 <> Html.p_ (Html.link_ file (Html.txt_ "..."))
--             _ ->
--               Html.h_ 3 (Html.link_ file (Html.txt_ file))
--         )
--         files
--   in
--     Html.html_
--       "Blog"
--       ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
--         <> Html.h_ 2 (Html.txt_ "Posts")
--         <> mconcat previews
--       )
