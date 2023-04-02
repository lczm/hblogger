module Hblogger.Convert where

import qualified Hblogger.Markup as Markup
import qualified Hblogger.Html as Html
-- import qualified Html.Internal as HI

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
    case structure of 
        Markup.Heading n txt ->
            Html.h_ n txt
        Markup.Paragraph p ->
            Html.p_ p
        Markup.UnorderedList list ->
            Html.ul_ $ map Html.p_ list
        Markup.OrderedList list ->
            Html.ol_ $ map Html.p_ list
        Markup.CodeBlock list ->
            Html.code_ (unlines list)

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure