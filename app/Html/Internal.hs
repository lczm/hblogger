module Html.Internal where

newtype Html = Html String
newtype Structure = Structure String

type Title = String

getStructureString :: Structure -> String
getStructureString (Structure str) = str
-- getStructureString content =
--     case content of
--         Structure str -> str

render :: Html -> String
render html =
    case html of
        Html str -> str

escape :: String -> String
escape =
    let 
        escapeChar c =
            case c of
                '<' -> "&lt;"
                '>' -> "&gt;"
                '&' -> "&amp;"
                '"' -> "&quot;"
                '\'' -> "&#39;"
                _ -> [c]
    in concat . map escapeChar

append_ :: Structure -> Structure -> Structure
append_ a b = 
    Structure (getStructureString a <> getStructureString b)

instance Semigroup Structure where
    (<>) c1 c2 =
        Structure (getStructureString c1 <> getStructureString c2)

el :: String -> String -> String
el tag content =
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: Title -> Structure -> Html
html_ title content =
    Html
        ( el "html"
            ( el "head" ( el "title" (escape title ))
                <> el "body" (getStructureString content)
            )
        )

-- paragraphs
-- This runs the input through escape first
p_ :: String -> Structure
p_ = Structure . el "p" . escape

-- headers
-- This runs the input through escape first
h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

-- unordered lists
ul_ :: [Structure] -> Structure
ul_ contents =
    Structure . el "ul" . concat . map ( el "li" . getStructureString ) $ contents

-- ordered lists
ol_ :: [Structure] -> Structure
ol_ contents =
    Structure . el "ol" . concat . map ( el "li" . getStructureString ) $ contents

-- code blocks
code_ :: String -> Structure
code_ = Structure . el "pre" . escape