module Html
    ( Html
    , Title
    , Structure
    , html_
    , p_
    , h1_
    , append_
    , render
    )
    where

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

-- This runs the input through escape first
p_ :: String -> Structure
p_ = Structure . el "p" . escape

-- This runs the input through escape first
h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape