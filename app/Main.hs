module Main where

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
            ( el "head" ( el "title" title )
                <> el "body" (getStructureString content)
            )
        )

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

myhtml :: Html
myhtml = 
    html_
        "My title"
        ( append_
            ( h1_ "Heading" )
            ( append_
                ( p_ "Paragraph #1" )
                ( p_ "Paragraph #2" )
            )
        )

main :: IO ()
main = putStrLn (render myhtml)
