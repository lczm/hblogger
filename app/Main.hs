module Main where
el :: String -> String -> String
el tag content =
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ = el "body"

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"

p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"

-- Try out writing using lambdas
h2 = (\h2 -> el h2)

makeHtml :: String -> String -> String
makeHtml title body = html_ (head_ (title_ title) <> body_ (h1_ (p_ body)))

myhtml :: String
myhtml = makeHtml "My page title" "Hello world!"

main :: IO ()
main = putStrLn myhtml
