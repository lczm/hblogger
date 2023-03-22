module Main where
import Html
import Markup

print :: Show a => a -> IO ()
print = putStrLn . show

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
