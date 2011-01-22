-- Simple test for html repr in haskell

data Html = Tag String [String] [Html]
          | Text String

res :: [Html]
res = [(Tag "div" []
        [
         (Text "Hallo"),
         (Tag "b" [] [Text "Bold"])
        ]
       )]

instance Show (Html) where show = stringifyItem

stringify :: [Html] -> String
stringify (h:hs) = (stringifyItem h) ++ (stringify hs)
stringify [] = ""

stringifyItem :: Html -> String
stringifyItem (Tag name attr body) = "<" ++ name ++ ">" ++ (stringify body) ++ "</" ++ name ++ ">"
stringifyItem (Text t) = concatMap f t
                         where f x = case x of
                                       '&' -> "&amp;"
                                       '<' -> "&lt;"
                                       '>' -> "&gt;"
                                       c -> [c]


main ::IO ()
main = do putStrLn $ stringify res
          putStrLn $ show res
          s <- readFile "input.data"
          putStrLn $ stringify [Tag "html" [] [Text s]]

-- Local Variables:
-- compile-command: "mkdir -p bin && ghc -o bin/html --make html.hs && bin/html"
-- End:
