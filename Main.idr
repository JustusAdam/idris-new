module Main


import System.Posix.Directory


takeFileName : String -> String
takeFileName str = last $ (str ::) $ split (== '/') str

data Result a b
    = Ok a
    | Failure b
    | NoInput

record Question a where
    constructor MkQuestion
    reader : IO (Result a String)
    msg : Maybe String
    suggestion : Maybe (IO a)

interface Prompt a where
    prompt : IO a

record PackageInfo where
    constructor MkPackageInfo
    packageName : String
    author : Maybe String
    sourcedir : String
    execConf : Maybe (String, String)

Show PackageInfo where
    show info = "MkPackageInfo { packageName = " ++ show (packageName info) ++ ", sourcedir = " ++ show (sourcedir info) ++ " }"

mayDo : Applicative m => Lazy (a -> m ()) -> Maybe a -> m ()
mayDo = maybe (pure ())

execQuestion : Show a => Question a -> IO a
execQuestion {a} q = do
    mayDo putStrLn $ msg q
    sug <- maybe (pure Nothing) (map Just) $ suggestion q
    mayDo printLn $ sug
    res <- reader q
    case res of
        NoInput => maybe (putStrLn "Please provide some input" *> redo) pure sug
        Failure err => putStrLn err *> redo
        Ok a => pure a
  where
    redo : IO a
    redo = execQuestion {a} q


question : Show a => IO (Result a String) -> (Question a -> Question a) -> IO a
question reader modifier = execQuestion (modifier $ MkQuestion reader Nothing Nothing)

suggest : IO a -> Question a -> Question a
suggest provider = record { suggestion = Just provider }

message : String -> Question a -> Question a
message m = record { msg = Just m }

-- choice : Show a => [a] -> IO a
-- choice choices = do
--     for (zip [1..] )

optional : IO (Result a String) -> IO (Result (Maybe a) String)
optional reader = map f reader
  where
      f NoInput = Ok Nothing
      f (Ok a) = Ok $ Just a
      f (Failure err) = Failure err

input : (String -> Result a String) -> IO (Result a String)
input read = read <$> getLine

strInput : IO (Result String String)
strInput = input $ \c => if c == "" then NoInput else Ok c

Prompt PackageInfo where
    prompt = MkPackageInfo
        <$> question strInput
            ( message "Name for the package?"
            . suggest (takeFileName <$> getCurrentDirectory))
        <*> question (optional strInput)
            ( message "Name of the author? (optional)" )
        <*> question strInput
            ( message "Name for the soure folder?"
            . suggest (pure "src"))
        <*> (do
                execName <- question (optional strInput)
                                ( message "Name for the executable (optional)")
                case execName of
                    Nothing => pure Nothing
                    Just name => do
                        main <- question strInput
                                    ( message "Name for the main module?"
                                    . suggest (pure "Main"))
                        pure $ Just (name, main)
            )

infixr 5 </>
(</>) : String -> String -> String
s1 </> s2 = s1 ++ "/" ++ s2

infixr 7 <.>
(<.>) : String -> String -> String
s1 <.> s2 = s1 ++ "." ++ s2

mainIdr : String -> String
mainIdr mainName = unlines
    [ "module " ++ mainName
    , ""
    , ""
    , "import Lib"
    , ""
    , ""
    , "main : IO ()"
    , "main = someFunction"
    , ""
    ]

libIdr : String
libIdr = unlines
    [ "module Lib"
    , ""
    , ""
    , "export"
    , "someFunction : IO ()"
    , "someFunction = putStrLn \"Hello Idris\""
    , ""
    ]

ipkg : PackageInfo -> String
ipkg info = unlines $
    [ "package " ++ packageName info
    , ""
    ] ++
    (maybe [] (\a => ["author = " ++ a, ""]) (author info))
    ++
    ["modules = " ++ mmain ++ "Lib"
    , ""
    , "sourcedir = " ++ sourcedir info
    , ""
    ] ++
    maybe [] (\(execName, main) =>
            [ "executable = " ++ execName
            , "main = " ++ main
            , ""
            ])
        (execConf info)
  where
    mmain = maybe "" (\(_, main) => main ++ ", ") (execConf info)


allFiles : PackageInfo -> List (String, String)
allFiles info =
    maybe id (\(execName, main) =>
            ((sourcedir info </> main <.> "idr", mainIdr main) ::)) (execConf info)
    [ (sourcedir info </> "Lib.idr", libIdr)
    , (packageName info <.> "ipkg", ipkg info)
    ]

unless : Applicative m => Bool -> m () -> m ()
unless False m = m
unless True _ = pure ()

takeDirectory : String -> String
takeDirectory str = case (split (== '/') str) of
    [] => ""
    (x::xs) => case (init (x::xs)) of
        [] => ""
        (x::xs) => foldl (</>) x xs



makeTree : String -> IO ()
makeTree path = for_ dirs $ \dir => do
    exists <- doesDirectoryExist dir
    unless exists $ createDirectory dir
  where
    dirs : List String
    dirs with (split (== '/') path)
        | [] = []
        | (x::xs) = scanl (</>) x xs


main : IO ()
main = do
    pkgInfo <- prompt
    cwd <- getCurrentDirectory
    for_ (allFiles pkgInfo) $ the ((String, String) -> IO ()) $ \(filename, contents) => do
        exists <- doesFileExist filename
        unless exists $ do
            makeTree $ cwd </> takeDirectory filename
            writeFile filename contents >>= either printLn pure
