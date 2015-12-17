module Main (main) where


import qualified Data.Char             as DCh
import           Data.List
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.Ord              (comparing)
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           System.Random

-- | A literal for version. It looks like a variable. Well, indeed, it is not.
--   It is a function that represents a literal. It even has signature... But as
--   every function is a value, so it is...
version = "0.0.1"
{-|
  The code below is a simple and password generator that demonstrates a number of
  programming problems that are necessary to write a minimal useful application
  in Haskell: parsing CL, reading from stdin or from file and doing some list
  operations on the values. There is nothing fancy, only basic language constructs and
  libraries with comments. It is not a tutorial on Haskell, more something where You may
  go on after covering the first 9 chapters of the LYH book - the comments are more to
  remark than to explain in details.
  There are two algorithms implemented:
  1. prompt a text from stdin, then mix its vowels and consonants, change the letters
    following some rule of similarity ( e -> 3, l -> 1, o -> 0 etc.) and return
    the result. The password can be memorized with the original word relatively easily.
    2. read a text file, pick the initials of each word, do the substitution as above
    and return the result.
    The second algorithm is safer, but the first one can still make dictionary attacks and
    brute force attacks more difficult, though once the rules are known
    - each even letter is a vowel, etc. -, it can easily be broken. Anyway, still far better than
    the usual 'mypassword' types, especially when tainted with some delimiters ( like _ # - )
    Should be able to read from sdtin ( pipe, for instance ), too.

    So, the tasks are:
    - prompt for a text and read it from  CL
    - if it flags a file - read text from a file - or stdin when no file is given -, pick the initials,
    substitute randomly and print the result to the screen
    - if it flags stdin, prompt user to enter text, scramble the letters, substitute randomly and print.

    The good part: if You remove all the comments, the rest of the code is very short and
    still not cryptic. It is possible to write such a short code in some other languages but
    those tend to be cryptic. Haskell always surprises me that I have to write down
    what I think of and the language usually stays in the background.
    My hard lesson: forget all that You learned about building a program. Tabula rasa.

    NOTES:
    Editor: I used atom editor with haskell ghci-mod and haskell ide. Turned out to be
    really handy, though the package ghci-mod crashed after each update.
    If You want to play around, load the code into ghci and the functions are accessible.
    I use two types of comments. The docu can be generated with 'haddock -h -o doc/', but as
    we put only (main) into the 'module' declaration, only the docu for main will be
    generated.

  main: the entry point of the program. It is always IO(). There is no program
  without interaction with the world, and that world is not pure.
  Here the interaction on the input happens with getArgs function - getting the CL list -,
  that is parsed with the getOpt function, something like [Input "stdin"] or whatever.
  'Permute' means freely intersperse options and non-options.
  Signatures start with IO() and likely end with IO(). For me the best approach to build the
  architecture was starting to write the impure ones, because those will wrap the pure ones.
  You may have clear data structures alread in mind but for the whole I found writing them first
  confusing later. This may change in the future, but hey, one has to reach that point!
  So, the last step in a do block must be an expression that returns something IO().
  If we have nothing better, Haskell's return function will do it for us. It will construct
  us one - return is a function. Now, here whatever we get from processOpt, that is
  wrapped into IO..
-}

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (flags, [],      [])     -> processOpt flags
    (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo messageHeader options

{-|
  to get the CL we need a data type that represents the flags.
  This type derives from Eq, so we can use them in functions where
  we need something Equal, e.g Data.List elem funtion. However, having a list of Ops,
  like [Input (Just "stdin")] won't work with Eq simply if we want Eq to compare against
  the kind of the value and not its contents. So, we can not say
  find Input [Input (Just "stdin")]. That won't work. But
  find Input "" [Input (Just "stdin")] will, and will be False and for this case.
  Input is a Maybe because it is an OptArg.
-}
data Ops = Input (Maybe String) | Prompt | Help | Version deriving(Eq)

-- | messageHeader...
messageHeader :: String
messageHeader = "Usage: password_generator [ OPTION...] , where "

-- | options: where the options are defined. The syntax is: define short flags as list of Chars,
--   long ones as list of Strings, then the kinds, and message on failure or help.
options :: [OptDescr Ops]
options = [
   Option ['v'] ["version"] (NoArg Version)     "show version number"
  ,Option ['i'] ["input"]   (OptArg Input "FILE") "input file. When no intput is given, stdin is used."
  ,Option ['h'] ["help"]    (NoArg Help)        "this help"
  ,Option ['p'] ["prompt"]  (NoArg Prompt)      "prompt text from stdin"
  ]

{-| Let's start the work. This function is still IO(), we are still following the line of
  impure function signatures. Haskell types force the propagation of IO() types through the code, as
  is it not possible to wrap IO() into a pure function. That would be polluted immediately.
  Of course, it works the other way around.
  When building a bigger code and suddenly an IO() must pop-up somewhere deep in the land of
  pure functions, it will delegate filt above. No such freedom as doing ad-hoc IO anywhere in your code
  without consequences - the IO kind signature will propagate up the call stack immediately.
  The list comprehension below - [x | Input x <- ops] - takes the ops list and does a pattern match
  against Input x. If that matches it returns x, otherwise just goes on. If nothing matches
  we receive [] and an exception - head on empty list.
  Depending on the value of Input we can take two directions: stdin or file processing.
-}
processOpt :: [Ops] -> IO ()
processOpt ops
  | True = processFile "stdin"
  | Version `elem` ops = putStrLn ("Version : " ++ version)
  | Help `elem` ops = putStr $ usageInfo messageHeader options
  | Prompt `elem` ops = promptForPassphrase
  | otherwise = processFile $ head [ fromMaybe "stdin" x | Input x <- ops]

{-|
  promptForPassphrase: simply get the intput from STDIN
  and pass the result to the algorithm.
-}
promptForPassphrase :: IO ()
promptForPassphrase = do
  line <- getContents
  generatePassword line

{-|
  Processing a file is simple. We need to read it in, pick the words, then
  the first characters of each word, lowercase and generate. buildPassword is a
  function that is built with piping several functions into a chain ( function
  composition ).
-}

processFile :: String -> IO()
processFile fn =
  let buildPassword = generatePassword . map DCh.toLower . head . words in
    case fn of
      "stdin" -> do
                  content <- getLine
                  buildPassword content
      _       -> do
                  content <- readFile fn
                  buildPassword content

{-|
  generatePassword: take a string, separate the consonants and the vowels,
  then comes again a do block: we need a random generator and that is IO().
  The random generator needs a seed and it means entropy and it means the outside world.
  We can also provide the seed, and we are pure then, but our random generator
  will return the same value each time we call it.
  After mixing up the consonants and vowels, we reassamble them and do the character substitution.
-}

generatePassword :: String -> IO()
generatePassword line =
  let (cons,vow) = foldl ( \(c,w) n -> if n `elem` "aeiouyAEIOUY" then (n:c,w) else (c,n:w) ) ([],[]) line in
    do
      print line
      gen <- getStdGen
      let c = scrambleList cons (randoms gen)
          v = scrambleList vow (randoms gen) in
            putStrLn (substituteChars . take (length line) $ concat [[a,b] | (a, b) <- zip (c++c) (v++v) ])

-- | scrambleList: scramble the list using a random sequence.
-- First generate tuples as ( char, rnd ), sort by comparing the snd element,
-- then pick the first of each sorted tuple.
-- Example:
-- >>> scrambleList "Ghanima" [7,6,5,4,3,2,1,0]
-- "aminahG"
scrambleList :: Ord a => [a] -> [a] -> [a]
scrambleList xs ys
  | null xs = xs
  | otherwise = map fst . sortBy (comparing snd) $ zip xs ys

{-|
  Let's make here a random generator again, but this one will
  be seeded by us, thus the returned 'random' number will be the same always
  as this is a pure function. However, we already have a 'properly' randomized
  piece of data, namely the input string, so we can use it to seed. The advantage
  is that we can write positive test to the function as it is pure.
  So, pick the sum of the numerical value of the first three characters and
  multiply it with the sum of the numerical value. This random generator
  will be used to decide on substitution.
  An example with several declarations in a let block. An other syntactic sugar
  in text@(x:y:z:xs) means that in the code we can refer to the whole list as
  text, or its parts as in the ( ).
  Example:

  >>> substituteChars "Ghanima"
  "Ghan1ma"
-}
substituteChars :: String -> String
substituteChars text@(x:y:z:xs) =
  let s1 = 1 + sum ( map DCh.ord [x,y,z])
      s2 = 1 + sum [ DCh.ord e | e <-xs ]
      seed = s1*s2
      gen = mkStdGen seed
      ranlist = zip (randomRs (False,True) gen) text
        in map (\(true,ch ) -> if true then trySubstitute ch else ch ) ranlist

-- | trySubstitute: With find from Data.List we try to pick a tuple where the first element
--   equals the character c using lambda. As 'find' will return a Maybe a,
--   we can use 'fromMaybe' from Data.Maybe to return us either the found Just(x,x)
--   or the passed parameter in a tuple ( c,c ). Whatever is returned,
--   just pick the second element and return it.
--   Here $ is saving of writing () around the 'find', the . after 'snd' is piping the
--   output of the expression after it ( that is a tuple ) into 'snd', saving some () again.
--   Example:
-- >>> trySubstitute 's'
-- '5'
-- >>> trySubstitute 'k'
-- 'k'
trySubstitute c = snd . fromMaybe (c,c) $ find(\x-> fst x == c) matchlist
    where matchlist = [('o','0'),('i','1'),('l','1'),('e','3'),('s','5')]
