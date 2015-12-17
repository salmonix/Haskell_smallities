-- file doctests.hs

-- Main for doctests. Run it as a normal haskell code.

import           Test.DocTest
main = doctest ["-isrc", "PassGen.hs"]
