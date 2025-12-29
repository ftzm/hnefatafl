{-# LANGUAGE TemplateHaskell #-}

module Version (
  version,
) where

import Data.Char (isSpace)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddDependentFile)

version :: Text
version =
  $( do
       -- Add package.yaml as a dependency so TH recompiles when it changes
       qAddDependentFile "package.yaml"

       -- Read and parse package.yaml to extract version
       content <- runIO $ T.readFile "package.yaml"
       let contentLines = T.lines content
           versionLine = find (T.isPrefixOf "version:") contentLines
       case versionLine of
         Just line -> do
           let versionStr = T.unpack $ T.dropWhile isSpace $ T.drop 8 line -- drop "version:"
           litE (stringL versionStr)
         Nothing -> fail "Could not find version in package.yaml"
   )
