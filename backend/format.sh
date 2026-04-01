#!/usr/bin/env bash

# Format all Haskell source files with fourmolu.
# Manual extensions are those that fourmolu cannot infer from the cabal file.

find src test cli -name "*.hs" -exec fourmolu -i \
  -o -XImportQualifiedPost \
  -o -XMultilineStrings \
  -o -XOverloadedRecordDot \
  -o -XOverloadedLabels \
  -o -XTemplateHaskell \
  {} +
