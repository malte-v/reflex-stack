{-# LANGUAGE TemplateHaskell #-}

module Common.TH (chooseAndIncludeFileInSource) where

import Control.Monad.Extra (findM)
import Language.Haskell.TH
import RIO
import RIO.ByteString (pack, readFile, unpack)
import RIO.Directory (doesFileExist)

includeFileInSource ::
  -- | Path to file
  FilePath ->
  -- | Haskell value name
  String ->
  Q [Dec]
includeFileInSource fp n = do
  byteString <- runIO $ readFile fp
  tWord8 <- [t|Word8|]
  tByteString <- [t|ByteString|]
  fnPack <- [|pack|]
  return
    [ SigD (mkName n) tByteString,
      FunD
        (mkName n)
        [ Clause
            []
            ( NormalB
                $ AppE (SigE fnPack $ ArrowT `AppT` (ListT `AppT` tWord8) `AppT` tByteString)
                $ ListE
                $ fmap (LitE . IntegerL . fromIntegral) (unpack byteString)
            )
            []
        ]
    ]

chooseAndIncludeFileInSource ::
  -- | Paths to files
  [FilePath] ->
  -- | Haskell value name
  String ->
  Q [Dec]
chooseAndIncludeFileInSource haystack name = do
  mbNeedle <- runIO $ findM doesFileExist haystack
  case mbNeedle of
    Just needle -> includeFileInSource needle name
    Nothing -> error "None of the file choices exist."
