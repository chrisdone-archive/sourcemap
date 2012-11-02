{-# LANGUAGE RecordWildCards #-}

module Test where

import           SourceMap.Types
import           SourceMap

import           Control.Monad hiding (forM_)
import           Control.Monad.ST
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Bytes
import qualified Data.ByteString.Lazy.UTF8 as Bytes
import           Data.Foldable (forM_)
import qualified Data.HashMap.Lazy as Map
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.STRef
import           Data.Word
import           System.Process.Extra
import qualified VLQ as VLQ

generateNode :: SourceMapping -> IO Value
generateNode SourceMapping{..} = do
  result <- readAllFromProcess' "node" [] source
  case result of
    Left err -> error err
    Right (_err,out) ->
      case decode (Bytes.fromString (concat (lines out))) of
         Just value -> return value
         Nothing    -> error "unable to parse node's json output"

  where source = unlines $
          ["var s = require('source-map');"
          ,""
          ,"var generator = new s.SourceMapGenerator({"] ++
          [intercalate "," (["file: " ++ show smFile] ++
                           ["sourceRoot: " ++ show root | Just root <- [smSourceRoot]])] ++
          ["});"
          ,""] ++
          map addMapping smMappings ++
          ["console.log(generator.toString())"]
        addMapping Mapping{..} = unlines $
            ["generator.addMapping({"
            ,"  generated: " ++ showPos mapGenerated ++ ","] ++
            ["  original: " ++ showPos orig ++ "," | Just orig <- [mapOriginal]] ++
            ["  source: " ++ show src ++ "," | Just src <- [mapSourceFile]] ++
            ["  name: " ++ show name | Just name <- [mapName]] ++
            ["});"]
        showPos Pos{..} = "{ line: " ++ show posLine ++ ", column: " ++ show posColumn ++ " }"

go m = do
  n <- generateNode m
  putStr $ "node:    "; Bytes.putStrLn $ encode n
  let h = generate m
  putStr "haskell: "; Bytes.putStrLn $ encode h
  putStrLn $ if  h == n then "OK" else "MISMATCH!"

gop xs = go SourceMapping { smFile = "out.js", smSourceRoot = Nothing, smMappings = xs }

mappings =
  [Mapping {
        mapGenerated = Pos 1 1,
        mapOriginal = Just (Pos 1 1),
        mapSourceFile = Just "one.js",
        mapName = Nothing
  }
  ,Mapping {
        mapGenerated = Pos 1 5,
        mapOriginal = Just (Pos 1 5),
        mapSourceFile = Just "one.js",
        mapName = Nothing
  }
  ,Mapping {
        mapGenerated = Pos 1 9,
        mapOriginal = Just (Pos 1 11),
        mapSourceFile = Just "one.js",
        mapName = Nothing
  }
  ,Mapping {
        mapGenerated = Pos 1 18,
        mapOriginal = Just (Pos 1 21),
        mapSourceFile = Just "one.js",
        mapName = Just "bar"
  }
  ,Mapping {
        mapGenerated = Pos 1 21,
        mapOriginal = Just (Pos 2 3),
        mapSourceFile = Just "one.js",
        mapName = Nothing
  }
  ,Mapping {
        mapGenerated = Pos 1 28,
        mapOriginal = Just (Pos 2 10),
        mapSourceFile = Just "one.js",
        mapName = Just "baz"
  }
  ,Mapping {
        mapGenerated = Pos 1 32,
        mapOriginal = Just (Pos 2 14),
        mapSourceFile = Just "one.js",
        mapName = Just "bar"
  }
  ,Mapping {
        mapGenerated = Pos 2 1,
        mapOriginal = Just (Pos 1 1),
        mapSourceFile = Just "two.js",
        mapName = Nothing
  }
  ,Mapping {
        mapGenerated = Pos 2 5,
        mapOriginal = Just (Pos 1 5),
        mapSourceFile = Just "two.js",
        mapName = Nothing
  }
  ,Mapping {
        mapGenerated = Pos 2 9,
        mapOriginal = Just (Pos 1 11),
        mapSourceFile = Just "two.js",
        mapName = Nothing
  },
  Mapping {
         mapGenerated = Pos 2 18,
         mapOriginal = Just (Pos 1 21),
         mapSourceFile = Just "two.js",
         mapName = Just "n"
   },
   Mapping {
         mapGenerated = Pos 2 21,
         mapOriginal = Just (Pos 2 3),
         mapSourceFile = Just "two.js",
         mapName = Nothing
   },
   Mapping {
         mapGenerated = Pos 2 28,
         mapOriginal = Just (Pos 2 10),
         mapSourceFile = Just "two.js",
         mapName = Just "n"
   }]
