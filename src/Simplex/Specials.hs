{-# LANGUAGE Haskell2010 #-}

module Simplex.Specials (
        processSpecials,
        newSpec, Spec(..)
    ) where

import Simplex.Config
import Simplex.ConfigData
import Simplex.CmdLineOpts
import Simplex.Parser
import Simplex.Util

import System.Directory
import System.Random

import Data.Maybe
import Data.List
import Data.List.Split

data Spec = Spec {
        sRemoveFiles :: [String]
    }

newSpec = Spec {
        sRemoveFiles = []
    }

processSpecials :: Opts -> Spec -> Document -> IO (Spec, Document)

processSpecials o s (Document b m) = do
    (s', b') <- processSpecials' o s b
    return (s', Document b' m)

processSpecials' :: Opts -> Spec -> [Block] -> IO (Spec, [Block])

processSpecials' opts spec (BVerbatim "digraph" b : xs) = do
    (spec', g)   <- mkGraph "dot" "digraph" opts spec b
    (spec'', rest) <- processSpecials' opts spec' xs
    return (spec'', (if null g
                     then BVerbatim "error" "Graphviz .digraph failed"
                     else BCommand "image" [g]) : rest)

processSpecials' opts spec (BVerbatim "graph" b : xs) = do
    (spec', g)   <- mkGraph "neato" "graph" opts spec b
    (spec'', rest) <- processSpecials' opts spec' xs
    return (spec'', (if null g
                     then BVerbatim "error" "Graphviz .graph failed"
                     else BCommand "image" [g]) : rest)

processSpecials' opts spec (BVerbatim "neato" b : xs) = do
    (spec', g)   <- mkGraph "neato" "" opts spec b
    (spec'', rest) <- processSpecials' opts spec' xs
    return (spec'', (if null g
                     then BVerbatim "error" "Graphviz .neato failed"
                     else BCommand "image" [g]) : rest)

processSpecials' opts spec (BVerbatim "dot" b : xs) = do
    (spec', g)   <- mkGraph "dot" "" opts spec b
    (spec'', rest) <- processSpecials' opts spec' xs
    return (spec'', (if null g
                     then BVerbatim "error" "Graphviz .dot failed"
                     else BCommand "image" [g]) : rest)

processSpecials' opts spec (BVerbatim "gnuplot" b : xs) = do
  (spec', g) <- mkGraph "gnuplot" "" opts spec b
  (spec'', rest) <- processSpecials' opts spec' xs
  return (spec'', (if null g
                   then BVerbatim "error" "Gnuplot .gp failed"
                   else BCommand "image" [g]) : rest)

processSpecials' opts spec (BVerbatim "mermaid" b : xs) = do
  (spec', g) <- mkGraph "mermaid" "" opts spec b
  (spec'', rest) <- processSpecials' opts spec' xs
  return (spec'', (if null g
                   then BVerbatim "error" "Mermaid .mmd failed"
                   else BCommand "image" [g]) : rest )

processSpecials' opts spec (BVerbatim "ditaa" b : xs) = do
  (spec', png) <- mkGraph "ditaa" "" opts spec b
  (spec'', rest) <- processSpecials' opts spec' xs
  return (spec'', (if null png
                   then BVerbatim "error" "ditaa .ditaa failed"
                   else BCommand "image" [png]) : rest )

processSpecials' opts spec (BVerbatim "plantuml" b : xs) = do
  (spec', g) <- mkGraph "plantuml" "" opts spec b
  (spec'', rest) <- processSpecials' opts spec' xs
  return (spec'', (if null g
                   then BVerbatim "error" "plantuml .plantuml failed"
                   else BCommand "image" [g]) : rest )

processSpecials' opts spec (x : xs) = do
    (spec', rest) <- processSpecials' opts spec xs
    return (spec', x : rest)

processSpecials' _ spec [] = return (spec, [])


randomString :: Int -> IO String
randomString 0 = return ""
randomString n = do
    char <- getStdRandom (randomR ('a', 'z'))
    str  <- randomString (n-1)
    return $ char : str

mkGraph e g opts spec c = do
    file <- randomString 10
    print $ file ++ "." ++ e
    case e of
      "dot" -> mkGraphDot e file g opts spec c
      "neato" -> mkGraphDot e file g opts spec c
      "gnuplot" -> mkGraphGnuPlot e file g opts spec c
      "mermaid" -> mkGraphMermaid e file g opts spec c
      "ditaa" -> mkGraphDitaa e file g opts spec c
      "plantuml" -> mkGraphPlantUML e file g opts spec c
      _ -> return (spec, [])


mkGraphDot e file g opts spec c = do
    let spec' = spec { sRemoveFiles = (file ++ ".jpeg") : (file ++ ".dot") : sRemoveFiles spec }
    writeFile (file ++ ".dot") (if null g then c else g ++ " G {\n" ++ c ++ "\n}\n")
    
    r <- exec (optVerbose opts) (optGraphviz opts) ["-Tjpeg", "-K" ++ e, file ++ ".dot", "-o" ++ file ++ ".jpeg"]
    return (spec', (either (const "") (const $ file ++ ".jpeg") r))  

mkGraphGnuPlot e file g opts spec c = do
    let spec' = spec { sRemoveFiles = (file ++ ".jpeg") : (file ++ ".gp") : sRemoveFiles spec }
    writeFile (file ++ ".gp") ("set terminal jpeg\n" ++ "set output \"" ++ file ++ ".jpeg" ++ "\"\n" ++c ++ "\n")

    r <- exec (optVerbose opts) (optGnuplot opts) [file ++ ".gp"]
    return (spec', (either (const "") (const $ file ++ ".jpeg") r))

mkGraphMermaid e file g opts spec c = do
  let spec' = spec { sRemoveFiles = (file ++ ".png") : (file ++ ".mmd") : sRemoveFiles spec}
  writeFile (file ++ ".mmd") (c ++ "\n")

  r <- exec (optVerbose opts) (optMermaid opts) ["-i", file ++ ".mmd" , "-o", file ++ ".png"]

  return (spec', (either (const "") (const $ file ++ ".png") r))

mkGraphDitaa e file g opts spec c = do
  let spec' = spec { sRemoveFiles = (file ++ ".png") : (file ++ ".ditaa") : sRemoveFiles spec}
  writeFile (file ++ ".ditaa") (c ++ "\n")

  r <- exec (optVerbose opts) (optJava opts) ["-jar", (optDitaa opts),file ++ ".ditaa", file ++ ".png"]

  return (spec', (either (const "") (const $ file ++ ".png") r))

mkGraphPlantUML e file g opts spec c = do
  let spec' = spec { sRemoveFiles = (file ++ ".png") : (file ++ ".plantuml") : sRemoveFiles spec }
  writeFile (file ++ ".plantuml") ("@startuml\n" ++ c ++ "\n@enduml\n")

  r <- exec (optVerbose opts) (optJava opts) ["-jar", (optPlantUML opts), file ++ ".plantuml"]
  
  return (spec', (either (const "") (const $ file ++ ".png") r))
