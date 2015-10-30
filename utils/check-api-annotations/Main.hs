{-# LANGUAGE RankNTypes #-}

import Data.Data
import Data.List
import GHC
import DynFlags
import Outputable
import ApiAnnotation
import System.Environment( getArgs )
import qualified Data.Map as Map
import qualified Data.Set as Set

main::IO()
main = do
  args <- getArgs
  case args of
   [libdir,fileName] -> testOneFile libdir fileName
   _ -> putStrLn "invoke with the libdir and a file to parse."

testOneFile :: FilePath -> String -> IO ()
testOneFile libdir fileName = do
       ((anns,cs),p) <- runGhc (Just libdir) $ do
                        dflags <- getSessionDynFlags
                        _ <- setSessionDynFlags dflags
                        let mn =mkModuleName fileName
                        addTarget Target { targetId = TargetModule mn
                                         , targetAllowObjCode = True
                                         , targetContents = Nothing }
                        _ <- load LoadAllTargets
                        modSum <- getModSummary mn
                        p <- parseModule modSum
                        return (pm_annotations p,p)

       let sspans = Set.fromList $ getAllSrcSpans (pm_parsed_source p)

           problems = filter (\(s,_a) -> not (Set.member s sspans))
                             $ getAnnSrcSpans (anns,cs)

           exploded = [((kw,ss),[anchor])
                      | ((anchor,kw),sss) <- Map.toList anns,ss <- sss]

           exploded' = Map.toList $ Map.fromListWith (++) exploded

           problems' = filter (\(_,anchors)
                               -> not (any (\a -> Set.member a sspans) anchors))
                              exploded'

       putStrLn "---Problems---------------------"
       putStrLn (intercalate "\n" [showAnns $ Map.fromList $ map snd problems])
       putStrLn "---Problems'--------------------"
       putStrLn (intercalate "\n" [pp $ Map.fromList $ map fst problems'])
       putStrLn "--------------------------------"
       putStrLn (intercalate "\n" [showAnns anns])

    where
      getAnnSrcSpans :: ApiAnns -> [(SrcSpan,(ApiAnnKey,[SrcSpan]))]
      getAnnSrcSpans (anns,_) = map (\a@((ss,_),_) -> (ss,a)) $ Map.toList anns

      getAllSrcSpans :: (Data t) => t -> [SrcSpan]
      getAllSrcSpans ast = everything (++) ([] `mkQ` getSrcSpan) ast
        where
          getSrcSpan :: SrcSpan -> [SrcSpan]
          getSrcSpan ss = [ss]


showAnns :: Map.Map ApiAnnKey [SrcSpan] -> String
showAnns anns = "[\n" ++ (intercalate "\n"
   $ map (\((s,k),v)
              -> ("(AK " ++ pp s ++ " " ++ show k ++" = " ++ pp v ++ ")\n"))
   $ Map.toList anns)
    ++ "]\n"

pp :: (Outputable a) => a -> String
pp a = showPpr unsafeGlobalDynFlags a


-- ---------------------------------------------------------------------

-- Copied from syb for the test


-- | Generic queries of type \"r\",
--   i.e., take any \"a\" and return an \"r\"
--
type GenericQ r = forall a. Data a => a -> r


-- | Make a generic query;
--   start from a type-specific case;
--   return a constant otherwise
--
mkQ :: ( Typeable a
       , Typeable b
       )
    => r
    -> (b -> r)
    -> a
    -> r
(r `mkQ` br) a = case cast a of
                        Just b  -> br b
                        Nothing -> r



-- | Summarise all nodes in top-down, left-to-right order
everything :: (r -> r -> r) -> GenericQ r -> GenericQ r

-- Apply f to x to summarise top-level node;
-- use gmapQ to recurse into immediate subterms;
-- use ordinary foldl to reduce list of intermediate results

everything k f x = foldl k (f x) (gmapQ (everything k f) x)