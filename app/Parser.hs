module Parser where
import Language.Haskell.Parser
import Language.Haskell.Syntax
import qualified Data.Set as Set
import Common

-- Reads file, parses it and returns Prog made of given file.
fromHsString :: FilePath -> IO ([Match])
fromHsString path = do
                  content <- readFile path
                  return (fromParseResult content)
    where
        -- Parses content of file and returns list of Match in it.
        fromParseResult :: String -> [Match]
        fromParseResult content' = case parseModule content' of
                                    ParseOk ins -> fromHsModule ins
                                    ParseFailed _ err -> error ("ERROR: Parse error: " ++ err)
        

        -- Extracts list of Match from HsModule.
        fromHsModule :: HsModule -> [Match]
        fromHsModule (HsModule _ _ _ _ funs) = extractFun funs

        -- Changes every HsDecl into coresponding Match.
        extractFun :: [HsDecl] -> [Match]
        extractFun (HsFunBind mat:xs) = let ans = extractFun xs
                                            in 
                                            extractMatch mat ans
        extractFun ((HsPatBind _ (HsPVar name) out _ ):xs) = let ans = extractFun xs
                                                               in 
                                                               process name Nothing out : ans
        extractFun _ = []


        -- Extracts Match from single HsMatch.
        extractMatch :: [HsMatch] -> [Match] -> [Match]
        extractMatch (HsMatch _ name params out _:xs') ans'= 
          let new_ans = extractMatch xs' ans' 
          in
            process name (Just params) out : new_ans
        extractMatch _ ans' = ans'


        -- Validates and combines parts of Match.
        process :: HsName -> Maybe [HsPat]-> HsRhs -> Match
        process (HsIdent name') (Just params') (HsUnGuardedRhs out') = 
                                            let (pars, _) = validPar params' name'
                                              in
                                              case name' of
                                              "main" -> error "ERROR: main can't have parameters."
                                              _ -> Match name' pars (extractExpr out')
        process (HsIdent name') Nothing (HsUnGuardedRhs out') = Match name' [] (extractExpr out')
        process _ _ _ = error "INTERNAL ERROR: Not expected pattern in process."                                            


        -- Validates and parses parameters. 
        validPar :: [HsPat] -> Name -> ([Pat], Set.Set Name)
        validPar (par:xs) name =
          let (lst, set) = validPar xs name
              (extracted, set') = extractPat par set
            in
             (extracted:lst, set')
        
        validPar _ _ = ([], Set.empty)
        

        -- Extracts expression from HsExp.
        extractExpr :: HsExp -> Expr
        extractExpr (HsApp left right) = extractExpr left :$ extractExpr right
        extractExpr (HsVar (UnQual (HsIdent name))) = Var name
        extractExpr (HsCon (UnQual (HsIdent name))) = Con name
        extractExpr (HsParen var) = extractExpr var
        extractExpr _ = error "INTERNAL ERROR: Not expected pattern in extractExpr."
                                                

        -- Extracts parameters from Hs.
        extractPat :: HsPat -> Set.Set Name -> (Pat, Set.Set Name)
        extractPat (HsPParen par) set = extractPat par set
        extractPat (HsPApp (UnQual (HsIdent name)) lst) set = 
            let (lst', set') = extractList lst set
              in
              (PApp name lst', set')
          where 
            extractList :: [HsPat] -> Set.Set Name -> ([Pat], Set.Set Name)
            extractList (x:xs) set' = 
              let (new_lst, new_set) = extractList xs set'
                  (new_el, ext_set) = extractPat x new_set
                in
                (new_el:new_lst, ext_set)
            extractList _ set' = ([], set')

        extractPat (HsPVar (HsIdent name)) set = case Set.member name set of
                                                  True -> error ("Two variables with the same name: " ++ name ++ " in one definition.")
                                                  False -> (PVar name, Set.insert name set) 
        extractPat _ _ = error "INTERNAL ERROR: Not expected pattern in extractPat."