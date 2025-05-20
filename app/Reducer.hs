module Reducer where
import qualified Data.Map as Map
import Common

-- Performs a step of reduction on given state.
rstep :: StateDesc -> (StateDesc, Bool)
-- If not beginning of expression.
rstep (StateDesc (Loc (e1 :$ e2) ctx) his f int mapping args) = 
    StateDesc (left (Loc (e1 :$ e2) ctx)) his f int mapping (args + 1)
-- Beginning of expression.
rstep (StateDesc (Loc e ctx) his f int mapping args) = 
    -- Check for matches for this expression.
    case Map.lookup name mapping of
        Def xs -> if length xs >= args then findReduce (StateDesc (Loc e1 : e2) his (f - 1) int mapping args) xs
                  else (StateDesc (Loc e ctx) his f int mapping args, False)
        -- No matches.
        _ -> (StateDesc (Loc e ctx) his f int mapping args, False) -- TODO possibly error required.
    where

    -- Checks any pattern matches expression in given order. 
    findReduce :: StateDesc -> [Match] -> (StateDesc, Bool)
    findReduce (StateDesc loc his f int mapping args) (x:xs) = 
        -- If already in internal matching.
        if int then 
          let val = checkArgs (StateDesc loc his f int mapping 0) x
        -- If matching first level argument.
        else 
          let val = checkArgs (StateDesc loc mempty internal_steps True mapping 0) x
        in
        case val of
          (_, False) -> findReduce (StateDesc loc his f int mapping args) xs
          (state, True) -> (state, True) -- TODO: potentially add histories.
    
    -- No pattern matches
    findReduce state _ = (state, False)

    -- Checks one pattern with Loc.
    checkArgs :: StateDesc -> Match -> (StateDesc, Bool)
    checkArgs (StateDesc loc his f int mapping args) (Match _ pats expr) = 
        case matchArgs (StateDesc (top loc) his f int mapping args) pats Map.empty of
          -- Pattern not matched.
          (_, False) -> ((StateDesc loc his f int mapping args), False)
          -- Pattern matched.
          (correct_mapping, True) -> (reduce (StateDesc (top loc) his f int mapping args) correct_mapping expr pats)

    -- Checks every parameter in patter with next Loc arguments.
    matchArgs :: StateDesc -> [Pat] -> ExprMap -> (ExprMap, Bool)
    matchArgs (StateDesc (Loc e1 :$ e2) his f int mapping args) (pat:ps) expr_map =
      case checkPattern e2 pat expr_map of
        -- Incorrect matching. Reduce if possible then check again.
        (_, False) -> ... -- TODO: Match with given pat until no fuel or cant reduce or matched.
        -- Correctly matched parameter. Go to the next one.
        (new_mapping, True) -> matchArgs (StateDesc (top (Loc (e1 :$ e2) ctx)) his f int mapping args) ps new_mapping

    -- All parameters matched.    
    matchArgs _ [] expr_map = (expr_map, True)

    

    -- Checks if pattern matches for expr. If the pattern is correct: passes correct mapping.  TODO (maybe change it into zipper.)

    checkPattern :: Expr -> Pat -> ExprMap -> (ExprMap, Bool)
    checkPattern e pat maps = comp (expToLst e []) pat maps
      where
      -- Compares one parameter with expression.
      comp :: [Expr] -> Pat -> ExprMap -> (ExprMap, Bool) 
      comp (Con e:es) (PApp name lst) mapping = if e == name && length es == length lst then compLsts es lst mapping
                                                else (mapping, False)
      comp (x:xs) (PVar name) mapping = (Map.insert name (lstToExp x xs) mapping, True)
      comp _ _ mapping = (mapping, False) 

      -- Compares a whole list of parameters inside of a parameter.
      compLsts :: [Expr] -> [Pat] -> ExprMap -> (ExprMap, Bool)
      compLsts (x:xs) (y:ys) mapping = case checkPattern x y mapping of 
                                   (new_mapping, False) -> (new_mapping, False)
                                   (new_mapping, True) -> compLsts xs ys new_mapping
      compLsts _ _ mapping = (mapping, True)

      -- Changes Expr to list.
      expToLst :: Expr -> [Expr] -> [Expr]
      expToLst (e1 :$ e2) lst = expToLst e1 (e2:lst)
      expToLst e lst = e:lst

      lstToExp :: Expr -> [Expr] -> Expr
      lstToExp e0 (e1:e2) = lstToExp (e0 :$ e1) e2
      lstToExp e _ = e



-- --     -- Connects two expressions.
-- --     merge :: Maybe Expr -> Expr -> Expr
-- --     merge Nothing e = e
-- --     merge (Just e1) e2 = e1 :$ e2


-- --     -- Creates submap for reduced expression.
-- --     reduce :: Expr -> [String] -> [Expr] -> Expr                   
-- --     reduce expr' def_pats params = subReduce expr' (Map.fromList (zip def_pats params))


-- --     -- Performes reduction.
-- --     subReduce :: Expr -> ExprMap -> Expr
-- --     subReduce (a :$ b) small_map = subReduce a small_map :$ subReduce b small_map
-- --     subReduce (Var name) small_map = case Map.lookup name small_map of
-- --                                      Nothing -> Var name
-- --                                      Just val -> val



