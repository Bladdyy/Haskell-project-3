module Reducer where
import qualified Data.Map as Map
import Common

-- -- Performs a step of reduction on given state.
-- rstep :: StateDesc -> Int -> Maybe StateDesc
-- -- If not beginning of expression.
-- rstep (StateDesc (e1 :$ e2, ctx) his f depth mapping) args = 
--     rstep (StateDesc (left (e1 :$ e2, ctx)) his f depth mapping) (args + 1)

-- -- Beginning of expression.
-- rstep (StateDesc (e, ctx) his f depth mapping) args = 
--     if depth == max_depth then Nothing
--     else 
--       -- Check for matches for this expression.
--       case Map.lookup name mapping of
--           Def xs -> if length xs >= args then 
--                         findReduce (StateDesc (e1 :$ e2, ctx) his (f - 1) depth mapping) xs
--                     else Nothing
--           -- No matches.
--           _ -> Nothing
--       where

--     -- Checks any pattern matches expression in given order. 
--     findReduce :: StateDesc -> [Match] -> Maybe StateDesc
--     findReduce state (Match _ pats expr:xs) = 
--         case matchPats state pats expr Map.empty of
--           (_, False) -> findReduce state xs
--           (correct_mapping, True) -> 
--               Just (reduce (StateDesc (up loc) his f depth mapping) correct_mapping expr (length pats))

--     -- No pattern matches
--     findReduce state _ = Nothing

--     -- Checks every parameter in pattern with corresponding expression.
--     matchPats :: StateDesc -> [Pat] -> Expr -> ExprMap -> (ExprMap, Bool)
--     matchPats (StateDesc (e1, L cxt e2) his f depth mapping) (pat:ps) expr_result expr_map =
--       case checkPattern e2 pat expr_map of
--         -- Incorrect matching. Reduce if possible then check again.
--         (_, False) -> case forceCheck (StateDesc (right (up (e, L cxt expr))) his f depth mapping) pat expr_map of

--         -- Correctly matched parameter. Go to the next one.
--         (new_mapping, True) -> matchPats (StateDesc (up (e, L cxt expr)) his f depth mapping) ps new_mapping

--     -- All parameters matched.    
--     matchPats _ [] expr_map = (expr_map, True)
--     matchPats _ _ expr_map = (expr_map, False)

--     forceCheck :: StateDesc -> Pat -> ExprMap ->
--     forceCheck (StateDesc (e1, R e2 cxt) his f depth mapping) pat expr_map = 
--       if depth == 0 then let val = rstep (StateDesc (e1, R e2 cxt) mempty internal_steps (depth + 1) mapping)
--       else let val = rstep (StateDesc (e1, R e2 cxt) his f (depth + 1) mapping)
--       case rstep state of
--         Nothing -> Nothing
--         Just (StateDesc (new_e1, R new_e new_cxt) new_his new_f new_depth new_mapping) -> 
--           case checkPattern new_e1 pat expr_map of
--             (new_mapping, True) -> (new_mapping, True) -- New history add also, Not only mapping.
--             (_, False) -> forceCheck (StateDesc (new_e1, R new_e new_cxt) new_his new_f new_depth new_mapping) pat expr_map -- Check for errors here! (Arguments might be wrong)




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



