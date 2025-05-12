module Reducer where
import qualified Data.Map as Map
import Common




-- -- Performs one step of reduction.
-- rstep :: Expr -> DefMap -> (Maybe Expr, Int)
-- rstep expr mapping = let (new_expr, _, code, _) = findReduce expr 0 Nothing
--                                    in
--                                    (new_expr, code)
--     where
--     -- Finds first reducable part, reduces and passes it. Adds non-reducable part to prefix.
--     findReduce :: Expr -> Int -> Maybe Expr -> (Maybe Expr, Def, Int, [Expr])
--     findReduce (e1 :$ e2) n pref = 
--         let (new_pref, Def nam par ex, to_param, lst) = findReduce e1 (n + 1) pref
--           in
--           case to_param of
--             -- Looking for reducable part.
--             (-1) -> let (small_pref, Def nam' par' ex', to_param', lst') = findReduce e2 0 Nothing
--                       in 
--                       case small_pref of
--                         Just pref' -> (Just (merge new_pref pref'), 
--                                               Def nam' par' ex', to_param', lst')
--                         Nothing -> (new_pref, Def nam' par' ex', to_param', lst')
--             -- Reducable part found and reduced already.
--             (-2) -> (Just (merge new_pref e2), Def nam par ex, to_param, lst)
--             -- Last argument for reduction. 
--             1 -> (Just (merge new_pref (reduce ex par (reverse (e2:lst)))), Def nam par ex, -2, [])
--             -- Middle argument for reduction.
--             _ -> (new_pref, Def nam par ex, to_param - 1, e2 : lst)

--     findReduce (Var name) n pref = 
--         case Map.lookup name mapping of
--           Nothing -> (Just (merge pref (Var name)), Def "a" [] (Var "a"), -1, [])
--           Just (Def _ pats expr') -> 
--               -- No arguments needed.
--               if length pats == 0 
--                then (Just (merge pref (reduce expr' pats [])), (Def name pats expr'), -2, [])
--               -- Enough arguments to perform reduction.
--               else if length pats <= n 
--                then (pref, (Def name pats expr'), length pats, [])
--               -- Not enough arguments to perform reduction.
--               else
--                 (Just (merge pref (Var name)), Def "a" [] (Var "a"), -1, [])
    

--     -- Connects two expressions.
--     merge :: Maybe Expr -> Expr -> Expr
--     merge Nothing e = e
--     merge (Just e1) e2 = e1 :$ e2


--     -- Creates submap for reduced expression.
--     reduce :: Expr -> [String] -> [Expr] -> Expr                   
--     reduce expr' def_pats params = subReduce expr' (Map.fromList (zip def_pats params))


--     -- Performes reduction.
--     subReduce :: Expr -> ExprMap -> Expr
--     subReduce (a :$ b) small_map = subReduce a small_map :$ subReduce b small_map
--     subReduce (Var name) small_map = case Map.lookup name small_map of
--                                      Nothing -> Var name
--                                      Just val -> val



-- -- Reduces and prints given expression until it is fully reduced or steps are depleted. 
-- iterateSteps :: Int -> Expr -> DefMap -> IO ()
-- iterateSteps 0 _ _ = return ()  -- Stop after n steps
-- iterateSteps n rest mapping = do
--     let (expr, code) = rstep rest mapping
--     case expr of
--       Nothing -> return ()
--       Just expr' -> do
--                   case code of 
--                       (-1) -> return ()
--                       _ -> do 
--                           print expr'
--                           iterateSteps (n - 1) expr' mapping