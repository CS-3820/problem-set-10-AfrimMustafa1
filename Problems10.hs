{-# LANGUAGE StandaloneDeriving #-}
module Problems10 where

-- Expression data type
data Expr
    = Const Int             -- Constants
    | Plus Expr Expr        -- Addition
    | Var String            -- Variables
    | Lam String Expr       -- Lambda abstractions
    | App Expr Expr         -- Function applications
    | Store Expr            -- Store operation
    | Recall                -- Recall operation
    | Throw Expr            -- Throw expression
    | Catch Expr String Expr-- Catch expression
    deriving (Eq, Show)

-- Function to check if an expression is a value
isValue :: Expr -> Bool
isValue (Const _)     = True
isValue (Lam _ _)     = True
isValue _             = False

-- Function to check if an expression is a 'Throw'
isThrow :: Expr -> Bool
isThrow (Throw _) = True
isThrow _         = False

-- Substitution function
subst :: String -> Expr -> Expr -> Expr
subst x s (Var y)
    | x == y    = s
    | otherwise = Var y
subst x s (Const i)     = Const i
subst x s (Plus e1 e2)  = Plus (subst x s e1) (subst x s e2)
subst x s (Lam y e)
    | x == y    = Lam y e  -- Do not substitute bound variables
    | otherwise = Lam y (subst x s e)
subst x s (App e1 e2)   = App (subst x s e1) (subst x s e2)
subst x s (Store e)     = Store (subst x s e)
subst x s Recall        = Recall
subst x s (Throw e)     = Throw (subst x s e)
subst x s (Catch m y n)
    | x == y    = Catch (subst x s m) y n  -- Do not substitute in handler variable
    | otherwise = Catch (subst x s m) y (subst x s n)

-- Small-step semantics function
smallStep :: (Expr, Expr) -> Maybe (Expr, Expr)
smallStep (e, acc) = case e of
    -- Constants and Lambdas are values; no reduction
    Const _ -> Nothing
    Lam _ _ -> Nothing

    -- Variables cannot be reduced further
    Var _ -> Nothing

    -- Addition
    Plus e1 e2
        | isThrow e1 -> return (e1, acc)
        | not (isValue e1) -> do
            (e1', acc') <- smallStep (e1, acc)
            return (Plus e1' e2, acc')
        | isThrow e2 -> return (e2, acc)
        | not (isValue e2) -> do
            (e2', acc') <- smallStep (e2, acc)
            return (Plus e1 e2', acc')
        | otherwise -> case (e1, e2) of
            (Const n1, Const n2) -> return (Const (n1 + n2), acc)
            _ -> Nothing

    -- Application
    App e1 e2
        | isThrow e1 -> return (e1, acc)  -- Exception from function position
        | not (isValue e1) -> do
            (e1', acc') <- smallStep (e1, acc)
            return (App e1' e2, acc')
        | isThrow e2 -> return (e2, acc)
        | not (isValue e2) -> do
            (e2', acc') <- smallStep (e2, acc)
            return (App e1 e2', acc')
        | otherwise -> case e1 of
            Lam x body -> return (subst x e2 body, acc)
            _ -> Nothing

    -- Store
    Store e1
        | isThrow e1 -> 
            case e1 of 
              Throw x -> if isValue x then return (e1, acc) 
              else 
                case smallStep(x,acc) of
                  Just (x',acc') -> return (Store (Throw x'),acc')
                  Nothing -> Just (Throw x,acc)
        | not (isValue e1) -> do
            (e1', acc') <- smallStep (e1, acc)
            return (Store e1', acc')
        | otherwise -> return (e1, e1)  -- Update accumulator with e1

    -- Recall
    Recall -> return (acc, acc)

    -- Throw
    Throw e1
        | isValue e1 -> Nothing
        | otherwise  -> 
            case e1 of
              Throw e1' -> return (Throw e1', acc)
              _         -> 
                case smallStep (e1,acc) of
                  Just (e1',acc') -> return (Throw e1', acc')
                  Nothing -> (Nothing)

    -- Catch
    Catch m y n
        | isThrow m -> case m of
            Throw w -> return (subst y w n, acc)  -- Substitute and continue
            _ -> Nothing
        | not (isValue m) -> do
            (m', acc') <- smallStep (m, acc)
            return (Catch m' y n, acc')
        | otherwise -> return (m, acc)

-- Function to compute all steps
steps :: (Expr, Expr) -> [(Expr, Expr)]
steps s = s : case smallStep s of
    Nothing -> []
    Just s' -> steps s'

-- Helper function to print each step
prints :: Show a => [a] -> IO ()
prints = mapM_ print