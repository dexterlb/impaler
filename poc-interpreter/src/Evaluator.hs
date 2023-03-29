module Evaluator
    ( eval
    )

where

import qualified Data.Text as T

import Values
import Environments
import PrimitiveData
import DebugInfo

-- | eval the given value under the given environment
eval :: (Monad m) => Env m -> Callback m -> Value m -> m ()
eval env ret v@(Value _ (Pair (Value _ x) xs))
    | (Symbol sym) <- x, isSpecialForm sym = evalSpecialForm env ret sym xs
    | otherwise = evalList env (callSexpr ret) v
eval env ret (Value dinfo (Symbol i)) = ret $ envGet dinfo i env
eval _   ret v = ret $ v   -- all other values evaluate to themselves

callSexpr   :: (Monad m)
            => Callback m
            -> Value m  -- ^ sexpr to call
            -> m ()
callSexpr ret (Value _ (Pair f arg)) = call ret f arg
callSexpr ret v@(Value dinfo _) = ret $ makeFailList dinfo "expected-sexpr" [v]

-- | execute the given callable
call  :: (Monad m)
      => Callback m -- ^ callback to call with result
      -> Value m    -- ^ callable
      -> Value m    -- ^ argument
      -> m ()
call ret (Value _ (UnsafeBuiltinFunc f)) arg = f ret arg
call ret (Value _ (CLambda body argn env)) arg = callCLambda ret env argn arg body
call ret expr@(Value dinfo _) _ = ret $ makeFailList dinfo "dont-know-how-to-call" [expr]

callCLambda :: forall m. (Monad m)
            => Callback m   -- ^ callback to call with result
            -> Env m        -- ^ closure that comes with lambda
            -> CArgSpec     -- ^ formal arguments (argument names)
            -> Value m      -- ^ arguments
            -> [Value m]    -- ^ body (list of statements)
            -> m ()
callCLambda ret closure argspec arg body = mapM_ evalBodyStatement body
    where
        evalBodyStatement :: Value m -> m ()
        evalBodyStatement = eval env void

        -- | callback that discards its argument
        -- | (expressions in body are interpreted as statements and their results are discarded)
        void :: Callback m
        void = error "not implemented"

        env :: Env m
        env = makeCLambdaEnv argspec ret arg closure


makeCLambdaEnv
    :: CArgSpec
    -> Callback m   -- ^ CPS callback
    -> Value m      -- ^ argument
    -> Env m        -- ^ closure
    -> Env m
makeCLambdaEnv (CArgSpec retname argspec) ret arg closure
    = envAdd retname (makeCallableFromCallback ret)
    $ envUnion (bindArgs argspec arg) closure

makeCallableFromCallback :: Callback m -> Value m
makeCallableFromCallback = error "not implemented"

bindArgs :: ArgSpec -> Value m -> Env m
bindArgs = error "not implemented"

-- | evaluate all elements in a given list
-- | afterwards, pass a list of evaluated items to the callback
evalList :: (Monad m) => Env m -> Callback m -> Value m -> m ()
evalList _ ret v@(Value _ Null) = ret v
evalList env ret (Value dinfo (Pair x xs)) = eval env g x
    where
        g evalledX = evalList env ret' xs
            where
                ret' evalledXS = ret $ Value dinfo (Pair evalledX evalledXS)
evalList _ ret v@(Value dinfo _) = ret $ makeFailList dinfo "trying-to-call-something-thats-not-list" [v]

isSpecialForm :: Identifier -> Bool
isSpecialForm "clambda" = True
isSpecialForm _ = False

evalSpecialForm :: Env m -> Callback m -> Identifier -> Value m -> m ()
evalSpecialForm env ret "clambda" (Value dinfo (Pair retname (Value _ (Pair arg bodyVal))))
    | (Just body) <- valToList bodyVal = ret $ makeClambda dinfo env retname arg body
    | otherwise = ret $ makeFailList dinfo "clambda-body-not-list" [bodyVal]
evalSpecialForm _   ret "clambda" val@(Value dinfo _)
    = ret $ makeFailList dinfo "clambda-malformed" [val]
evalSpecialForm _ _ (Identifier i) _ = error $ "no special form handler defined for '" <> (T.unpack i) <> "' - this is a bug."

makeClambda
    :: DebugInfo
    -> Env m
    -> Value m      -- ^ name of CPS return callback (symbol)
    -> Value m      -- ^ argument (may be a list of symbols or a single symbol)
    -> [Value m]    -- ^ body
    -> Value m      -- ^ resulting clambda object
makeClambda dinfo env retname arg body
    | (Just argsyms) <- vtsymlist vtarg, (Just retsym) <- vtsym vtretname
    = Value dinfo $ CLambda body (CArgSpec retsym $ ArgSpecList argsyms) env
    | (Just argsym)  <- vtsym vtarg, (Just retsym) <- vtsym vtretname
    = Value dinfo $ CLambda body (CArgSpec retsym $ ArgSpecCombined argsym) env
    | otherwise = makeFailList dinfo "clambda-args-malformed" [arg]
    where
        vtarg = toValTree arg
        vtretname = toValTree retname
