module ValueBuilders
    ( makeLambdaCPS
    )

where

import Values
import DebugInfo

makeLambdaCPS
    :: DebugInfo
    -> Env v m
    -> Value v m      -- ^ name of CPS return callback (symbol)
    -> Value v m      -- ^ argument (may be a list of symbols or a single symbol)
    -> [Value v m]    -- ^ body
    -> Value v m      -- ^ resulting lambda-cps object
makeLambdaCPS dinfo env retname arg body
    | (Right spec) <- mspec, (Value _ (Symbol retsym)) <- retname
    = Value dinfo $ LambdaCPS body (CArgSpec retsym spec) env
    | (Left err) <- mspec = Value dinfo err
    | otherwise = makeFailList dinfo "lambda-cps-malformed" [arg]
    where
        mspec = makeArgSpec arg

makeArgSpec :: Value v m -> CouldFail v m ArgSpec
makeArgSpec (Value _ (Pair (Value _ (Symbol argName)) vs)) = do
    rest <- makeArgSpec vs
    let restTail = tailName rest
    let restNames = argNames rest
    pure $ ArgSpec { argNames = argName:restNames, tailName = restTail }
makeArgSpec (Value _ Null) = pure $ ArgSpec { argNames = [], tailName = Nothing }
makeArgSpec (Value _ (Symbol tn)) = pure $ ArgSpec {argNames = [], tailName = Just tn}
makeArgSpec v = returnFailList "malformed-arg-list" [v]
