module PartialEvaluator
  ( peval,
  )
where

import Environments
import Values

-- | partially evaluate the given value under the given environment
peval :: Env v m -> Callback v m -> Value v m -> m ()
-- peval = peval'
peval = error "not implemented"
