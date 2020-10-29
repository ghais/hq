{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Q.MonteCarlo where
import           Control.Monad.State
import           Data.RVar
import           Q.Stochastic.Discretize
import           Q.Stochastic.Process
type Path b = [(Time, b)]

-- | Summary type class aggregates all priced values of paths
class (PathPricer p)  => Summary m p | m->p where
        -- | Updates summary with given priced pathes
        sSummarize      :: m -> [p] -> m
        -- | Defines a metric, i.e. calculate distance between 2 summaries
        sNorm           :: m -> m -> Double

-- | Path generator is a stochastic path generator
class PathGenerator m where
        pgMkNew         :: m->IO m
        pgGenerate      :: Integer -> m -> Path b

-- | Path pricer provides a price for given path
class PathPricer m where
        ppPrice :: m -> Path b -> m


type MonteCarlo s a = StateT [(Time, s)] RVar a

generatePath :: forall a b d. (StochasticProcess a b, Discretize d b) => d -> a -> b -> [Time] -> [RVar b] -> RVar [b]
generatePath disc p initState times dws = reverse <$> evalStateT (onePath times dws) initState' where
  initState' :: [(Time, b)]
  initState' = [(0, initState)]

  onePath :: [Time] -> [RVar b] -> MonteCarlo b [b]
  onePath [] _ = do
    s <- get
    return $ map snd s
  onePath (t1:tn) (dw1:dws) = do
    s <- get
    let t0 = head s
    b <- lift $ pEvolve p disc t0 t1 dw1
    put $ (t1, b) : s
    onePath tn dws


