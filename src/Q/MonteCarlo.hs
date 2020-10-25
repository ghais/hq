{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Q.MonteCarlo where
import Q.Stochastic.Process


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

        

