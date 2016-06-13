{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Timing
    ( -- * Introduction
      -- $intro

      -- * @MonadTiming@
      MonadTiming(..), TimingTree, Tag,

      -- * @TimingT@, general timing handler
      TimingT(..), runTimingT, execTimingT,

      -- * @NoTimingT@, a no-timing handler
      NoTimingT (..)
    ) where

import Control.Applicative         (Alternative (..), liftA2)
import Control.Arrow               (first, second)
import Control.Monad.Base          (MonadBase (..))
import Control.Monad.Catch         (MonadCatch (..), MonadMask (..),
                                    MonadThrow (..))
import Control.Monad.Cont
import Control.Monad.Except        (MonadError (..))
import Control.Monad.IO.Class      (MonadIO (..))
import Control.Monad.RWS
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Function
import Data.List
import Data.Time
import Data.Tree

-- | Monads that support timing subcomputations.
class MonadTiming m where
    -- | Time the given computation with the given tag.
    timeGroup :: Tag -> m a -> m a

-- | A tree of timing events.
type TimingTree = Tree (Tag, NominalDiffTime)

-- | Tags are strings.
type Tag = String

-- | A monad transformer that records timing events.
--
-- Note that this module exports both '_runTimingT' and 'runTimingT'.
--
-- * '_runTimingT' returns the timing events verbatim without sanitization.
-- * 'runTimingT' combines trees with the same 'Tag' and tags the duration
-- of the entire computation with the special tag @\<all\>@.
newtype TimingT m a = TimingT { _runTimingT :: m (a, [TimingTree]) }

evalTimingT :: Functor f => TimingT f b -> f b
evalTimingT = fmap fst . _runTimingT

-- | Run a @TimingT@ computation, discarding the result.
execTimingT :: MonadIO m => TimingT m b -> m TimingTree
execTimingT = fmap snd . runTimingT

-- | Run the giving @TimingT@ computation, recording timing events.
runTimingT :: MonadIO m => TimingT m a -> m (a, TimingTree)
runTimingT = fmap (second (head . condenseTree)) . _runTimingT . timeGroup "<all>"

condenseTree :: [TimingTree] -> [TimingTree]
condenseTree = map (foldl collapseNodes emptyNode)
             . groupBy ((==) `on` (fst . rootLabel)) where
    collapseNodes (Node (_, x) sub) (Node (t, x1) sub2)
        = Node (t, x + x1) (condenseTree $ sub ++ sub2)
    emptyNode = Node ("", 0) []

liftTimingT :: Functor m => m a -> TimingT m a
liftTimingT = TimingT . fmap (\ x -> (x, []))

instance Functor m => Functor (TimingT m) where
    fmap f (TimingT q) = TimingT (first f <$> q)

instance Applicative m => Applicative (TimingT m) where
    pure x = TimingT (pure (x, []))
    TimingT f <*> TimingT a' = TimingT $
        liftA2 (\ (a,b) (c,d) -> (a c, b ++ d)) f a'

instance Alternative m => Alternative (TimingT m) where
    empty = liftTimingT empty
    a <|> b = TimingT $ _runTimingT a <|> _runTimingT b

instance Monad m => Monad (TimingT m) where
    TimingT a >>= f = TimingT $ do
        (thing1, b) <- a
        (thing2, c) <- _runTimingT $ f thing1
        return (thing2, b ++ c)

instance MonadTrans TimingT where
    lift = liftTimingT

instance MonadFix m => MonadFix (TimingT m) where
    mfix f = lift $ mfix $ \ a -> evalTimingT (f a)

instance MonadPlus m => MonadPlus (TimingT m) where

instance MonadReader r m => MonadReader r (TimingT m) where
    local f m = TimingT $ local f $ _runTimingT m
    ask = lift ask

instance MonadWriter w m => MonadWriter w (TimingT m) where
    tell = lift . tell
    listen m = TimingT $ do
        ~((a, b), c) <- listen (_runTimingT m)
        return ((a, c), b)
    pass m = TimingT $ pass $ do
        ~((a, b), c) <- _runTimingT m
        return ((a, c), b)

instance MonadState s m => MonadState s (TimingT m) where
    get = lift get
    put = lift . put

instance MonadRWS r w s m => MonadRWS r w s (TimingT m)

instance MonadCont m => MonadCont (TimingT m) where
    callCC f = lift $ callCC $ \ c -> evalTimingT (f (TimingT . c))

instance MonadThrow m => MonadThrow (TimingT m) where
    throwM e = TimingT $ throwM e

instance MonadCatch m => MonadCatch (TimingT m) where
    TimingT a `catch` f = TimingT $ a `catch` (_runTimingT . f)

instance MonadMask m => MonadMask (TimingT m) where
    mask a = TimingT $ mask $ \ u -> _runTimingT (a $ q u) where
        q u (TimingT m) = TimingT (u m)
    uninterruptibleMask a = TimingT $ uninterruptibleMask $ \ u -> _runTimingT (a $ q u) where
        q u (TimingT m) = TimingT (u m)

instance MonadError e m => MonadError e (TimingT m) where
    throwError = TimingT . throwError
    TimingT a `catchError` f = TimingT $ a `catchError` (_runTimingT . f)

instance MonadBase b m => MonadBase b (TimingT m) where
    liftBase = lift . liftBase

instance MonadBaseControl b m => MonadBaseControl b (TimingT m) where
    type StM (TimingT m) a = StM m a

    liftBaseWith runInBase = lift $
        liftBaseWith (\ runInTiming -> runInBase (\ (TimingT f) -> runInTiming $ fmap fst f))

    restoreM = lift . restoreM

instance MonadIO m => MonadIO (TimingT m) where
    liftIO = lift . liftIO

instance MonadIO m => MonadTiming (TimingT m) where
    timeGroup s (TimingT m) = TimingT $ do
        t1 <- liftIO getCurrentTime
        (res, ps) <- m
        t2 <- liftIO getCurrentTime
        return (res, [Node (s, diffUTCTime t2 t1) ps])

-- | Run a computation without recording any timing data.
newtype NoTimingT m a = NoTimingT { runNoTimingT :: m a }
    deriving ( Functor, Applicative, Monad, MonadIO, Alternative, MonadFix, MonadPlus
             , MonadReader r, MonadState s, MonadWriter w, MonadRWS r w s
             , MonadError e, MonadCatch, MonadThrow, MonadMask
             )

instance MonadTrans NoTimingT where
    lift = NoTimingT

instance MonadBase b m => MonadBase b (NoTimingT m) where
    liftBase = lift . liftBase

instance MonadBaseControl b m => MonadBaseControl b (NoTimingT m) where
    type StM (NoTimingT m) a = StM m a

    liftBaseWith runInBase = lift $
        liftBaseWith (\ runInTiming -> runInBase (\ (NoTimingT f) -> runInTiming f))

    restoreM = lift . restoreM

instance MonadCont m => MonadCont (NoTimingT m) where
    callCC f = lift $ callCC $ \ c -> runNoTimingT (f (NoTimingT . c))

instance MonadTiming (NoTimingT m) where timeGroup _ = id

{- $intro
This package provides an mtl-like interface for timing subcomponents of an
arbitrary computation.
-}
