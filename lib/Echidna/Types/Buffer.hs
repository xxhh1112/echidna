{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Echidna.Types.Buffer where

import Data.ByteString (ByteString)
import EVM.Types (Expr(ConcreteBuf, Lit, LitAddr, WAddr), EType(Buf, EWord, EAddr), W256, Addr)

forceBuf :: Expr 'Buf -> ByteString
forceBuf (ConcreteBuf b) = b
forceBuf _ = error "expected ConcreteBuf"

forceLit :: Expr 'EWord -> W256
forceLit x = case x of
  Lit x' -> x'
  WAddr x' -> fromIntegral $ forceLitAddr x'
  _ -> error $ "expected Lit: " <> show x

forceLitAddr :: Expr 'EAddr -> Addr
forceLitAddr x = case x of
  LitAddr x' -> x'
  _ -> error "expected Lit"
