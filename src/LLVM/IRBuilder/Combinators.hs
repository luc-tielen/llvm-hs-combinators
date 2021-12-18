{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module LLVM.IRBuilder.Combinators
  ( if'
  , loop
  , loopWhile
  , loopFor
  , pointerDiff
  , not'
  , allocate
  , minimum'
  ) where

import Control.Monad.Fix
import LLVM.AST.Type
import LLVM.AST.Operand ( Operand(..) )
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import qualified LLVM.AST.IntegerPredicate as IP


allocate :: MonadIRBuilder m => Type -> Operand -> m Operand
allocate ty beginValue = do
  value <- alloca ty (Just (int32 1)) 0
  store value 0 beginValue
  pure value

if' :: (MonadIRBuilder m, MonadFix m)
    => Operand -> m a -> m ()
if' condition asm = mdo
  condBr condition ifBlock end
  ifBlock <- block `named` "if"
  asm
  br end
  end <- block `named` "end_if"
  pure ()

-- Note: this loops forever, only way to exit is if the inner block of ASM
-- Jumps to a label outside the loop
loop :: (MonadIRBuilder m, MonadFix m) => m a -> m ()
loop asm = mdo
  br begin
  begin <- block `named` "loop"
  asm
  br begin

loopWhile :: (MonadIRBuilder m, MonadFix m)
          => m Operand -> m a -> m ()
loopWhile condition asm = mdo
  br begin
  begin <- block `named` "while_begin"
  result <- condition
  condBr result body end
  body <- block `named` "while_body"
  asm
  br begin
  end <- block `named` "while_end"
  pure ()

loopFor :: (MonadModuleBuilder m, MonadIRBuilder m, MonadFix m)
        => Operand
        -> (Operand -> m Operand)
        -> (Operand -> m Operand)
        -> (Operand -> m a)
        -> m ()
loopFor beginValue condition post asm = mdo
  start <- currentBlock
  br begin
  begin <- block `named` "for_begin"
  loopValue <- phi [(beginValue, start), (updatedValue, bodyEnd)]
  result <- condition loopValue
  condBr result bodyStart end
  bodyStart <- block `named` "for_body"
  asm loopValue
  updatedValue <- post loopValue
  bodyEnd <- currentBlock
  br begin
  end <- block `named` "for_end"
  pure ()

pointerDiff :: (MonadModuleBuilder m, MonadIRBuilder m)
            => Type -> Operand -> Operand -> m Operand
pointerDiff ty a b = do
  a' <- ptrtoint a i64
  b' <- ptrtoint b i64
  result <- sub a' b'
  trunc result ty

-- NOTE: assumes input is of type i1
not' :: (MonadModuleBuilder m, MonadIRBuilder m)
     => Operand -> m Operand
not' bool = select bool (bit 0) (bit 1)

data Signedness = Signed | Unsigned

-- NOTE: only works for unsigned integers!
minimum' :: (MonadModuleBuilder m, MonadIRBuilder m)
         => Signedness -> Operand -> Operand -> m Operand
minimum' sign a b = do
  let inst = case sign of
        Signed -> icmp IP.SLT
        Unsigned -> icmp IP.ULT
  isLessThan <- inst a b
  select isLessThan a b

