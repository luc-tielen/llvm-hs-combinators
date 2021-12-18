{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module LLVM.IRBuilder.Combinators
  ( loop
  , loopWhile
  , loopFor
  ) where

import Control.Monad.Fix
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.AST.Operand ( Operand(..) )

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

