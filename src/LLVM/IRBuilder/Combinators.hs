{-# LANGUAGE RecursiveDo, PolyKinds, RoleAnnotations, OverloadedStrings #-}

module LLVM.IRBuilder.Combinators
  ( if'
  , loop, loopWhile, loopFor
  , pointerDiff
  , not'
  , allocate
  , minimum'
  , eq, ne, sge, sgt, sle, slt, uge, ugt, ule, ult
  , Path(..), mkPath, this
  , addr, deref, assign, update, increment, copy, swap
  ) where

import Control.Monad.Fix
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import LLVM.AST.Type
import LLVM.AST.Operand ( Operand(..) )
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import qualified LLVM.AST.IntegerPredicate as IP


eq, ne, sge, sgt, sle, slt, uge, ugt, ule, ult
  :: MonadIRBuilder m => Operand -> Operand -> m Operand
eq = icmp IP.EQ
ne = icmp IP.NE
sge = icmp IP.SGE
sgt = icmp IP.SGT
sle = icmp IP.SLE
slt = icmp IP.SLT
uge = icmp IP.UGE
ugt = icmp IP.UGT
ule = icmp IP.ULE
ult = icmp IP.ULT

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
        Signed -> slt
        Unsigned -> ult
  isLessThan <- inst a b
  select isLessThan a b


newtype Path (a :: k) (b :: k)
  = Path (NonEmpty Operand)
type role Path nominal nominal

mkPath :: [Operand] -> Path a b
mkPath path = Path (int32 0 :| path)

this :: Path a a
this = mkPath []

(->>) :: Path a b -> Path b c -> Path a c
Path a2b ->> Path b2c =
  let b2c' = if NE.head b2c == int32 0
               then NE.tail b2c
               else NE.toList b2c
   in Path $ NE.head a2b :| (NE.tail a2b ++ b2c')

addr :: (MonadModuleBuilder m, MonadIRBuilder m)
     => Path a b -> Operand -> m Operand
addr path p = gep p (pathToIndices path)
  where
    pathToIndices :: Path a b -> [Operand]
    pathToIndices (Path indices) =
      NE.toList indices

deref :: (MonadModuleBuilder m, MonadIRBuilder m)
      => Path a b -> Operand -> m Operand
deref path p = do
  addr <- addr path p
  load addr 0

assign :: (MonadModuleBuilder m, MonadIRBuilder m)
       => Path a b -> Operand -> Operand -> m ()
assign path p value = do
  dstAddr <- addr path p
  store dstAddr 0 value

update :: (MonadModuleBuilder m, MonadIRBuilder m)
       => Path a b
       -> Operand
       -> (Operand -> m Operand)
       -> m ()
update path p f = do
  dstAddr <- addr path p
  store dstAddr 0 =<< f =<< load dstAddr 0

increment :: (MonadModuleBuilder m, MonadIRBuilder m)
          => (Integer -> Operand) -> Path a b -> Operand -> m ()
increment ty path p = update path p (add (ty 1))

copy :: (MonadModuleBuilder m, MonadIRBuilder m)
     => Path a b -> Operand -> Operand -> m ()
copy path src dst = do
  value <- deref path src
  assign path dst value

swap :: (MonadModuleBuilder m, MonadIRBuilder m)
     => Path a b -> Operand -> Operand -> m ()
swap path lhs rhs = do
  tmp <- deref path lhs
  copy path rhs lhs
  assign path rhs tmp

