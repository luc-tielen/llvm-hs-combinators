{-# LANGUAGE RecursiveDo, PolyKinds, RoleAnnotations, OverloadedStrings #-}

-- | A module that provides combinators for high level constructs commonly
--   found in other languages.
module LLVM.IRBuilder.Combinators
  ( -- * Control flow
    if'
  , loop, loopWhile, loopFor
  -- * GEP-/pointer-related combinators
  , Path(..), mkPath, this
  , addr, deref, assign, update, increment, copy, swap
  -- * Comparisons
  , eq, ne, sge, sgt, sle, slt, uge, ugt, ule, ult
  -- * General helper functions
  , pointerDiff
  , not'
  , minimum'
  , allocate
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


-- | Helper for not equal comparison "=="
eq :: MonadIRBuilder m => Operand -> Operand -> m Operand
eq = icmp IP.EQ

-- | Helper for not equal comparison "!="
ne :: MonadIRBuilder m => Operand -> Operand -> m Operand
ne = icmp IP.NE

-- | Helper for signed greater than or equal comparison ">="
sge :: MonadIRBuilder m => Operand -> Operand -> m Operand
sge = icmp IP.SGE

-- | Helper for signed greater than comparison ">"
sgt :: MonadIRBuilder m => Operand -> Operand -> m Operand
sgt = icmp IP.SGT

-- | Helper for signed less than or equal comparison "<="
sle :: MonadIRBuilder m => Operand -> Operand -> m Operand
sle = icmp IP.SLE

-- | Helper for signed less than comparison "<"
slt :: MonadIRBuilder m => Operand -> Operand -> m Operand
slt = icmp IP.SLT

-- | Helper for unsigned greater or equal comparison ">="
uge :: MonadIRBuilder m => Operand -> Operand -> m Operand
uge = icmp IP.UGE

-- | Helper for unsigned greater than comparison ">"
ugt :: MonadIRBuilder m => Operand -> Operand -> m Operand
ugt = icmp IP.UGT

-- | Helper for unsigned less than or equal comparison "<="
ule :: MonadIRBuilder m => Operand -> Operand -> m Operand
ule = icmp IP.ULE

-- | Helper for unsigned less than comparison "<"
ult :: MonadIRBuilder m => Operand -> Operand -> m Operand
ult = icmp IP.ULT

-- | Allocates a value of a certain type on the stack and initializes it with
--   the value contained in the 2nd argument.
--   Note that no checks are made if the operand is of the same 'Type'!
allocate :: MonadIRBuilder m => Type -> Operand -> m Operand
allocate ty beginValue = do
  value <- alloca ty (Just (int32 1)) 0
  store value 0 beginValue
  pure value

-- | Combinator for an if statement that contains no else block.
--   The operand resembles the value of the condition that needs to be checked
--   in the if. The monadic action resembles a block of statements that will be
--   inserted inside the body of the if.
--   Note that the type of the operand is not checked. It should be of type
--   'i1' or LLVM will crash when trying to compile the generated code.
if' :: (MonadIRBuilder m, MonadFix m)
    => Operand -> m a -> m ()
if' condition asm = mdo
  condBr condition ifBlock end
  ifBlock <- block `named` "if"
  asm
  br end
  end <- block `named` "end_if"
  pure ()

-- | Combinator for creating an infinite loop ("while(true) { ... }").
--   The monadic action resembles a block of LLVM statements that will be
--   inserted inside the body of the loop.
--   Note: It might seem like this would loop forever, but you can exit the
--   loop by jumping to a label outside of this loop block
--   (just like in another language, a break or return statement in loop would
--   exit that loop)
loop :: (MonadIRBuilder m, MonadFix m) => m a -> m ()
loop asm = mdo
  br begin
  begin <- block `named` "loop"
  asm
  br begin

-- | Combinator for creating a while loop ("while(condition) { ... }")
--   The first monadic action represents a set of LLVM statements that will
--   compute the result of the condition. The condition operand has to be of
--   type 'i1'. (This is not checked by this function!) The second argument
--   represents a block of LLVM statements that will be inserted inside the
--   body of the loop.
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

-- | Combinator for creating a for loop ("for(int i = ...; ...; ...) { ... }")
--   The first argument represents the variable that "i" is initialized with.
--   The second argument is a function that resembles the condition check of
--   the for loop. It takes as an argument the current value of "i" and it
--   should return an 'Operand' of type 'i1' (this is not checked!). The third
--   argument is a function that takes the current value of "i" as an input and
--   returns the statement(s) that are executed at the end of the loop block
--   (often this is "i++"). The function should return an argument of the same
--   type as the input (again, this is not checked!). The fourth argument
--   represents the set of statements inside the loop, and takes as an input
--   the current value of "i".
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

-- | Calculates the distance between 2 pointers, and converts the result to the
--   'Type' provided.
pointerDiff :: (MonadModuleBuilder m, MonadIRBuilder m)
            => Type -> Operand -> Operand -> m Operand
pointerDiff ty a b = do
  a' <- ptrtoint a i64
  b' <- ptrtoint b i64
  result <- sub a' b'
  trunc result ty

-- | Calculates the logical not of a boolean 'Operand'.
--   NOTE: This assumes the 'Operand' is of type 'i1', this is not checked!
--   Passing in an argument of another width will lead to a crash in LLVM.
not' :: (MonadModuleBuilder m, MonadIRBuilder m)
     => Operand -> m Operand
not' bool = select bool (bit 0) (bit 1)

-- | Helper data type used in the 'minimum'' function.
data Signedness = Signed | Unsigned

-- | Computes the minimum of 2 'Operand's.
--   NOTE: No check is made if the 2 opernads have the same 'Type'!
minimum' :: (MonadModuleBuilder m, MonadIRBuilder m)
         => Signedness -> Operand -> Operand -> m Operand
minimum' sign a b = do
  let inst = case sign of
        Signed -> slt
        Unsigned -> ult
  isLessThan <- inst a b
  select isLessThan a b


-- | A helper data type for building type-safe paths to index into a struct
--   with a "gep" instruction. The start- and end-point of a path are tracked on
--   the type level. For more information about this type, see this
--   <https://luctielen.com/posts/making_llvm_gep_safer_in_haskell/ blogpost>.
newtype Path (a :: k) (b :: k)
  = Path (NonEmpty Operand)
type role Path nominal nominal

-- | Constructs a path from a list of indices. Passing in an empty list is the
--   same as dereferencing the current pointer.
mkPath :: [Operand] -> Path a b
mkPath path = Path (int32 0 :| path)

-- | Operator for composing 2 paths together in a type-safe way. A way to
--   remember this operator is the similarity with the "->" operator in C.
(->>) :: Path a b -> Path b c -> Path a c
Path a2b ->> Path b2c =
  let b2c' = if NE.head b2c == int32 0
               then NE.tail b2c
               else NE.toList b2c
   in Path $ NE.head a2b :| (NE.tail a2b ++ b2c')

-- | Computes an address based on a path and an initial pointer (stored in the
--   'Operand').
addr :: (MonadModuleBuilder m, MonadIRBuilder m)
     => Path a b -> Operand -> m Operand
addr path p = gep p (pathToIndices path)
  where
    pathToIndices :: Path a b -> [Operand]
    pathToIndices (Path indices) =
      NE.toList indices

-- | Loads a value from a memory location based on a 'Path' and an initial
--   pointer (stored in the 'Operand').
deref :: (MonadModuleBuilder m, MonadIRBuilder m)
      => Path a b -> Operand -> m Operand
deref path p = do
  addr <- addr path p
  load addr 0

-- | Stores a value in a memory location based on a 'Path' and an initial pointer
--   (stored in the 'Operand').
assign :: (MonadModuleBuilder m, MonadIRBuilder m)
       => Path a b -> Operand -> Operand -> m ()
assign path p value = do
  dstAddr <- addr path p
  store dstAddr 0 value

-- | Updates a value at some memory location based on a 'Path' and an initial
--   pointer (stored in the 'Operand') by applying a function.
--   Note: The function should return an 'Operand' of the same type as it's
--   input (this is not checked!)
update :: (MonadModuleBuilder m, MonadIRBuilder m)
       => Path a b
       -> Operand
       -> (Operand -> m Operand)
       -> m ()
update path p f = do
  dstAddr <- addr path p
  store dstAddr 0 =<< f =<< load dstAddr 0

-- | Increments a value at some memory location based on a 'Path' and an
--   initial pointer stored in an 'Operand'. The first argument can be a
--   function such as 'int32' to convert a Haskell integer to a LLVM value.
increment :: (MonadModuleBuilder m, MonadIRBuilder m)
          => (Integer -> Operand) -> Path a b -> Operand -> m ()
increment ty path p =
  update path p (add (ty 1))

-- | Copies (a part of) a struct from one place to another.
copy :: (MonadModuleBuilder m, MonadIRBuilder m)
     => Path a b -> Operand -> Operand -> m ()
copy path src dst = do
  value <- deref path src
  assign path dst value

-- | Swaps (a part of) a struct from one place to another.
swap :: (MonadModuleBuilder m, MonadIRBuilder m)
     => Path a b -> Operand -> Operand -> m ()
swap path lhs rhs = do
  tmp <- deref path lhs
  copy path rhs lhs
  assign path rhs tmp

