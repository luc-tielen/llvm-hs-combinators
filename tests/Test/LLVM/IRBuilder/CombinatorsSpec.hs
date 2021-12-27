{-# LANGUAGE RecursiveDo, OverloadedStrings, OverloadedLists, QuasiQuotes #-}

module Test.LLVM.IRBuilder.CombinatorsSpec
  ( module Test.LLVM.IRBuilder.CombinatorsSpec
  ) where

import Test.Hspec
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Combinators
import LLVM.AST.Type
import LLVM.Pretty
import NeatInterpolation
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T


(==>) :: IRBuilderT ModuleBuilder () -> T.Text -> IO ()
llvmIR ==> txt =
  let ir = function "test" [] i32 $ const llvmIR
      llvmText = TL.toStrict (ppllvm $ buildModule "<module>" ir)
      strip = T.stripEnd . T.unlines . map T.stripEnd . T.lines
   in strip llvmText `shouldBe` [text|
      ; ModuleID = '<module>'





      define external ccc  i32 @test()    {
      $txt
      }
      |]


spec :: Spec
spec = describe "llvm-hs combinators" $ parallel $ do
  it "supports one-side if statements" $ do
    let ir = do
          if' (bit 1) $ do
            ret (int32 42)

          ret (int32 1000)
    ir ==> [text|
      ; <label>:0:
        br i1 1, label %if_0, label %end_if_0
      if_0:
        br label %end_if_0
      end_if_0:
        ret i32 1000
      |]

  it "supports infinite loops" $ do
    let ir = mdo
          i <- allocate i32 (int32 0)

          loop $ do
            iValue <- load i 0
            isEqual <- iValue `eq` int32 10
            if' isEqual $ do
              br end

          end <- block `named` "end"
          ret $ int32 42
    ir ==> [text|
      ; <label>:0:
        %1 = alloca i32, i32 1
        store   i32 0, i32* %1
        br label %loop_0
      loop_0:
        %2 = load   i32, i32* %1
        %3 = icmp eq i32 %2, 10
        br i1 %3, label %if_0, label %end_if_0
      if_0:
        br label %end_if_0
      end_if_0:
        br label %loop_0
      end_0:
        ret i32 42
      |]

  it "supports while loops" $ do
    let ir = mdo
          i <- allocate i32 (int32 10)
          let notZero = do
                iVal <- load i 0
                iVal `ne` int32 0
          loopWhile notZero $ do
            iVal <- load i 0
            iVal' <- sub iVal (int32 1)
            store i 0 iVal'

          ret $ int32 42
    ir ==> [text|
      ; <label>:0:
        %1 = alloca i32, i32 1
        store   i32 10, i32* %1
        br label %while_begin_0
      while_begin_0:
        %2 = load   i32, i32* %1
        %3 = icmp ne i32 %2, 0
        br i1 %3, label %while_body_0, label %while_end_0
      while_body_0:
        %4 = load   i32, i32* %1
        %5 = sub   i32 %4, 1
        store   i32 %5, i32* %1
        br label %while_begin_0
      while_end_0:
        ret i32 42
      |]

  it "supports for loops" $ do
    let ir = mdo
          x <- allocate i32 (int32 10)

          loopFor (int32 0) (`ult` int32 10) (add (int32 1)) $ \i -> do
            xVal <- load x 0
            xVal' <- add i xVal
            store x 0 xVal'

          ret $ int32 42
    ir ==> [text|
      ; <label>:0:
        %1 = alloca i32, i32 1
        store   i32 10, i32* %1
        br label %for_begin_0
      for_begin_0:
        %2 = phi i32 [0, %0], [%6, %for_body_0]
        %3 = icmp ult i32 %2, 10
        br i1 %3, label %for_body_0, label %for_end_0
      for_body_0:
        %4 = load   i32, i32* %1
        %5 = add   i32 %2, %4
        store   i32 %5, i32* %1
        %6 = add   i32 1, %2
        br label %for_begin_0
      for_end_0:
        ret i32 42
      |]

  describe "comparisons" $ parallel $ do
    let check f op = do
          let ir = mdo
                _isEqual <- f (int32 42) (int32 10)
                ret $ int32 42
          ir ==> T.stripEnd (T.unlines
              [ "  %1 = icmp " <> op <> " i32 42, 10"
              , "  ret i32 42"
              ])

    it "supports == comparison" $ do
      check eq "eq"

    it "supports != comparison" $ do
      check ne "ne"

    it "supports (signed) >= comparison" $ do
      check sge "sge"

    it "supports (signed) > comparison" $ do
      check sgt "sgt"

    it "supports (signed) <= comparison" $ do
      check sle "sle"

    it "supports (signed) < comparison" $ do
      check slt "slt"

    it "supports (unsigned) >= comparison" $ do
      check uge "uge"

    it "supports (unsigned) > comparison" $ do
      check ugt "ugt"

    it "supports (unsigned) <= comparison" $ do
      check ule "ule"

    it "supports (unsigned) < comparison" $ do
      check ult "ult"

  it "supports pointer subtraction" $ do
    let ir = mdo
          _entry <- block `named` "entry"
          array <- alloca i32 (Just $ int32 5) 0
          ptr1 <- gep array [int32 0]
          ptr2 <- gep array [int32 3]
          _true <- pointerDiff i32 ptr1 ptr2
          ret $ int32 42
    ir ==> [text|
      entry_0:
        %0 = alloca i32, i32 5
        %1 = getelementptr  i32, i32* %0, i32 0
        %2 = getelementptr  i32, i32* %0, i32 3
        %3 = ptrtoint i32* %1 to i64
        %4 = ptrtoint i32* %2 to i64
        %5 = sub   i64 %3, %4
        %6 = trunc i64 %5 to i32
        ret i32 42
      |]

  it "supports computing the minimum of 2 values" $ do
    let ir = mdo
          _entry <- block `named` "entry"
          _result1 <- minimum' Signed (int32 100) (int32 42)
          _result2 <- minimum' Unsigned (int32 100) (int32 42)
          ret $ int32 42
    ir ==> [text|
      entry_0:
        %0 = icmp slt i32 100, 42
        %1 = select i1 %0, i32 100, i32 42
        %2 = icmp ult i32 100, 42
        %3 = select i1 %2, i32 100, i32 42
        ret i32 42
      |]

  it "supports logical not" $ do
    let ir = mdo
          _entry <- block `named` "entry"
          _true <- not' $ bit 0
          ret $ int32 42
    ir ==> [text|
      entry_0:
        %0 = select i1 0, i1 0, i1 1
        ret i32 42
      |]

  it "supports allocating and initializing a variable on the stack" $ do
    let ir = mdo
          _entry <- block `named` "entry"
          _i <- allocate i32 (int32 0)
          ret $ int32 42
    ir ==> [text|
      entry_0:
        %0 = alloca i32, i32 1
        store   i32 0, i32* %0
        ret i32 42
      |]


  describe "GEP combinators" $ parallel $ do
    it "supports composing Paths" $ do
      let path = mkPath [int32 1, int32 2] ->> mkPath [int32 3]
      path `shouldBe` Path [int32 0, int32 1, int32 2, int32 3]

    it "supports computing the address based on a Path" $ do
      let path = Path [int32 5]
          ir = mdo
            _entry <- block `named` "entry"
            array <- alloca i32 (Just $ int32 5) 0
            _address <- addr path array
            ret $ int32 42
      ir ==> [text|
        entry_0:
          %0 = alloca i32, i32 5
          %1 = getelementptr  i32, i32* %0, i32 5
          ret i32 42
        |]

    it "supports dereferencing an address based on a Path" $ do
      let path = Path [int32 5]
          ir = mdo
            _entry <- block `named` "entry"
            array <- alloca i32 (Just $ int32 5) 0
            _value <- deref path array
            ret $ int32 42
      ir ==> [text|
        entry_0:
          %0 = alloca i32, i32 5
          %1 = getelementptr  i32, i32* %0, i32 5
          %2 = load   i32, i32* %1
          ret i32 42
        |]

    it "supports storing a value at an address based on a Path" $ do
      let path = Path [int32 5]
          ir = mdo
            _entry <- block `named` "entry"
            array <- alloca i32 (Just $ int32 5) 0
            assign path array (int32 1000)
            ret $ int32 42
      ir ==> [text|
        entry_0:
          %0 = alloca i32, i32 5
          %1 = getelementptr  i32, i32* %0, i32 5
          store   i32 1000, i32* %1
          ret i32 42
        |]

    it "supports updating a value at an address based on a Path" $ do
      let path = Path [int32 5]
          ir = mdo
            _entry <- block `named` "entry"
            array <- alloca i32 (Just $ int32 5) 0
            assign path array (int32 1000)
            update path array (add (int32 10))
            ret $ int32 42
      ir ==> [text|
        entry_0:
          %0 = alloca i32, i32 5
          %1 = getelementptr  i32, i32* %0, i32 5
          store   i32 1000, i32* %1
          %2 = getelementptr  i32, i32* %0, i32 5
          %3 = load   i32, i32* %2
          %4 = add   i32 10, %3
          store   i32 %4, i32* %2
          ret i32 42
        |]

    it "supports incrementing a value at an address based on a Path" $ do
      let path = Path [int32 5]
          ir = mdo
            _entry <- block `named` "entry"
            array <- alloca i32 (Just $ int32 5) 0
            assign path array (int32 1000)
            increment int32 path array
            ret $ int32 42
      ir ==> [text|
        entry_0:
          %0 = alloca i32, i32 5
          %1 = getelementptr  i32, i32* %0, i32 5
          store   i32 1000, i32* %1
          %2 = getelementptr  i32, i32* %0, i32 5
          %3 = load   i32, i32* %2
          %4 = add   i32 1, %3
          store   i32 %4, i32* %2
          ret i32 42
        |]

    it "supports copying (part of) a type based on a Path" $ do
      let path = Path [int32 5]
          ir = mdo
            _entry <- block `named` "entry"
            array <- alloca i32 (Just $ int32 5) 0
            assign path array (int32 1000)
            array2 <- alloca i32 (Just $ int32 5) 0
            copy path array array2
            ret $ int32 42
      ir ==> [text|
        entry_0:
          %0 = alloca i32, i32 5
          %1 = getelementptr  i32, i32* %0, i32 5
          store   i32 1000, i32* %1
          %2 = alloca i32, i32 5
          %3 = getelementptr  i32, i32* %0, i32 5
          %4 = load   i32, i32* %3
          %5 = getelementptr  i32, i32* %2, i32 5
          store   i32 %4, i32* %5
          ret i32 42
        |]

    it "supports swapping (part of) a type based on a Path" $ do
      let path = Path [int32 5]
          ir = mdo
            _entry <- block `named` "entry"
            array <- alloca i32 (Just $ int32 5) 0
            assign path array (int32 1000)
            array2 <- alloca i32 (Just $ int32 5) 0
            swap path array array2
            ret $ int32 42
      ir ==> [text|
        entry_0:
          %0 = alloca i32, i32 5
          %1 = getelementptr  i32, i32* %0, i32 5
          store   i32 1000, i32* %1
          %2 = alloca i32, i32 5
          %3 = getelementptr  i32, i32* %0, i32 5
          %4 = load   i32, i32* %3
          %5 = getelementptr  i32, i32* %2, i32 5
          %6 = load   i32, i32* %5
          %7 = getelementptr  i32, i32* %0, i32 5
          store   i32 %6, i32* %7
          %8 = getelementptr  i32, i32* %2, i32 5
          store   i32 %4, i32* %8
          ret i32 42
        |]

