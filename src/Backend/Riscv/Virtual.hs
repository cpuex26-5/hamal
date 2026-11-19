module Backend.Riscv.Virtual (virtualify) where

import Backend.Riscv.Asm (Block (..), FunDef (..), Inst (..), Operand (..), Program (..), SingClass (..), SomeClass (..))
import Basic (Binder (..), Cmp (..), Rel (..))
import Basic qualified as B
import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask, asks, local, runReader)
import GHC.Float (castFloatToWord32)
import Middleend.Closure (Captured (..), freeVars)
import Middleend.Closure qualified as C
import Name (Fresh, Id, Label (..), fresh)
import Type qualified as T

type Env = M.Map Id SomeClass

type Statics = M.Map Id Int

classOf :: T.Type -> SomeClass
classOf (T.Type ty) = case ty of
  T.Unit -> SomeClass SVoid
  T.Float -> SomeClass SFPR
  _ -> SomeClass SGPR

operand :: SingClass c -> Id -> Maybe (Operand c Id Id)
operand c ident = case c of
  SGPR -> Just (OpX ident)
  SFPR -> Just (OpF ident)
  SVoid -> Nothing

partitionByClass :: [(Id, SomeClass)] -> ([Id], [Id])
partitionByClass = foldr step ([], [])
  where
    step (n, SomeClass SGPR) (xs, fs) = (n : xs, fs)
    step (n, SomeClass SFPR) (xs, fs) = (xs, n : fs)
    step (_, SomeClass SVoid) (xs, fs) = (xs, fs)

insertLet :: Maybe (Operand c' x f) -> Block c' x f -> Block c x f -> Block c x f
insertLet dst (Ans inst) k = Let dst inst k
insertLet dst (Let d i rest) k = Let d i (insertLet dst rest k)

classOfVar :: (Reader Env :> es) => Id -> Eff es SomeClass
classOfVar = asks . M.findWithDefault (error "unreachable")

fill :: (Fresh :> es, Reader Env :> es) => Id -> Id -> Id -> Eff es (Block c Id Id -> Block c Id Id)
fill arr size initial =
  classOfVar initial >>= \case
    SomeClass SVoid -> pure id
    SomeClass SGPR -> pure $ call initial
    SomeClass SFPR -> do
      bits <- fresh "bits"
      pure $ Let (Just (OpX bits)) (BitsOfFloat initial) . call bits
  where
    call val = Let Nothing (CallDir (Global "hamal_fill") [arr, size, val] [])

newtype Adhoc = Adhoc (M.Map Id Int)

vBlock :: (Fresh :> es, Reader Env :> es, Reader Statics :> es, Reader Adhoc :> es) => SingClass c -> C.Exp -> Eff es (Block c Id Id)
vBlock c expr = do
  statics <- ask @Statics
  case expr of
    C.Unit | SVoid <- c -> pure $ Ans Nop
    C.Int int | SGPR <- c -> pure $ Ans (Li int)
    C.Float float | SFPR <- c -> do
      bits <- fresh "bits"
      pure
        $ Let (Just (OpX bits)) (Li (fromIntegral (castFloatToWord32 float)))
        $ Ans (FloatOfBits bits)
    C.Var ident | SGPR <- c -> pure . Ans . Mv . OpX $ ident
    C.Var ident | SFPR <- c -> pure . Ans . Mv . OpF $ ident
    C.PrimOp (B.Floor f) | SFPR <- c -> do
      rounded <- fresh "rounded"
      roundedF <- fresh "roundedF"
      set <- fresh "set"
      zero <- fresh "zero"
      dec <- fresh "dec"
      pure
        $ Let (Just (OpX rounded)) (IntOfFloat f)
        $ Let (Just (OpF roundedF)) (FloatOfInt rounded)
        $ Let (Just (OpX set)) (Fle roundedF f)
        $ Let (Just (OpX zero)) (Li 0)
        $ Ans (If Neq set zero (Ans (Mv (OpF roundedF))) (Let (Just (OpX dec)) (Addi rounded (-1)) (Ans (FloatOfInt dec))))
    C.PrimOp (B.Tuple binders) | SGPR <- c -> do
      tuple <- fresh "tuple"
      classes <- traverse (asks @Env . M.findWithDefault (error "unreachable")) binders
      let store (x, SomeClass c', offset) b = maybe b (\op -> Let Nothing (Store op tuple offset) b) (operand c' x)
      pure
        $ Let (Just (OpX tuple)) (Alloc (4 * length binders))
        $ foldr store (Ans (Mv (OpX tuple))) (zip3 binders classes [0, 4 ..])
    C.PrimOp (B.Array size initial) | SGPR <- c -> do
      arr <- fresh "array"
      bytes <- fresh "bytes"
      withFill <- fill arr size initial
      asks (\(Adhoc env) -> M.lookup size env) <&> \case
        Nothing ->
          Let (Just (OpX bytes)) (Slli size 2)
            $ Let (Just (OpX arr)) (AllocDyn bytes)
            $ withFill (Ans (Mv (OpX arr)))
        Just n ->
          Let (Just (OpX arr)) (Alloc (n * 4)) $
            withFill (Ans (Mv (OpX arr)))
    C.PrimOp (B.Get {}) | SVoid <- c -> pure $ Ans Nop
    C.PrimOp (B.Get arr idx) -> do
      offsetM <- asks @Statics (M.lookup arr)
      addr <- fresh "addr"
      addr' <- fresh "addr"
      asks (\(Adhoc env) -> M.lookup idx env) <&> \case
        Nothing ->
          Let (Just (OpX addr)) (Slli idx 2)
            $ Let (Just (OpX addr')) (maybe (Add addr arr) (Addi addr) offsetM)
            $ Ans (case c of SGPR -> Lw 0 addr'; SFPR -> FLw 0 addr')
        Just n ->
          Ans (case c of SGPR -> Lw (n * 4) arr; SFPR -> FLw (n * 4) arr)
    C.PrimOp (B.Put arr idx rhs) | SVoid <- c -> do
      offsetM <- asks @Statics (M.lookup arr)
      addr <- fresh "addr"
      addr' <- fresh "addr"
      SomeClass crhs <- classOfVar rhs
      idxM <- asks (\(Adhoc env) -> M.lookup idx env)
      pure $ case operand crhs rhs of
        Just op ->
          case idxM of
            Nothing ->
              Let (Just (OpX addr)) (Slli idx 2)
                $ Let (Just (OpX addr')) (maybe (Add addr arr) (Addi addr) offsetM)
                $ Ans (Store op addr' 0)
            Just n -> Ans (Store op arr (4 * n))
        Nothing -> Ans Nop
    C.PrimOp (B.In {}) | SGPR <- c -> do
      base <- fresh "base"
      pure $ Let (Just (OpX base)) (Li mmioBase) (Ans (In base))
    C.PrimOp (B.Out ident) | SVoid <- c -> do
      base <- fresh "base"
      pure $ Let (Just (OpX base)) (Li mmioBase) (Ans (Out base ident))
    C.PrimOp op -> Ans <$> vPrimOp c op
    -- FIXME: adhoc addressing mode folding
    C.Let binder rhs@(C.Int n) body | SomeClass SGPR <- classOf binder.ty -> do
      rhs' <- vBlock SGPR rhs
      body' <-
        local (\(Adhoc env) -> Adhoc (M.insert binder.name n env)) $
          local (M.insert binder.name (SomeClass SGPR)) (vBlock c body)
      pure $ insertLet (operand SGPR binder.name) rhs' body'
    C.Let binder (C.PrimOp (B.Tuple idents)) body | Just base <- M.lookup binder.name statics -> do
      classes <- traverse classOfVar idents
      body' <- local @Env (M.insert binder.name (SomeClass SGPR)) (vBlock c body)
      let store (ident, SomeClass c', offset) k = maybe k (\op -> Let Nothing (Store op binder.name offset) k) (operand c' ident)
      pure
        $ Let (Just (OpX binder.name)) (Li base)
        $ foldr store body' (zip3 idents classes [0, 4 ..])
    C.Let binder (C.PrimOp (B.Array size initial)) body | Just base <- M.lookup binder.name statics -> do
      body' <- local @Env (M.insert binder.name (SomeClass SGPR)) (vBlock c body)
      withFill <- fill binder.name size initial
      pure $ Let (Just (OpX binder.name)) (Li base) (withFill body')
    C.Let binder rhs body | SomeClass c' <- classOf binder.ty -> do
      rhs' <- vBlock c' rhs
      body' <- local (M.insert binder.name (SomeClass c')) (vBlock c body)
      pure $ insertLet (operand c' binder.name) rhs' body'
    C.LetTuple binders rhs body -> do
      body' <- local @Env (M.union (M.fromList [(b.name, classOf b.ty) | b <- binders])) (vBlock c body)
      pure $ foldr (withLoad rhs) body' (zip binders [0, 4 ..])
      where
        withLoad :: forall c. Id -> (Binder Id T.Type, Int) -> Block c Id Id -> Block c Id Id
        withLoad base (Binder {name, ty}, offset) k = case classOf ty of
          SomeClass SGPR -> Let (operand SGPR name) (Lw offset base) k
          SomeClass SFPR -> Let (operand SFPR name) (FLw offset base) k
          SomeClass SVoid -> k
    C.LetCls binder C.Closure {entry, actualFv} body | SomeClass c' <- classOf binder.ty -> do
      body' <- local (M.insert binder.name (SomeClass c')) (vBlock c body)
      closure <- fresh "code"
      pure
        $ Let (operand SGPR binder.name) (Alloc (4 * length actualFv + 4))
        $ Let (operand SGPR closure) (La entry)
        $ Let Nothing (Store (OpX closure) binder.name 0)
        $ foldr (withStore binder.name) body' (zip actualFv [4, 8 ..])
      where
        withStore :: forall c. Id -> (Binder Id T.Type, Int) -> Block c Id Id -> Block c Id Id
        withStore base (Binder {name, ty}, offset) k = case classOf ty of
          SomeClass SGPR -> Let Nothing (Store (OpX name) base offset) k
          SomeClass SFPR -> Let Nothing (Store (OpF name) base offset) k
          SomeClass SVoid -> k
    C.If (ICmp rel) lhs rhs thn els -> Ans <$> (If rel lhs rhs <$> vBlock c thn <*> vBlock c els)
    C.If (FCmp rel) lhs rhs thn els -> do
      set <- fresh "set"
      zero <- fresh "zero"
      let (cmp, branch) = case rel of
            Eq -> (Feq lhs rhs, Neq)
            Neq -> (Feq lhs rhs, Eq)
            Lt -> (Flt lhs rhs, Neq)
            Le -> (Fle lhs rhs, Neq)
            Gt -> (Flt rhs lhs, Neq)
            Ge -> (Fle rhs lhs, Neq)
      thn' <- vBlock c thn
      els' <- vBlock c els
      pure
        $ Let (Just (OpX set)) cmp
        $ Let (Just (OpX zero)) (Li 0)
        $ Ans (If branch set zero thn' els')
    C.AppDir label args -> do
      cargs <- traverse (\n -> (n,) <$> classOfVar n) args
      let (xargs, fargs) = partitionByClass cargs
      pure $ Ans $ CallDir label xargs fargs
    C.AppCls closure args -> do
      cargs <- traverse (\n -> (n,) <$> classOfVar n) args
      let (xargs, fargs) = partitionByClass cargs
      code <- fresh "code"
      pure
        $ Let (Just (OpX code)) (Lw 0 closure)
        $ Ans (CallInd code (xargs <> [closure]) fargs)
    e -> error $ "vInst(unreachable): " <> show (c, e)
  where
    mmioBase = 0xffff0000

vPrimOp :: (Reader Env :> es) => SingClass c -> B.PrimOp Id -> Eff es (Inst c Id Id)
vPrimOp c op = case (c, op) of
  (SGPR, B.Not ident) -> pure $ Xori ident 1
  (SGPR, B.Xor lhs rhs) -> pure $ Xor lhs rhs
  (SGPR, B.Neg ident) -> pure $ Neg ident
  (SFPR, B.FNeg ident) -> pure $ FNeg ident
  (SGPR, B.Add lhs rhs) -> pure $ Add lhs rhs
  (SGPR, B.Sub lhs rhs) -> pure $ Sub lhs rhs
  (SGPR, B.Slli ident shamt) -> pure $ Slli ident shamt
  (SGPR, B.Srai ident shamt) -> pure $ Srai ident shamt
  (SFPR, B.FAdd lhs rhs) -> pure $ FAdd lhs rhs
  (SFPR, B.FSub lhs rhs) -> pure $ FSub lhs rhs
  (SFPR, B.FMul lhs rhs) -> pure $ FMul lhs rhs
  (SFPR, B.FDiv lhs rhs) -> pure $ FDiv lhs rhs
  (SFPR, B.FSqrt ident) -> pure $ FSqrt ident
  (SFPR, B.FAbs ident) -> pure $ FAbs ident
  (SFPR, B.FloatOfInt int) -> pure $ FloatOfInt int
  (SGPR, B.IntOfFloat float) -> pure $ IntOfFloat float
  (_, B.Coerce ident) ->
    classOfVar ident <&> \(SomeClass src) -> case (src, c) of
      (SGPR, SGPR) -> Mv (OpX ident)
      (SFPR, SFPR) -> Mv (OpF ident)
      (SGPR, SFPR) -> FloatOfBits ident
      (SFPR, SGPR) -> BitsOfFloat ident
      (_, SVoid) -> Nop
      (SVoid, _) -> error "coerce: unit has no bits"
  _ -> error $ "virtualOp(unreachable): " <> show (c, op)

vFunc :: (Fresh :> es, Reader Statics :> es, Reader Adhoc :> es) => C.FunDef -> Eff es (FunDef Id Id)
vFunc C.FunDef {binder, params, captured, rhs} = case binder.ty of
  T.Type (T.Fun _ ret) | SomeClass cret <- classOf (T.Type ret) -> do
    statics <- M.toList <$> asks @Statics (`M.restrictKeys` freeVars rhs)
    let formalFv = maybe [] (.formalFv) captured
        env = M.fromList $ [(b.name, classOf b.ty) | b <- params <> formalFv] <> [(static, SomeClass SGPR) | (static, _) <- statics]
    rhs' <- runReader env (vBlock cret rhs)
    let (xparams, fparams) = partitionByClass [(b.name, classOf b.ty) | b <- params]
        rhs'' = foldr (withLoad binder.name) rhs' (zip formalFv [4, 8 ..]) -- load formalFv
        rhs''' = foldr (\(static, addr) k -> Let (Just (OpX static)) (Li addr) k) rhs'' statics -- load statics
    pure $ FunDef {binder = Local binder.name, xparams = xparams <> maybe [] (pure . (.self)) captured, fparams, rhs = rhs''', ret = cret}
    where
      withLoad :: forall c. Id -> (Binder Id T.Type, Int) -> Block c Id Id -> Block c Id Id
      withLoad base (Binder {name, ty}, offset) k = case classOf ty of
        SomeClass SGPR -> Let (operand SGPR name) (Lw offset base) k
        SomeClass SFPR -> Let (operand SFPR name) (FLw offset base) k
        SomeClass SVoid -> k
  _ -> error "unreachable(vFunc): rhs of FunDef is not a function."

virtualify :: (Fresh :> es) => C.Program -> Eff es (Program Id Id)
virtualify (C.Program statics heapBase fundefs expr) = do
  (fundefs', main) <-
    runReader @Env M.empty
      $ runReader @Statics statics
      $ runReader @Adhoc (Adhoc M.empty)
      $ (,) <$> traverse vFunc fundefs <*> vBlock SVoid expr
  pure $ Program heapBase fundefs' main
