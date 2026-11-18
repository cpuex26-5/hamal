module Middleend.Closure (Closure (..), Exp (..), Captured (..), FunDef (..), Program (..), closureConvert, freeVars) where

import Basic (Binder (..), Cmp, FunBind (..), PrimOp (..))
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Traversable (mapAccumL)
import Effectful (Eff, (:>))
import Effectful.Output.Static.Local.List (Output, output, runOutput)
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Log (Log, logMsg)
import Middleend.KNormal qualified as K
import Name (Id, Label (Local))
import Pretty qualified as P
import Prettyprinter (Pretty (..), group, hardline, hsep, nest, parens, tupled, vsep, (<+>))
import Type (Type)

data Closure = Closure
  { entry :: Label
  , actualFv :: [Binder Id Type]
  }
  deriving stock (Eq, Show)

instance Pretty Closure where
  pretty Closure {entry, actualFv} = "closure" <+> pretty entry <+> P.braced (pretty . (.name) <$> actualFv)

data Exp
  = Unit
  | Int Int
  | Float Float
  | Var Id
  | PrimOp (PrimOp Id)
  | If Cmp Id Id Exp Exp
  | Let (Binder Id Type) Exp Exp
  | LetTuple [Binder Id Type] Id Exp
  | LetCls (Binder Id Type) Closure Exp
  | AppDir Label [Id]
  | AppCls Id [Id]
  deriving stock (Eq, Show)

instance Pretty Exp where
  pretty = \case
    Unit -> "()"
    Int int -> pretty int
    Float float -> pretty float
    Var ident -> pretty ident
    PrimOp op -> pretty op
    If cmp lhs rhs thn els -> P.ifThenElse (pretty lhs <+> pretty cmp <+> pretty rhs) (pretty thn) (pretty els)
    Let binder rhs body -> P.letIn (pretty binder) (pretty rhs) (pretty body)
    LetTuple binders rhs body -> P.letIn (tupled (pretty <$> binders)) (pretty rhs) (pretty body)
    LetCls binder closure body -> P.letIn (pretty binder) (pretty closure) (pretty body)
    AppDir fun args -> hsep (pretty fun : (pretty <$> args))
    AppCls fun args -> hsep (pretty fun : (pretty <$> args))

data Captured = Captured
  { self :: Id
  , formalFv :: [Binder Id Type]
  }
  deriving stock (Eq, Show)

data FunDef = FunDef
  { binder :: Binder Id Type
  , params :: [Binder Id Type]
  , captured :: Maybe Captured
  , rhs :: Exp
  }
  deriving stock (Eq, Show)

instance Pretty FunDef where
  pretty FunDef {binder, params, captured, rhs} = nest 2 (header <> hardline <> pretty rhs)
    where
      header =
        group . nest 2 $
          vsep
            [ "fundef" <+> pretty binder.name
            , P.braced (pretty <$> maybe [] (.formalFv) captured)
            , hsep (parens . pretty <$> params) <+> "="
            ]

data Program = Program
  { statics :: M.Map Id Int
  , fundefs :: [FunDef]
  , main :: Exp
  }
  deriving stock (Eq, Show)

instance Pretty Program where
  pretty Program {statics, fundefs, main} =
    P.paragraphs $
      [vsep (static <$> sortOn snd (M.toList statics)) | not (M.null statics)]
        <> (pretty <$> fundefs)
        <> [nest 2 ("main =" <> hardline <> pretty main)]
    where
      static (ident, addr) = "static" <+> pretty ident <+> "=" <+> pretty addr

freeVars :: Exp -> S.Set Id
freeVars = \case
  (Unit; Int _; Float _) -> S.empty
  Var ident -> S.singleton ident
  PrimOp op -> foldMap S.singleton op
  If _ lhs rhs thn els -> S.singleton lhs <> S.singleton rhs <> freeVars thn <> freeVars els
  Let binder rhs body -> S.delete binder.name (freeVars rhs <> freeVars body)
  LetTuple binders rhs body -> (S.singleton rhs <> freeVars body) S.\\ S.fromList ((.name) <$> binders)
  LetCls binder Closure {actualFv} body -> S.delete binder.name (S.union (S.fromList ((.name) <$> actualFv)) (freeVars body))
  AppDir _ args -> S.fromList args
  AppCls cls args -> S.fromList $ cls : args

knownFuns :: S.Set Id -> K.Exp -> S.Set Id
knownFuns statics kexp = S.map (.binder) $ stabilize monotonic top
  where
    stabilize f x
      | f x == x = x
      | otherwise = stabilize f (f x)

    top = go kexp
      where
        go = \case
          (K.Unit; K.Int {}; K.Float {}; K.Var {}; K.PrimOp {}; K.App {}) -> S.empty
          K.If _ _ _ thn els -> S.union (go thn) (go els)
          K.Let _ rhs body -> go rhs <> go body
          K.LetTuple _ _ body -> go body
          K.LetRec fundef body -> S.insert (first (.name) fundef) (go body)

    monotonic known = S.filter (\K.FunBind {params, rhs} -> fvK known rhs `S.isSubsetOf` S.fromList params) known
    fvK known expr = go expr S.\\ statics
      where
        isKnown f = f `S.member` S.map (.binder) known
        go = \case
          (K.Unit; K.Int {}; K.Float {}) -> S.empty
          K.Var ident -> S.singleton ident
          K.PrimOp op -> foldMap S.singleton op
          K.If _ lhs rhs thn els -> S.fromList [lhs, rhs] <> go thn <> go els
          K.Let binder rhs body -> go rhs <> (S.delete binder.name (go body))
          K.LetRec FunBind {binder, params, rhs} body ->
            if binder.name `S.member` fvBody
              then S.delete binder.name $ S.union actualFv fvBody
              else fvBody
            where
              fvBody = go body
              actualFv = go rhs S.\\ S.fromList (binder.name : ((.name) <$> params))
          K.LetTuple binders rhs body -> S.singleton rhs <> go body S.\\ S.fromList ((.name) <$> binders)
          K.App fun args | isKnown fun -> S.fromList args
          K.App fun args -> S.fromList $ fun : args

staticAlloc :: K.Exp -> M.Map Id Int
staticAlloc = M.fromList . snd . mapAccumL (\offset (ident, size) -> (offset + size, (ident, offset))) 0 . go M.empty
  where
    go consts = \case
      (K.Unit; K.Int {}; K.Float {}; K.Var {}; K.PrimOp {}; K.If {}; K.App {}) -> []
      K.Let binder (K.Int n) body -> go (M.insert binder.name n consts) body
      K.Let binder (K.PrimOp (Array size _)) body | Just n <- M.lookup size consts -> (binder.name, n * 4) : go consts body
      K.Let binder (K.PrimOp (Tuple idents)) body -> (binder.name, length idents * 4) : go consts body
      K.Let _ _ body -> go consts body
      K.LetRec _ body -> go consts body
      K.LetTuple _ _ body -> go consts body

type Env = M.Map Id Type

closureConvert :: (Log :> es) => K.Exp -> Eff es Program
closureConvert kexp = do
  (cexp, fundefs) <- runOutput $ runReader @Env M.empty $ go kexp
  pure $ Program statics fundefs cexp
  where
    statics = staticAlloc kexp
    known = knownFuns (M.keysSet statics) kexp
    go :: (Output FunDef :> es, Reader (M.Map Id Type) :> es, Log :> es) => K.Exp -> Eff es Exp
    go = \case
      K.Unit -> pure Unit
      K.Int int -> pure $ Int int
      K.Float float -> pure $ Float float
      K.Var ident -> pure $ Var ident
      K.PrimOp op -> pure $ PrimOp op
      K.If cmp lhs rhs thn els -> If cmp lhs rhs <$> go thn <*> go els
      K.Let binder rhs body -> Let binder <$> go rhs <*> local (with binder) (go body)
      K.LetRec FunBind {binder, params, rhs} body -> do
        rhs' <- local (foldMap with (binder : params)) (go rhs)
        body' <- local (with binder) (go body)
        let fv = S.toList $ freeVars rhs' S.\\ M.keysSet statics S.\\ S.fromList ((.name) <$> binder : params)
        typedFv <- asks (\env -> (\name -> Binder {name, ty = M.findWithDefault (error $ "closure(unreachable): " <> show name) name env}) <$> fv)
        if binder.name `S.member` freeVars body'
          then do
            logMsg $ "making closure: " <> show binder
            output $ FunDef {binder, params, captured = Just (Captured binder.name typedFv), rhs = rhs'}
            pure $ LetCls binder Closure {entry = Local binder.name, actualFv = typedFv} body'
          else do
            logMsg $ "eliminating closure: " <> show binder
            output $ FunDef {binder, params, captured = Nothing, rhs = rhs'}
            pure body'
      K.LetTuple binders rhs body -> LetTuple binders rhs <$> local (foldMap with binders) (go body)
      K.App fun args | fun `S.member` known -> logMsg ("directly applying: " <> show (K.App fun args)) $> AppDir (Local fun) args
      K.App fun args -> pure $ AppCls fun args
    with binder = M.insert binder.name binder.ty
