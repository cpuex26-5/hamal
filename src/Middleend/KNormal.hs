module Middleend.KNormal (Exp (..), Binder (..), FunBind (..), kNormal) where

import Basic (Binder (..), Cmp (..), FunBind (..), PrimOp (..), Rel (..))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Bool (bool)
import Data.Map.Strict qualified as M
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Frontend.Syntax qualified as S
import Name (Fresh, Id, Symbol, fresh)
import Prettyprinter (Pretty (..), group, hardline, hsep, line, nest, parens, tupled, (<+>))
import Type (Type)
import Type qualified as T

data Exp
  = Unit
  | Int Int
  | Float Float
  | Var Id
  | PrimOp (PrimOp Id)
  | If Cmp Id Id Exp Exp
  | Let (Binder Id Type) Exp Exp
  | LetRec (FunBind (Binder Id Type) Exp) Exp
  | LetTuple [Binder Id Type] Id Exp
  | App Id [Id]
  deriving stock (Show, Eq)

instance Pretty Exp where
  pretty = \case
    Unit -> "()"
    Int int -> pretty int
    Float float -> pretty float
    Var ident -> pretty ident
    PrimOp op -> pretty op
    If cmp lhs rhs thn els ->
      group $
        nest 2 ("if" <+> pretty lhs <+> pretty cmp <+> pretty rhs <+> "then" <+> line <> pretty thn)
          <> line
          <> nest 2 ("else" <> line <> pretty els)
    Let binder rhs body ->
      group (nest 2 ("let" <+> pretty binder <+> "=" <> line <> pretty rhs) <> line <> "in")
        <> hardline
        <> pretty body
    LetRec FunBind {binder, args, rhs} body ->
      nest 2 ("let rec" <+> pretty binder.name <+> hsep (parens . pretty <$> args) <+> "=" <> line <> pretty rhs) <> hardline <> "in" <> hardline <> pretty body
    LetTuple binders rhs body -> "let" <+> tupled (pretty <$> binders) <+> "=" <+> pretty rhs <+> "in" <> hardline <> pretty body
    App fun params -> hsep (pretty fun : (pretty <$> params))

insertLet :: (Fresh :> es) => Type -> Eff es Exp -> (Id -> Eff es Exp) -> Eff es Exp
insertLet ty scrutM k = do
  scrut <- scrutM
  case scrut of
    Var x -> k x
    _ -> do
      name <- fresh "T"
      body <- k name
      pure $ Let Binder {name, ty} scrut body

kNormal :: (Fresh :> es) => S.Typed -> Eff es Exp
kNormal = runReader (M.empty :: M.Map Symbol Id) . go

go :: (Fresh :> es, Reader (M.Map Symbol Id) :> es) => S.Typed -> Eff es Exp
go expr@(_ :< t) = evalContT $ case t of
  S.Unit -> pure Unit
  S.Int int -> pure $ Int int
  S.Float float -> pure $ Float float
  S.Bool bol -> pure . Int $ bool 0 1 bol
  S.Cmp {} -> kIf expr (pure (Int 1)) (pure (Int 0))
  S.Var sym -> Var <$> lift (asks (M.findWithDefault (error "kNormal(unreachable)") sym))
  S.PrimOp op -> PrimOp <$> traverse atomize op
  S.If cnd thn els -> kIf cnd (go thn) (go els)
  S.Let binder rhs body -> do
    rhs' <- lift (go rhs)
    binder' <- bind binder
    Let binder' rhs' <$> lift (go body)
  S.LetRec FunBind {binder, args, rhs} body -> do
    binder' <- bind binder
    (args', rhs') <- lift . evalContT $ do
      args' <- traverse bind args
      (args',) <$> lift (go rhs)
    LetRec FunBind {binder = binder', args = args', rhs = rhs'} <$> lift (go body)
  S.App fun params -> App <$> atomize fun <*> traverse atomize params
  S.LetTuple binders rhs body -> do
    rhs' <- atomize rhs
    binders' <- traverse bind binders
    LetTuple binders' rhs' <$> lift (go body)
  where
    atom ty = ContT . insertLet ty
    atomize e = atom (snd (extract e)) (go e)
    bind b = ContT \k -> do
      b' <- bitraverse fresh pure b
      local (M.insert b.name b'.name) (k b')
    kIf (_ :< S.Cmp cmp lhs rhs) thn els = If cmp <$> atomize lhs <*> atomize rhs <*> lift thn <*> lift els
    kIf cnd thn els = If (ICmp Eq) <$> atomize cnd <*> atom (T.Type T.Int) (pure (Int 1)) <*> lift thn <*> lift els
