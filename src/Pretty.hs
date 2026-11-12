module Pretty (letIn, ifThenElse, braced, insn, mem, call, paragraphs) where

import Prettyprinter (Doc, Pretty, align, comma, concatWith, encloseSep, flatAlt, group, hardline, hsep, line, nest, parens, pretty, punctuate, surround, tupled, (<+>))

letIn :: Doc ann -> Doc ann -> Doc ann -> Doc ann
letIn lhs rhs body =
  group (nest 2 ("let" <+> lhs <+> "=" <> line <> rhs) <> line <> "in") <> hardline <> body

ifThenElse :: Doc ann -> Doc ann -> Doc ann -> Doc ann
ifThenElse cond thn els =
  group $ nest 2 ("if" <+> cond <+> "then" <> line <> thn) <> line <> nest 2 ("else" <> line <> els)

braced :: [Doc ann] -> Doc ann
braced = align . group . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "

insn :: Doc ann -> [Doc ann] -> Doc ann
insn op operands = op <+> hsep (punctuate comma operands)

mem :: (Pretty a) => Int -> a -> Doc ann
mem offset base = pretty offset <> parens (pretty base)

call :: (Pretty x, Pretty f) => Doc ann -> Doc ann -> [x] -> [f] -> Doc ann
call op callee xargs fargs = op <+> callee <+> tupled (pretty <$> xargs) <+> tupled (pretty <$> fargs)

paragraphs :: [Doc ann] -> Doc ann
paragraphs = concatWith (surround (hardline <> hardline))
