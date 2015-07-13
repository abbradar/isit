{-# LANGUAGE TupleSections, ViewPatterns #-}

module Language.Haskell.IsIt (is) where

import Control.Monad.Trans
import Control.Monad.Writer
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Exts.SrcLoc (SrcLoc(..))
import Language.Haskell.Exts.Parser (parse, ParseResult(..))
import qualified Language.Haskell.Exts.Syntax as H
import Language.Haskell.Meta.Syntax.Translate (toPat)

lookupValueName' :: String -> Q Name
lookupValueName' n = do
  x <- lookupValueName n
  case x of
   Nothing -> fail $ "cannot find (" ++ n ++ ") in scope"
   Just r -> return r

convEq :: Name -> WriterT [(Name, Name)] Q Name
convEq (nameBase -> n) = do
  var <- lift $ lookupValueName' n
  tmp <- lift $ newName n
  tell [(var, tmp)]
  return tmp

convPat :: Pat -> WriterT [(Name, Name)] Q Pat
convPat (LitP l) = pure $ LitP l
convPat (VarP n) = VarP <$> convEq n
convPat (TupP ps) = TupP <$> mapM convPat ps
convPat (UnboxedTupP ps) = UnboxedTupP <$> mapM convPat ps
convPat (ConP name ps) = ConP name <$> mapM convPat ps
convPat (InfixP p1 name p2) = InfixP <$> convPat p1 <*> pure name <*> convPat p2
convPat (UInfixP p1 name p2) = UInfixP <$> convPat p1 <*> pure name <*> convPat p2
convPat (ParensP p) = ParensP <$> convPat p
convPat (TildeP p) = TildeP <$> convPat p
convPat (BangP p) = BangP <$> convPat p
convPat (AsP n p) = AsP <$> convEq n <*> convPat p
convPat (WildP) = pure WildP
convPat (RecP name fps) = RecP name <$> mapM (\(n, p) -> (n, ) <$> convPat p) fps
convPat (ListP ps) = ListP <$> mapM convPat ps
convPat (SigP p t) = SigP <$> convPat p <*> pure t
convPat (ViewP exp p) = ViewP exp <$> convPat p

isExp :: String -> Q Exp
isExp input = case parse input of
  ParseFailed (SrcLoc _ line col) str ->
    fail $ "can't parse a pattern at line " ++ show line ++ ", column " ++ show col ++ ": " ++ str
  ParseOk pat' -> do
    (pat, eqs) <- runWriterT $ convPat $ toPat (pat' :: H.Pat)
    eq <- VarE <$> lookupValueName' "=="
    and <- VarE <$> lookupValueName' "&&"
    true <- ConE <$> lookupValueName' "True"
    false <- ConE <$> lookupValueName' "False"
    let texp = foldr (\(a, b) e -> InfixE (Just $ InfixE (Just $ VarE a) eq (Just $ VarE b)) and (Just e)) true eqs
    return $ LamCaseE [ Match pat (GuardedB [(NormalG texp, true)]) []
                      , Match WildP (NormalB false) []
                      ]

is :: QuasiQuoter
is = QuasiQuoter { quoteExp = isExp
                 , quotePat = const $ fail "'is' is supported only for expressions"
                 , quoteType = const $ fail "'is' is supported only for expressions"
                 , quoteDec = const $ fail "'is' is supported only for expressions"
                 }
