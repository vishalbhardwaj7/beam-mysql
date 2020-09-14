{-# LANGUAGE KindSignatures #-}

module Database.Beam.MySQL.Syntax.Render where

import           Data.Binary (Binary (put))
import qualified Data.Binary.Builder as Bin
import           Data.Bool (bool)
import           Data.ByteString.Builder.Scientific (FPFormat (Fixed),
                                                     formatScientificBuilder)
import           Data.Char (isLatin1)
import           Data.Foldable (fold)
import           Data.HashSet (HashSet)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Semigroup (sconcat)
import           Data.String (IsString)
import           Data.Text (Text, unpack)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Vector (toList)
import           Database.Beam.MySQL.Syntax.Delete (MySQLDelete (DeleteStmt))
import           Database.Beam.MySQL.Syntax.Misc (MySQLFieldNameSyntax (..),
                                                  MySQLQuantifierSyntax (..))
import           Database.Beam.MySQL.Syntax.Select (BinOp (..),
                                                    CaseBranch (CaseBranch),
                                                    CompOp (..),
                                                    MySQLExpressionSyntax (..),
                                                    MySQLTableNameSyntax (TableName),
                                                    PostOp (..), PrefOp (..))
import           Database.Beam.MySQL.Syntax.Value (MySQLValueSyntax (..))
import           Database.MySQL.Base (Query (Query), QueryParam (render),
                                      renderParams)
import           Database.MySQL.Protocol.Escape (escapeBytes, escapeText)
import           Mason.Builder (BuilderFor, LazyByteStringBackend, byteString,
                                char8, int16Dec, int32Dec, int64Dec, int8Dec,
                                intersperse, lazyByteString, string8, textUtf8,
                                toLazyByteString, word16Dec, word32Dec,
                                word64Dec, word8Dec)

data RenderError =
  NotLatin1 {-# UNPACK #-} !Text |
  CharLengthTooLarge {-# UNPACK #-} !Text {-# UNPACK #-} !Word |
  VarCharLengthTooLarge {-# UNPACK #-} !Text {-# UNPACK #-} !Word |
  InvalidBitLength {-# UNPACK #-} !Text {-# UNPACK #-} !Word |
  InvalidVarBitLength {-# UNPACK #-} !Text {-# UNPACK #-} !Word
  deriving stock (Show, Eq)

renderDelete :: MySQLDelete -> Either RenderError (HashSet Text, Query)
renderDelete (DeleteStmt ann' tableName wher) = do
  params <- traverse valueToParam . toList $ ann'.parameters
  let tabs = ann'.tablesInvolved
  tableName' <- renderTableName tableName
  wher' <- traverse renderExpr wher
  let delete' =
        "DELETE FROM " <>
        tableName' <>
        foldMap (" WHERE " <>) wher' <>
        ";"
  let query = renderParams (Query . toLazyByteString $ delete') params
  pure (tabs, query)

-- Helpers

newtype P = P (BuilderFor LazyByteStringBackend)

instance QueryParam P where
  {-# INLINABLE render #-}
  render (P b) = put . toLazyByteString $ b

valueToParam :: MySQLValueSyntax -> Either RenderError P
valueToParam = fmap P . \case
  VBool b -> pure . bool "FALSE" "TRUE" $ b
  VInt8 i -> pure . int8Dec $ i
  VInt16 i -> pure . int16Dec $ i
  VInt32 i -> pure . int32Dec $ i
  VInt64 i -> pure . int64Dec $ i
  VWord8 w -> pure . word8Dec $ w
  VWord16 w -> pure . word16Dec $ w
  VWord32 w -> pure . word32Dec $ w
  VWord64 w -> pure . word64Dec $ w
  VScientific s ->
    pure .
      lazyByteString .
      Bin.toLazyByteString .
      formatScientificBuilder Fixed Nothing $ s
  VNothing -> pure "NULL"
  VNull -> pure "NULL"
  VByteString b -> pure . quoteWrap . byteString . escapeBytes $ b
  VText t -> do
    let t' = unpack . escapeText $ t
    builder <-
      sconcat . ((pure . char8 $ '\'') :|) . fmap go $ t'
    pure $ builder <> "'"
    where
      go :: Char -> Either RenderError (BuilderFor LazyByteStringBackend)
      go c = if isLatin1 c
        then pure . char8 $ c
        else Left . NotLatin1 $ t
  VDay d ->
    pure . quoteWrap . string8 . formatTime defaultTimeLocale "%F" $ d
  VLocalTime lt ->
    pure. quoteWrap . string8 . formatTime defaultTimeLocale "%F %T%Q" $ lt
  VTimeOfDay tod ->
    pure . quoteWrap . string8 . formatTime defaultTimeLocale "%T%Q" $ tod

renderTableName ::
  MySQLTableNameSyntax -> Either RenderError (BuilderFor LazyByteStringBackend)
renderTableName (TableName mSchema nam) = do
  let mSchema' = fmap textUtf8 mSchema
  let nam' = textUtf8 nam
  pure $ fold mSchema' <> backtickWrap nam'

renderExpr ::
  MySQLExpressionSyntax -> Either RenderError (BuilderFor LazyByteStringBackend)
renderExpr = \case
  Placeholder{} -> pure . char8 $ '?'
  Row _ exprs' -> do
    exprs'' <- traverse renderExpr exprs'
    pure . bracketWrap . intersperse ", " . toList $ exprs''
  Coalesce _ exprs' -> do
    exprs'' <- traverse renderExpr exprs'
    pure . ("COALESCE " <>) . bracketWrap . intersperse ", " . toList $ exprs''
  Case _ cases' def' -> do
    cases'' <- traverse renderCaseBranch cases'
    def'' <- renderExpr def'
    pure $
      "CASE " <>
      (intersperse " " . toList $ cases'') <>
      "ELSE " <>
      def'' <>
      " END"
  Field _ fieldName' -> renderFieldName fieldName'
  BinaryOperation _ op' l' r' -> do
    op'' <- renderBinOp op'
    l'' <- renderExpr l'
    r'' <- renderExpr r'
    pure $
      bracketWrap l'' <>
      " " <>
      op'' <>
      " " <>
      bracketWrap r''
  ComparisonOperation _ op q l r -> do
    op' <- renderCompOp op
    q' <- traverse renderQuantifier q
    l' <- renderExpr l
    r' <- renderExpr r
    pure $
      bracketWrap l' <>
      " " <>
      op' <>
      fold q' <>
      " " <>
      bracketWrap r'
  PrefixOperation _ op e -> do
    op' <- renderPrefOp op
    e' <- renderExpr e
    pure $ op' <> bracketWrap e'
  PostfixOperation _ op e -> do
    op' <- renderPostOp op
    e' <- renderExpr e
    pure $ bracketWrap e' <> op'

renderPostOp :: PostOp -> Either RenderError (BuilderFor LazyByteStringBackend)
renderPostOp = pure . \case
  GIsNull -> "IS NULL"
  GIsNotNull -> "IS NOT NULL"
  GIsTrue -> "IS TRUE"
  GIsNotTrue -> "IS NOT TRUE"
  GIsFalse -> "IS FALSE"
  GIsNotFalse -> "IS NOT FALSE"
  GIsUnknown -> "IS UNKNOWN"
  GIsNotUnknown -> "IS NOT UNKNOWN"

renderPrefOp :: PrefOp -> Either RenderError (BuilderFor LazyByteStringBackend)
renderPrefOp = pure . \case
  LNot -> "NOT"
  NNegate -> "-"
  TCharLength -> "CHAR_LENGTH"
  TOctetLength -> "OCTET_LENGTH"
  BBitLength -> "BIT_LENGTH"
  TLower -> "LOWER"
  TUpper -> "UPPER"
  TTrim -> "TRIM"
  NAbs -> "ABS"

renderCompOp :: CompOp -> Either RenderError (BuilderFor LazyByteStringBackend)
renderCompOp = pure . \case
  CEq -> "="
  CNe -> "<>"
  CLe -> "<="
  CLt -> "<"
  CGe -> ">="
  CGt -> ">"

renderQuantifier ::
  MySQLQuantifierSyntax -> Either RenderError (BuilderFor LazyByteStringBackend)
renderQuantifier = pure . \case
  Any -> "ANY"
  All -> "ALL"

renderBinOp :: BinOp -> Either RenderError (BuilderFor LazyByteStringBackend)
renderBinOp = pure . \case
  LAnd -> "AND"
  LOr -> "OR"
  NAdd -> "+"
  NMul -> "*"
  NSub -> "-"
  NDiv -> "/"
  NMod -> "%"
  GLike -> "LIKE"
  GOverlaps -> "OVERLAPS"

renderFieldName ::
  MySQLFieldNameSyntax -> Either RenderError (BuilderFor LazyByteStringBackend)
renderFieldName = pure . \case
  UnqualifiedField f -> backtickWrap . textUtf8 $ f
  QualifiedField t f ->
    (backtickWrap . textUtf8 $ t) <> (backtickWrap . textUtf8 $ f)

renderCaseBranch :: CaseBranch -> Either RenderError (BuilderFor LazyByteStringBackend)
renderCaseBranch (CaseBranch c a) = do
  c' <- renderExpr c
  a' <- renderExpr a
  pure $
    "WHEN " <>
    c' <>
    " THEN " <>
    a'

wrap :: (Semigroup s) => s -> s -> s -> s
wrap l r v = l <> v <> r

bracketWrap :: (IsString s, Semigroup s) => s -> s
bracketWrap = wrap "(" ")"

quoteWrap :: (IsString s, Semigroup s) => s -> s
quoteWrap = wrap "'" "'"

backtickWrap :: (IsString s, Semigroup s) => s -> s
backtickWrap = wrap "`" "`"
