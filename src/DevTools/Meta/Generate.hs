{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module DevTools.Meta.Generate
    ( write
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (empty)
import           Data.Bifunctor (first)
import           Data.Bool (bool)
import           Data.Char (toLower, toUpper)
import           Data.Foldable (fold, toList, traverse_)
import           Data.Functor.Identity (runIdentity)
import           Data.List (intersperse, sort)
import           Data.Maybe (mapMaybe)
import           Data.Monoid (Ap (Ap), getAp)
import           Data.Tuple (swap)
import           Prelude hiding (last, lookup, show, unlines)
import qualified Prelude as P


-- devtools-api-generate -----------------------------------------------------
import           DevTools.Meta.Protocol


-- directory -----------------------------------------------------------------
import           System.Directory
                     ( createDirectoryIfMissing, removePathForcibly
                     )


-- filepath ------------------------------------------------------------------
import           System.FilePath ((</>), (<.>))


-- text ----------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import           Data.Text.Lazy.Builder (Builder, fromText, toLazyText)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict
                     ( State, execState, get, gets, modify, put, state
                     , StateT, evalStateT, mapStateT
                     )
import           Control.Monad.Trans.Writer.CPS (writerT, runWriterT)


-- unordered-containers ------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS


-- witherable ----------------------------------------------------------------
import           Data.Witherable (wither)


------------------------------------------------------------------------------
qualify :: Text -> Type -> Type
qualify name = go
  where
    go (Ref ref) = Ref $ case T.breakOnEnd "." ref of
        (module', _)
            | T.null module' -> name <> "." <> ref
            | otherwise -> ref
    go (Record ps) = Record (map cogo ps)
      where
        cogo (Property n d o x c t) = Property n d o x c (go t)
    go (Array a) = Array (go a)
    go type_ = type_


------------------------------------------------------------------------------
type Refs = HashMap Text Type


------------------------------------------------------------------------------
type Ords = HashSet Text


------------------------------------------------------------------------------
getRefs :: Protocol -> Refs
getRefs protocol = H.fromList $ foldMap go domains
  where
    Protocol _ domains = protocol
    go domain = foldMap (foldMap subgo) datas
      where
        Domain name _ _ _ _ datas _ _ = domain
        subgo data_ = [(name <> "." <> subname, qualify name type_)]
          where
            Data subname _ _ _ type_ = data_


------------------------------------------------------------------------------
getOrds :: Refs -> Ords
getOrds refs = HS.fromMap . H.mapMaybe (bool empty (pure ())) $
    execState (traverse_ reference (H.keys refs)) mempty
  where
    reference ref = do
        ord <- go mempty ref
        modify $ H.insert ref ord
    go recursions ref = do
        contains <- gets (H.lookup ref)
        case contains of
            Nothing -> case HS.member ref recursions of
                False -> case H.lookup ref refs of
                    Nothing -> pure False
                    Just type_ -> subgo type_
                True -> pure True
            Just a -> pure a
      where
        recursions' = HS.insert ref recursions
        subgo Any = pure False
        subgo Object = pure False
        subgo (Ref ref') = go recursions' ref'
        subgo (Record ps) = and <$> traverse cogo ps
        subgo (Array a) = subgo a
        subgo _ = pure True
        cogo (Property _ _ _ _ _ a) = subgo a


------------------------------------------------------------------------------
ordable :: Type -> State Names Bool
ordable type_ = do
    name <- gets name
    ords <- gets ords
    pure $ go ords $ qualify name type_
  where
    go ords = subgo
      where
        subgo Any = False
        subgo Object = False
        subgo (Ref ref) = HS.member ref ords
        subgo (Record ps) = all cogo ps
        subgo (Array a) = subgo a
        subgo _ = True
        cogo (Property _ _ _ _ _ a) = subgo a


------------------------------------------------------------------------------
pordable :: Property -> State Names Bool
pordable (Property _ _ _ _ _ type_) = ordable type_


------------------------------------------------------------------------------
cabal :: Protocol -> Text -> Builder
cabal (Protocol (Version major minor) domains) revision = unlines
    [ "name:           devtools-api"
    , "version:        " <> version
    , "synopsis:       Haskell bindings to the Chrome DevTools Protocol\
        \ (bindings)"
    , "description:    https://github.com/ChromeDevTools/devtools-protocol"
    , "license:        MPL-2.0"
    , "author:         Shane O'Brien"
    , "maintainer:     me@shane.sh"
    , "category:       Web"
    , "cabal-version:  >= 1.6"
    , "build-type:     Simple"
    , "homepage:       https://github.com/duairc/devtools-api"
    , "bug-reports:    https://github.com/duairc/devtools-api/issues"
    , ""
    , "extra-source-files:"
    , "  LICENSE"
    , ""
    , "library"
    , "  hs-source-dirs:"
    , "    src"
    , ""
    , "  exposed-modules:"
    , unlines1 $ map go domains
    , ""
    , "  other-modules:"
    , unlines1 $ map rego domains
    , ""
    , "  build-depends:"
    , "    aeson,"
    , "    base,"
    , "    devtools,"
    , "    deepseq,"
    , "    hashable,"
    , "    text,"
    , "    vector"
    , ""
    , "  ghc-options: -Wall"
    , ""
    , "source-repository head"
    , "  type:     git"
    , "  location: https://github.com/duairc/devtools-api.git"
    ]
  where
    version = fromText major <> "." <> fromText minor <> "."
        <> fromText revision
    go (Domain domain _ _ _ _ _ _ _) = "    DevTools.API." <> fromText domain
    rego (Domain domain _ _ _ _ _ _ _) = "    DevTools.API."
        <> fromText domain <> ".Types"


------------------------------------------------------------------------------
core :: Monad m => Domain -> StateT Names m Builder
core domain = relax $ do
    modify $ \names -> names {name = name}
    declarations <- foldMapA (foldMapA declare) types
    dependencies <- gets modules
    pragmas <- foldMapA id
        [ warning experimental empty
        , deprecation deprecated empty
        ]
    pure $ unlines
        [ extensions
        , comment <> "module DevTools.API." <> fromText name <> ".Types"
            <> pragmas
        , "where" <> imports dependencies <> declarations
        ]
  where
    Domain name description _ _ _ _ _ _ = domain
    Domain _ _ experimental deprecated _ _ _ _ = domain
    Domain _ _ _ _ _ types _ _ = domain
    extensions = unlines
        [ "{-# LANGUAGE DeriveAnyClass #-}"
        , "{-# LANGUAGE DeriveGeneric #-}"
        , "{-# LANGUAGE DuplicateRecordFields #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , ""
        , "{-# OPTIONS_GHC -fno-warn-deprecations #-}"
        , "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
        ]
    comment = haddock mempty mempty description
    imports dependencies = fold
        [ header "aeson" <> unlines
            [ "import           Data.Aeson ((.:), (.:?), (.=))"
            , "import qualified Data.Aeson as A"
            , "                     ( FromJSON, ToJSON, Object, Value (Null)"
            , "                     , parseJSON, withText, withArray,\
                \ withObject"
            , "                     , toEncoding, pairs, toJSON, object"
            , "                     )"
            ]
        , header "base" <> unlines
            [ "import           Control.Applicative ((<|>), (<*>))"
            , "import qualified Control.Applicative as P (empty)"
            , "import qualified Data.Foldable as P (fold)"
            , "import           Data.Function (($))"
            , "import           Data.Functor ((<$>))"
            , "import qualified Data.Maybe as P (catMaybes)"
            , "import qualified Data.Typeable as P (Typeable)"
            , "import qualified GHC.Generics as P (Generic)"
            , "import qualified Prelude as P"
            ]
        , header "deepseq" <> unlines
            [ "import qualified Control.DeepSeq as D (NFData)"
            ]
        , case sort $ toList dependencies of
            [] -> mempty
            as -> header "devtools-api" <> foldMap ((<> "\n") . go) as
              where
                go a = "import qualified DevTools.API." <> fromText a
                    <> ".Types as " <> fromText a
        , header "hashable" <> unlines
            [ "import qualified Data.Hashable as H (Hashable)"
            ]
        , header "text" <> unlines
            [ "import qualified Data.Text as T"
            ]
        , header "vector" <> unlines
            [ "import           Data.Vector ((!?))"
            ]
        ]


------------------------------------------------------------------------------
fruit :: Monad m => Domain -> StateT Names m Builder
fruit domain = relax $ do
    oldValues <- gets getValues
    modify $ \names -> names
        { name = name
        , modules = mempty
        , values = mempty
        , selectors = mempty
        , pragmas = mempty
        }
    commands' <- traverse preallocateM $ fold commands
    events' <- traverse preallocateE $ fold events
    callers <- foldMapA (uncurry caller) commands'
    listeners <- foldMapA (uncurry listener) events'
    newValues <- gets getValues
    let exclusions = HS.intersection oldValues newValues
    dependencies <- gets modules
    pragmas <- foldMapA id
        [ warning experimental empty
        , deprecation deprecated empty
        ]
    pure $ unlines
        [ extensions
        , comment <> "module DevTools.API." <> fromText name <> pragmas
        , "    ( module DevTools.API." <> fromText name <> ".Types"
        , "    , module DevTools.API." <> fromText name
        , "    ) "
        , "where" <> imports dependencies exclusions <> callers <> listeners
        ]
  where
    getValues names = HS.difference (values names) (selectors names)
    Domain name description _ _ _ _ _ _ = domain
    Domain _ _ experimental deprecated _ _ _ _ = domain
    Domain _ _ _ _ _ _ commands events = domain
    preallocateM command = do
        new <- freshValueName old
        pure (command, new)
      where
        Command old _ _ _ _ _ = command
    preallocateE event = do
        new <- freshValueName old
        pure (event, new)
      where
        Event old _ _ _ _ = event
    extensions = unlines
        [ "{-# LANGUAGE DeriveAnyClass #-}"
        , "{-# LANGUAGE DeriveGeneric #-}"
        , "{-# LANGUAGE DuplicateRecordFields #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "{-# LANGUAGE TypeFamilies #-}"
        , ""
        , "{-# OPTIONS_GHC -fno-warn-deprecations #-}"
        , "{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}"
        , "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
        ]
    comment = haddock mempty mempty description
    imports dependencies exclusions = fold
        [ header "aeson" <> unlines
            [ "import           Data.Aeson ((.:), (.:?), (.=))"
            , "import qualified Data.Aeson as A"
            , "                     ( FromJSON, ToJSON, Object, Value (Null)"
            , "                     , parseJSON, withText, withArray,\
                \ withObject"
            , "                     , toEncoding, pairs, toJSON, object"
            , "                     )"
            ]
        , header "base" <> unlines
            [ "import           Control.Applicative ((<|>), (<*>))"
            , "import qualified Control.Applicative as P (empty)"
            , "import qualified Data.Foldable as P (fold)"
            , "import           Data.Function (($))"
            , "import           Data.Functor ((<$>))"
            , "import qualified Data.Maybe as P (catMaybes)"
            , "import qualified Data.Proxy as P (Proxy (Proxy))"
            , "import qualified Data.Typeable as P (Typeable)"
            , "import qualified GHC.Generics as P (Generic)"
            , "import qualified Prelude as P"
            ]
        , header "deepseq" <> unlines
            [ "import qualified Control.DeepSeq as D (NFData)"
            ]
        , header "devtools" <> unlines
            [ "import qualified DevTools.Event as E (Event, Result, name)"
            , "import qualified DevTools.Method as M (Method, Result, name)"
            ]
        , header "devtools-api" <> foldMap ((<> "\n") . go) dependencies'
        , header "hashable" <> unlines
            [ "import qualified Data.Hashable as H (Hashable)"
            ]
        , header "text" <> unlines
            [ "import qualified Data.Text as T"
            ]
        , header "vector" <> unlines
            [ "import           Data.Vector ((!?))"
            ]
        ]
      where
        dependencies' = sort $ (name, pure exclusions) :
            map (flip (,) empty) (toList dependencies)
        go (dependency, Nothing) = 
            "import qualified DevTools.API." <> fromText dependency
                <> ".Types as " <> fromText dependency
        go (dependency, Just hidings) = import_ <> case hidings' of
            (a : as) -> unlines1
                [ " hiding\n"
                , "                      ( " <> subgo a
                , unlines1 (map (("                      , " <>) . subgo) as)
                , "                      )"
                ]
              where
                subgo = fromText
            _ -> mempty
          where
            import_ = "import           DevTools.API." <> fromText dependency
                <> ".Types"
            hidings' = sort $ HS.toList hidings


------------------------------------------------------------------------------
header :: Text -> Builder
header package = "\n\n-- " <> fromText package <> fold (replicate n "-")
    <> "\n"
  where
    n = 75 - T.length package


------------------------------------------------------------------------------
line :: Builder
line = "\n\n-----------------------------------------------------------------\
    \-------------\n"


------------------------------------------------------------------------------
data Names = Names
    { name :: !Text
    , ords :: !Ords
    , types :: !(HashSet Text)
    , constructors :: !(HashSet Text)
    , values :: !(HashSet Text)
    , selectors :: !(HashSet Text)
    , modules :: !(HashSet Text)
    , pragmas :: !(HashSet Text)
    , enums :: !(HashMap [Text] Text)
    , records :: !(HashMap [Property] Text)
    , arrays :: !(HashMap Type Text)
    }


------------------------------------------------------------------------------
instance Semigroup Names where
    Names n o t c v s m p e r a <> Names n' o' t' c' v' s' m' p' e' r' a'
        = Names (n <> n') (o <> o') (t <> t') (c <> c') (v <> v') (s <> s')
            (m <> m') (p <> p') (e <> e') (r <> r') (a <> a')


------------------------------------------------------------------------------
instance Monoid Names where
    mempty = Names mempty mempty mempty mempty mempty mempty mempty mempty
        mempty mempty mempty


------------------------------------------------------------------------------
inline :: Type -> State Names (Maybe Text)
inline Any = pure $ pure "A.Value"
inline Boolean = pure $ pure "P.Bool"
inline Integer = pure $ pure "P.Int"
inline Number = pure $ pure "P.Double"
inline String = pure $ pure "T.Text"
inline Object = pure $ pure "A.Object"
inline (Enum as) = gets (H.lookup as . enums)
inline (Record as) = gets (H.lookup as . records)
inline (Array a) = do
    mname <- gets (H.lookup a . arrays)
    case mname of
        Just name -> pure $ pure name
        Nothing -> fmap (("[" <>) . (<> "]")) <$> inline a
inline (Ref ref) = case T.breakOnEnd "." ref of
    (module', _)
        | T.null module' -> pure $ pure ref
        | otherwise -> do
            modify $ \names -> names
                { modules = HS.insert (T.init module') (modules names)
                }
            pure $ pure ref


------------------------------------------------------------------------------
inlining :: Type -> State Names
    (Either (Text -> State Names (Builder, (Text, [Text]))) Text)
inlining type_ = do
    mname <- inline type_
    case mname of
        Just name -> pure $ pure name
        Nothing -> case type_ of
            Enum as -> pure $ Left $ enum as
            Record as -> pure $ Left $ record as
            Array a -> do
                ename <- inlining a
                pure $ case ename of
                    Left _ -> Left $ array a
                    Right name -> Right $ "[" <> name <> "]"
            _ -> inlining type_


------------------------------------------------------------------------------
naming :: Type -> Text -> State Names (Builder, (Text, [Text]))
naming type_ name = do
    enamed <- inlining type_
    case enamed of
        Left f -> fmap (first (line <>)) $ f name
        Right named -> pure (mempty, (named, []))


------------------------------------------------------------------------------
declaring :: Text -> Type -> State Names (Builder, (Text, [Text]))
declaring name type_ = do
    enamed <- inlining type_
    case enamed of
        Left f -> f name
        Right named -> case type_ of
            Record ps -> record ps name
            _ -> do
                name' <- freshTypeName name
                let declaration = unlines
                     [ "type " <> fromText name' <> " = " <> fromText named
                     ]
                pure (declaration, (name', []))


------------------------------------------------------------------------------
array :: Type -> Text -> State Names (Builder, (Text, [Text]))
array type_ name = do
    name' <- freshTypeName name
    modify $ \names -> names
        { arrays = H.insert type_ name' (arrays names)
        }
    (declaration, (item, _)) <- naming type_ $ name <> "Item"
    let declaration' = unlines
         [ "type " <> fromText name' <> " = [" <> fromText item <> "]"
         ] <> declaration
    pure (declaration', (name', []))


------------------------------------------------------------------------------
enum :: [Text] -> Text -> State Names (Builder, (Text, [Text]))
enum as name = do
    name' <- freshTypeName name
    modify $ \names -> names
        { enums = H.insert as name' (enums names)
        }
    constructors <- traverse go as
    let data_ = "data " <> fromText name'
    let declaration = unlines (data_ : case constructors of
         [] -> []
         (c : cs) -> one : rest
           where
            one = "    = " <> fromText (snd c)
            rest = map (("    | " <>) . fromText . snd) cs)
    pure
        ( declaration <> deriving_ <> instances name' constructors
        , (name', map snd constructors)
        )
  where
    go a = (,) a <$> freshConstructorName a
    deriving_ = unlines
        [ "  deriving"
        , "    ( P.Eq, P.Ord, P.Read, P.Show, P.Enum, P.Bounded, P.Generic,\
            \ P.Typeable"
        , "    , D.NFData, H.Hashable"
        , "    )"
        ]
    instances name' cs = from <> to
      where
        from = line <> unlines
            [ "instance A.FromJSON " <> fromText name' <> " where"
            , "    parseJSON = A.withText " <> show name
                <> " $ \\t -> case t of"
            , foldMap subgo cs <> "        _ -> P.empty"
            ]
          where
            subgo (old, new) = unlines
                [ "        " <> show old <> " -> P.pure " <> fromText new
                ]
        to = line <> unlines
            [ "instance A.ToJSON " <> fromText name' <> " where"
                <> foldMap subgo cs
            ]
          where
            subgo (old, new) = "\n    toJSON " <> fromText new <> " = "
                <> show old


------------------------------------------------------------------------------
record :: [Property] -> Text -> State Names (Builder, (Text, [Text]))
record properties name = do
    name' <- freshTypeName name
    constructor <- freshConstructorName name
    modify $ \names -> names
        { records = H.insert properties name' (records names)
        }
    (fields, declarations) <- runWriterT $ traverse go properties
    ord <- and <$> traverse pordable properties
    let data_ = "data " <> fromText name' <> " = " <> fromText constructor
    pragmas <- foldMapA id [deprecations fields, warnings fields]
    let declaration = unlines (pragmas <> data_ : case fields of
         [] -> ["    {", "    }"]
         (f : fs) ->
            [ "    { " <> selector "" "      " "      " False f <>
                prelines (map (selector "      " "      " "    , " True) fs)
            , "    }"
            ])
    pure
        ( declaration <> deriving_ ord <> instances name' constructor fields
            <> declarations
        , (name', [constructor])
        )
  where
    go property = writerT $ do
        (declaration, (type', _)) <- naming type_ $ bigName old
        new <- freshSelectorName old
        pure ((property, new, type'), declaration)
      where
        Property old _ _ _ _ type_ = property
    warnings fields = warning (pure (not (null fields'))) fields'
      where
        subgo (Property _ _ _ (Just True) _ _, new, _) = pure new
        subgo _ = empty
        fields' = mapMaybe subgo fields
    deprecations fields = deprecation (pure (not (null fields'))) fields'
      where
        subgo (Property _ _ _ _ (Just True) _, new, _) = pure new
        subgo _ = empty
        fields' = mapMaybe subgo fields
    selector initial subsequent prefix i (property, new, type_) =
        haddock initial subsequent description <> iprefix <> fromText new
            <> " :: !" <> type'
      where
        Property _ description optional _ _ _ = property
        type' = case optional of
            Just True -> "(P.Maybe " <> fromText type_ <> ")"
            _ -> fromText type_
        iprefix = bool initial prefix $ maybe i (const True) description
    deriving_ ord
        | ord = unlines
            [ "  deriving"
            , "    ( P.Eq, P.Ord, P.Read, P.Show, P.Generic, P.Typeable"
            , "    , D.NFData, H.Hashable"
            , "    )"
            ]
        | otherwise = unlines
            [ "  deriving"
            , "    ( P.Eq, P.Read, P.Show, P.Generic, P.Typeable"
            , "    , D.NFData, H.Hashable"
            , "    )"
            ]
    instances name' constructor fields = from <> to <> semigroup <> monoid
      where
        from = line <> unlines
            [ "instance A.FromJSON " <> fromText name' <> " where"
            , case fields of
                [] -> unlines1
                    [ "    parseJSON A.Null = P.pure " <> fromText constructor
                    , "    parseJSON v = A.withArray " <> show name
                        <> " go v"
                    , "        <|> A.withObject " <> show name <> " go v"
                    , "      where"
                    , "        go _ = P.pure " <> fromText constructor
                    ]
                (f : fs) -> unlines1
                    [ "    parseJSON v = ago v <|> ogo v"
                    , "      where"
                    , "        ogo = A.withObject " <> show name
                        <> " $ \\_o -> "
                        <> fromText constructor
                    , "            <$> " <> field f <> prelines
                        (map (("            <*> " <>) . field) fs)
                    , "        ago = A.withArray " <> show name
                        <> " $ \\_a -> "
                        <> fromText constructor
                    , "            <$> " <> cell (length []) f <> prelines
                        (imap (\n -> ("            <*> " <>) . cell (n + 1))
                            fs)
                    ]
            ]
          where
            field (property, _, _) = case optional of
                Just True -> "_o .:? " <> show old
                _ -> "_o .: " <> show old
              where
                Property old _ optional _ _ _ = property
            cell n (property, _, _) = case optional of
                Just True -> "P.traverse A.parseJSON (_a !? " <> show n
                    <> ")"
                _ -> "P.maybe P.empty A.parseJSON (_a !? " <> show n <> ")"
              where
                Property _ _ optional _ _ _ = property
        to = line <> unlines
            [ "instance A.ToJSON " <> fromText name' <> " where"
            , "    toEncoding " <> pattern
                <> " = A.pairs $ P.fold $ P.catMaybes"
            , list
            , "    toJSON " <> pattern <> " = A.object $ P.catMaybes"
            , list
            ]
          where
            pattern = case fields of
                [] -> fromText constructor
                _ -> "(" <> fromText constructor <> fold (imap subgo fields)
                    <> ")"
                  where
                    subgo n _ = " _" <> show n
            list = case fields of
                [] -> unlines1
                    [ "        ["
                    , "        ]"
                    ]
                (a : as) -> unlines1
                    [ "        [ " <> field (length []) a <> prelines
                        (imap (\n -> ("        , " <>) . field (n + 1)) as)
                    , "        ]"
                    ]
              where
                field n (property, _, _) = case optional of
                    Just True -> "(" <> show old <> " .=) <$> _"
                        <> show n 
                    _ -> "P.pure $ " <> show old <> " .= _" <> show n
                  where
                    Property old _ optional _ _ _ = property
        semigroup = line <> unlines
            [ "instance P.Semigroup " <> fromText name' <> " where"
            , "    " <> left <> " <> " <> right <> " = " <> result
            ]
          where
            left = fromText constructor <> fold (imap subgo fields)
              where
                subgo n _ = " _" <> show n
            right = fromText constructor <> fold (imap subgo fields)
              where
                subgo n (property, _, _) = case optional of
                    Just True -> " __" <> show n
                    _ -> " _"
                  where
                    Property _ _ optional _ _ _ = property
            result = fromText constructor <> fold (imap subgo fields)
              where
                subgo n (property, _, _) = case optional of
                    Just True -> " (_" <> show n <> " <|> __" <> show n
                        <> ")"
                    _ -> " _" <> show n
                  where
                    Property _ _ optional _ _ _ = property
        monoid
            | all option fields = line <> unlines
                [ "instance P.Monoid " <> fromText name' <> " where"
                , "    mempty = " <> fromText constructor
                    <> foldMap (const " P.empty") fields
                ]
            | otherwise = mempty
          where
            option (property, _, _) = maybe False id optional
              where
                Property _ _ optional _ _ _ = property


------------------------------------------------------------------------------
bigName :: Text -> Text
bigName = foldMap (mapHead toUpper) . T.split (\c -> c == '-' || c == '_')


------------------------------------------------------------------------------
smallName :: Text -> Text
smallName name = case go of
    "type" -> "type_"
    "class" -> "class_"
    "instance" -> "instance_"
    "module" -> "module_"
    "data" -> "data_"
    "newtype" -> "newtype_"
    "deriving" -> "deriving_"
    "case" -> "case_"
    "of" -> "of_"
    "where" -> "where_"
    "let" -> "let_"
    "in" -> "in_"
    "do" -> "do_"
    "import" -> "import_"
    "if" -> "if_"
    "then" -> "then_"
    "else" -> "else_"
    result -> result
  where
    go = case T.split (\c -> c == '-' || c == '_') name of
        [] -> mempty
        (n : ns) -> mapHead toLower n <> foldMap (mapHead toUpper) ns


------------------------------------------------------------------------------
mapHead :: (Char -> Char) -> Text -> Text
mapHead f text = case T.uncons text of
    Nothing -> text
    Just (c, cs) -> T.cons (f c) cs


------------------------------------------------------------------------------
freshTypeName :: Text -> State Names Text
freshTypeName = go . bigName
  where
    go name = do
        names <- get
        case HS.member name (types names) of
            False -> do
                put $ names {types = HS.insert name (types names)}
                pure name
            True -> go $ name <> "_"


------------------------------------------------------------------------------
freshConstructorName :: Text -> State Names Text
freshConstructorName = go . bigName
  where
    go name = do
        names <- get
        case HS.member name (constructors names) of
            False -> do
                put $ names
                    { constructors = HS.insert name (constructors names)
                    }
                pure name
            True -> go $ name <> "_"


------------------------------------------------------------------------------
freshSelectorName :: Text -> State Names Text
freshSelectorName = go . smallName
  where
    go name = do
        names <- get
        case HS.member name (values names) of
            False -> do
                put $ names
                    { selectors = HS.insert name (selectors names)
                    , values = HS.insert name (values names)
                    }
                pure name
            True -> case HS.member name (selectors names) of
                False -> go $ name <> "_"
                True -> pure name


------------------------------------------------------------------------------
freshValueName :: Text -> State Names Text
freshValueName = go . smallName
  where
    go name = do
        names <- get
        case HS.member name (values names) of
            False -> do
                put $ names
                    { values = HS.insert name (values names)
                    }
                pure name
            True -> go $ name <> "_"


------------------------------------------------------------------------------
declare :: Data -> State Names Builder
declare (Data name description experimental deprecated type_) = do
    (declaration, (name', _)) <- declaring name type_
    pragmas <- foldMapA id
        [ deprecation deprecated (pure name')
        , warning experimental (pure name')
        ]
    pure $ line <> haddock mempty mempty description <> pragmas <> declaration


------------------------------------------------------------------------------
caller :: Command -> Text -> State Names Builder
caller command new = do
    (ideclaration, (iname, iconstructors)) <- declaring old irecord
    let constructor = foldr const iname iconstructors
    ipragmas <- foldMapA id
        [ deprecation deprecated (pure iname)
        , warning experimental (pure iname)
        ]
    declaration <- instance_ iname
    function <- helper iname constructor
    pure $ mconcat
        [ line <> haddock mempty mempty description <> ipragmas
            <> ideclaration
        , declaration
        , function
        ]
  where
    Command old description experimental deprecated params returns = command
    irecord = Record (fold params)
    orecord = Record (fold returns)
    instance_ iname = do
        domain <- gets name
        case returns of
            Nothing -> pure $ go domain empty
            Just _ -> do
                (odeclaration, (oname, _)) <-
                    declaring (old <> "Result") orecord
                opragmas <- foldMapA id
                    [ deprecation deprecated (pure oname)
                    , warning experimental (pure oname)
                    ]
                let odeclarations = line <> haddock mempty mempty description
                     <> opragmas <> odeclaration
                pure $ odeclarations <> go domain (pure oname)
      where
        go domain oname = line <> unlines [signature, result, method]
          where
            signature = "instance M.Method " <> fromText iname <> " where"
            result = "    type Result " <> fromText iname <> " = " <>
                maybe "()" fromText oname
            method = "    name _ = " <> show (domain <> "." <> old)
    helper iname constructor = do
        (requireds, types) <- runWriterT $ wither select $ fold params
        pragmas <- foldMapA id
            [ deprecation deprecated (pure new)
            , warning experimental (pure new)
            ]
        let declaration = signature requireds
        pure $ unlines
            [ line <> haddock mempty mempty description <> pragmas
                <> declaration <> types
            , body requireds
            ]
      where
        select property = case optional of
            Just True -> pure empty
            _ -> do
                (type', _) <- writerT . fmap swap $ naming type_ key
                pure $ Just (comment, type')
          where
            Property key comment optional _ _ type_ = property
        signature requireds = fromText new <> "\n    :: "
            <> foldMap go requireds
            <> fromText iname
          where
            go (comment, type_) = fromText type_ <> argument comment
                <> "\n    -> " 
        body requireds = fromText new <> fold (imap go requireds) <> " = " <>
            fromText constructor <> rego (length []) (fold params)
          where
            go n _ = " _" <> show n
            rego _ [] = mempty
            rego n (p : ps) = case optional of
                Just True -> " P.empty" <> rego n ps
                _ -> " _" <> show n <> rego (n + 1) ps
              where
                Property _ _ optional _ _ _ = p
                


------------------------------------------------------------------------------
listener :: Event -> Text -> State Names Builder
listener event new = do
    (ideclaration, (iname, _)) <- declaring old irecord
    ipragmas <- foldMapA id
        [ deprecation deprecated (pure iname)
        , warning experimental (pure iname)
        ]
    declaration <- instance_ iname
    function <- helper iname
    pure $ mconcat
        [ line <> haddock mempty mempty description <> ipragmas
            <> ideclaration
        , declaration
        , function
        ]
  where
    Event old description experimental deprecated params = event
    irecord = Record (fold params)
    instance_ iname = do
        domain <- gets name
        pure $ line <> unlines [signature, result, method domain]
      where
        signature = "instance E.Event " <> fromText iname <> " where"
        result = "    type Result " <> fromText iname <> " = " <>
            maybe "()" (const (fromText iname)) params
        method domain = "    name _ = " <> show (domain <> "." <> old)
    helper iname = do
        pragmas <- foldMapA id
            [ deprecation deprecated (pure new)
            , warning experimental (pure new)
            ] 
        pure $ unlines
            [ line <> haddock mempty mempty description <> pragmas
                <> signature
            , body
            ]
      where
        signature = fromText new <> " :: P.Proxy " <> fromText iname
        body = fromText new <> " = P.Proxy"


------------------------------------------------------------------------------
prelines :: [Builder] -> Builder
prelines = foldMap ("\n" <>)


------------------------------------------------------------------------------
unlines :: [Builder] -> Builder
unlines = foldMap (<> "\n")


------------------------------------------------------------------------------
unlines1 :: [Builder] -> Builder
unlines1 = fold . intersperse "\n"


------------------------------------------------------------------------------
show :: Show a => a -> Builder
show = fromText . T.pack . P.show


------------------------------------------------------------------------------
metahaddock :: Builder -> Builder -> Builder -> Maybe Text -> Builder
metahaddock meta initial prefix =
    maybe mempty (go . T.replace "`" "@" . T.replace "/" "\\/")
  where
    go comment = case T.lines comment of
        [] -> mempty
        (a : as) -> unlines $ (initial <> "-- " <> meta <> " " <> fromText a)
            : map ((prefix <>) . ("-- " <>) . fromText) as


------------------------------------------------------------------------------
haddock :: Builder -> Builder -> Maybe Text -> Builder
haddock = metahaddock "|"


------------------------------------------------------------------------------
argument :: Maybe Text -> Builder
argument = metahaddock "^" "\n    " "\n    "


------------------------------------------------------------------------------
pragma :: Text -> Text -> Maybe Bool -> [Text] -> State Names Builder
pragma title message (Just True) names = do
    already <- traverse go names
    (wrap, names') <- pure $ case all snd already of
        True -> (wrap, fold $ intersperse ", " $ map fromText names)
          where
            wrap = comment
        False -> (id, rego False already)
    pure $ (<> "\n") $ wrap $ "{-# " <> fromText title <> " " <> names'
        <> bool " " "" (null names) <> show message <> " #-}"
  where
    comment = ("{-" <>) . (<> "-}")
    go name = do
        answer <- state $ test name
        pure (name, answer)
    rego _ [] = mempty
    rego precomma ((name, already) : rest) = wrap (prefix <> fromText name) <>
        rego (precomma || not already) rest
      where
        postcomma = not $ precomma || null rest
        prefix = bool "" ", " precomma
        suffix = bool "" ", " postcomma
        wrap = bool id (comment . (<> suffix)) already
    test name s =
        ( HS.member name (pragmas s)
        , s {pragmas = HS.insert name (pragmas s)}
        )
pragma _ _ _ _ = pure mempty


------------------------------------------------------------------------------
deprecation :: Maybe Bool -> [Text] -> State Names Builder
deprecation = pragma "DEPRECATED" message
  where
    message = "This may be removed in a future release."


------------------------------------------------------------------------------
warning :: Maybe Bool -> [Text] -> State Names Builder
warning = pragma "WARNING" message
  where
    message = "This feature is marked as EXPERIMENTAL."


------------------------------------------------------------------------------
write :: FilePath -> Protocol -> Text -> IO ()
write dir protocol revision = do
    removePathForcibly $ dir </> "src"
    createDirectoryIfMissing True api
    T.writeFile (dir </> "devtools-api.cabal") $ toLazyText $
        cabal protocol revision
    foldMap go domains
  where
    api = dir </> "src" </> "DevTools" </> "API"
    Protocol _ domains = protocol
    ords = getOrds $ getRefs protocol
    go domain = flip evalStateT (mempty {ords = ords}) $ do
        lift $ createDirectoryIfMissing False $ subdir
        core domain >>= lift . T.writeFile subfile . toLazyText
        fruit domain >>= lift . T.writeFile file . toLazyText
      where
        Domain name _ _ _ _ _ _ _ = domain
        file = api </> T.unpack name <.> "hs"
        subdir = api </> T.unpack name
        subfile = subdir </> "Types" <.> "hs"


------------------------------------------------------------------------------
foldMapA :: (Foldable t, Applicative f, Monoid m) => (a -> f m) -> t a -> f m
foldMapA = (getAp .) . foldMap . (Ap .)


------------------------------------------------------------------------------
imap :: (Int -> a -> b) -> [a] -> [b]
imap f = map (uncurry f) . zip [0..]


------------------------------------------------------------------------------
relax :: Monad m => State s a -> StateT s m a
relax = mapStateT $ pure . runIdentity
