module ElmSyntaxToGrain exposing
    ( modules, grainDeclarationsToModuleString
    , GrainLetDeclaration(..), GrainExpression(..), GrainPattern(..), GrainType(..)
    )

{-| Transpiling [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/)
declarations to grain.

@docs modules, grainDeclarationsToModuleString
@docs GrainLetDeclaration, GrainExpression, GrainPattern, GrainType

If you need more fine-grained helpers,
[open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new)

-}

import Data.Graph
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import FastDict
import FastSet
import Print exposing (Print)
import Unicode


{-| The sub-set of F# type syntax used in generated code
-}
type GrainType
    = GrainTypeConstruct
        { moduleOrigin : Maybe String
        , name : String
        , arguments : List GrainType
        }
    | GrainTypeTuple
        { part0 : GrainType
        , part1 : GrainType
        , part2Up : List GrainType
        }
    | GrainTypeVariable String
    | GrainTypeFunction
        { input : List GrainType
        , output : GrainType
        }


{-| The sub-set of F# pattern syntax used in generated code
-}
type GrainPattern
    = GrainPatternIgnore
    | GrainPatternNumber Float
    | GrainPatternChar Char
    | GrainPatternString String
    | GrainPatternVariable String
    | GrainPatternAs
        { variable : String
        , pattern : GrainPattern
        }
    | GrainPatternListCons
        { initialElement0 : GrainPattern
        , initialElement1Up : List GrainPattern
        , tailVariable : Maybe GrainPattern
        }
    | GrainPatternListExact (List GrainPattern)
    | GrainPatternRecordInexhaustive (FastSet.Set String)
    | GrainPatternVariant
        { moduleOrigin : Maybe String
        , name : String
        , values : List GrainPattern
        }
    | GrainPatternTuple
        { part0 : GrainPattern
        , part1 : GrainPattern
        , part2Up : List GrainPattern
        }


{-| The sub-set of F# expression syntax used in generated code
-}
type GrainExpression
    = GrainExpressionFloat Float
    | GrainExpressionChar Char
    | GrainExpressionString String
    | GrainExpressionReference
        { moduleOrigin : Maybe String
        , name : String
        }
    | GrainExpressionRecordAccess
        { record : GrainExpression
        , field : String
        }
    | GrainExpressionTuple
        { part0 : GrainExpression
        , part1 : GrainExpression
        , part2Up : List GrainExpression
        }
    | GrainExpressionIfElse
        { condition : GrainExpression
        , onTrue : GrainExpression
        , onFalse : GrainExpression
        }
    | GrainExpressionList (List GrainExpression)
    | GrainExpressionRecord (FastDict.Dict String GrainExpression)
    | GrainExpressionRecordUpdate
        { originalRecordVariable : String
        , fields : FastDict.Dict String GrainExpression
        }
    | GrainExpressionCall
        { called : GrainExpression
        , argument0 : GrainExpression
        , argument1Up : List GrainExpression
        }
    | GrainExpressionLambda
        { parameter0 : GrainPattern
        , parameter1Up : List GrainPattern
        , result : GrainExpression
        }
    | GrainExpressionMatch
        { matched : GrainExpression
        , case0 :
            { pattern : GrainPattern
            , result : GrainExpression
            }
        , case1Up :
            List
                { pattern : GrainPattern
                , result : GrainExpression
                }
        }
    | GrainExpressionWithLetDeclarations
        { declaration0 : GrainLetDeclaration
        , declaration1Up : List GrainLetDeclaration
        , result : GrainExpression
        }


{-| The sub-set of grain local declaration syntax used in generated grain code
-}
type GrainLetDeclaration
    = GrainLetDestructuring
        { pattern : GrainPattern
        , expression : GrainExpression
        }
    | GrainLetDeclarationValueOrFunction
        { name : String
        , result : GrainExpression
        , type_ : Maybe GrainType
        }


{-| How do references used in a module map to their origin module?

Contains variants, variant function and value declaration names.

-}
type alias ModuleContext =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    }


{-| Calculate valid mappings of qualifications + name
to origin module based on a module's imports.

Requires all exposed names
so we can resolve `exposing (..)` and `EnumType(..)`.

-}
importsToModuleContext :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { valueOrFunctionOrTypeAliasNames : FastSet.Set String
        , enumTypesExposingVariants :
            FastDict.Dict String (FastDict.Dict String { valueCount : Int })
        }
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
    -> ModuleContext
importsToModuleContext moduleExposes imports =
    let
        importsNormal :
            List
                { moduleName : Elm.Syntax.ModuleName.ModuleName
                , alias : Maybe String
                , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                    FastSet.Set String
                , exposedVariants :
                    FastDict.Dict String { valueCount : Int }
                }
        importsNormal =
            implicitImports
                ++ (imports
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ syntaxImport) ->
                                let
                                    importModuleName : Elm.Syntax.ModuleName.ModuleName
                                    importModuleName =
                                        syntaxImport.moduleName |> Elm.Syntax.Node.value

                                    exposes :
                                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes :
                                            FastSet.Set String
                                        , variants :
                                            FastDict.Dict String { valueCount : Int }
                                        }
                                    exposes =
                                        case syntaxImport.exposingList of
                                            Nothing ->
                                                { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                    FastSet.empty
                                                , variants = FastDict.empty
                                                }

                                            Just (Elm.Syntax.Node.Node _ syntaxExposing) ->
                                                case moduleExposes |> FastDict.get importModuleName of
                                                    Nothing ->
                                                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                            FastSet.empty
                                                        , variants = FastDict.empty
                                                        }

                                                    Just moduleExposedNames ->
                                                        case syntaxExposing of
                                                            Elm.Syntax.Exposing.All _ ->
                                                                { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                    moduleExposedNames.enumTypesExposingVariants
                                                                        |> FastDict.foldl
                                                                            (\enumTypeName _ soFar ->
                                                                                soFar |> FastSet.insert enumTypeName
                                                                            )
                                                                            moduleExposedNames.valueOrFunctionOrTypeAliasNames
                                                                , variants =
                                                                    moduleExposedNames.enumTypesExposingVariants
                                                                        |> FastDict.foldl
                                                                            (\_ variantNames soFar -> FastDict.union variantNames soFar)
                                                                            FastDict.empty
                                                                }

                                                            Elm.Syntax.Exposing.Explicit explicitEposes ->
                                                                explicitEposes
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ expose) soFar ->
                                                                            case expose of
                                                                                Elm.Syntax.Exposing.InfixExpose _ ->
                                                                                    soFar

                                                                                Elm.Syntax.Exposing.TypeOrAliasExpose name ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                                                                            |> FastSet.insert name
                                                                                    , variants = soFar.variants
                                                                                    }

                                                                                Elm.Syntax.Exposing.FunctionExpose name ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                                                                            |> FastSet.insert name
                                                                                    , variants = soFar.variants
                                                                                    }

                                                                                Elm.Syntax.Exposing.TypeExpose enumTypeExpose ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                                                                            |> FastSet.insert enumTypeExpose.name
                                                                                    , variants =
                                                                                        case enumTypeExpose.open of
                                                                                            Nothing ->
                                                                                                soFar.variants

                                                                                            Just _ ->
                                                                                                case
                                                                                                    moduleExposedNames.enumTypesExposingVariants
                                                                                                        |> FastDict.get enumTypeExpose.name
                                                                                                of
                                                                                                    Nothing ->
                                                                                                        soFar.variants

                                                                                                    Just enumTypeDeclared ->
                                                                                                        FastDict.union
                                                                                                            soFar.variants
                                                                                                            enumTypeDeclared
                                                                                    }
                                                                        )
                                                                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                            FastSet.empty
                                                                        , variants = FastDict.empty
                                                                        }
                                in
                                { moduleName = importModuleName
                                , alias =
                                    syntaxImport.moduleAlias
                                        |> Maybe.map
                                            (\(Elm.Syntax.Node.Node _ syntaxAlias) ->
                                                syntaxAlias |> String.join "."
                                            )
                                , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                    exposes.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                , exposedVariants =
                                    exposes.variants
                                }
                            )
                   )
                |> importsCombine
    in
    importsNormal
        |> List.foldl
            (\syntaxImport soFar ->
                let
                    importedModuleMembers :
                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes :
                            FastSet.Set String
                        , variants : FastDict.Dict String { valueCount : Int }
                        }
                    importedModuleMembers =
                        case moduleExposes |> FastDict.get syntaxImport.moduleName of
                            Nothing ->
                                { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                    FastSet.empty
                                , variants = FastDict.empty
                                }

                            Just moduleExposedNames ->
                                { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                    moduleExposedNames.enumTypesExposingVariants
                                        |> FastDict.foldl
                                            (\enumTypeName _ namesSoFar ->
                                                namesSoFar
                                                    |> FastSet.insert enumTypeName
                                            )
                                            moduleExposedNames.valueOrFunctionOrTypeAliasNames
                                , variants =
                                    moduleExposedNames.enumTypesExposingVariants
                                        |> FastDict.foldl
                                            (\_ variantNames variantsSoFar ->
                                                FastDict.union variantNames variantsSoFar
                                            )
                                            FastDict.empty
                                }
                in
                moduleContextMerge
                    (moduleContextMerge
                        { variantLookup =
                            syntaxImport.exposedVariants
                                |> FastDict.foldl
                                    (\variantName variantInfo dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], variantName )
                                                { moduleOrigin = syntaxImport.moduleName
                                                , valueCount = variantInfo.valueCount
                                                }
                                    )
                                    FastDict.empty
                        , valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            syntaxImport.exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes
                                |> FastSet.foldl
                                    (\expose dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], expose )
                                                syntaxImport.moduleName
                                    )
                                    FastDict.empty
                        }
                        (case syntaxImport.alias of
                            Nothing ->
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    importedModuleMembers.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                        |> FastSet.foldl
                                            (\exposeFromImportedModule dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        syntaxImport.moduleName
                                            )
                                            FastDict.empty
                                , variantLookup =
                                    importedModuleMembers.variants
                                        |> FastDict.foldl
                                            (\exposeFromImportedModule variantInfo dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        { moduleOrigin = syntaxImport.moduleName
                                                        , valueCount = variantInfo.valueCount
                                                        }
                                            )
                                            FastDict.empty
                                }

                            Just importAlias ->
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    importedModuleMembers.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                        |> FastSet.foldl
                                            (\exposeFromImportedModule dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( [ importAlias ], exposeFromImportedModule )
                                                        syntaxImport.moduleName
                                            )
                                            FastDict.empty
                                , variantLookup =
                                    importedModuleMembers.variants
                                        |> FastDict.foldl
                                            (\exposeFromImportedModule variantInfo dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( [ importAlias ], exposeFromImportedModule )
                                                        { moduleOrigin = syntaxImport.moduleName
                                                        , valueCount = variantInfo.valueCount
                                                        }
                                            )
                                            FastDict.empty
                                }
                        )
                    )
                    soFar
            )
            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                FastDict.empty
            , variantLookup =
                FastDict.empty
            }


moduleContextMerge : ModuleContext -> ModuleContext -> ModuleContext
moduleContextMerge a b =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
        FastDict.union
            a.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
            b.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
    , variantLookup =
        FastDict.union
            a.variantLookup
            b.variantLookup
    }


implicitImports :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
implicitImports =
    [ { moduleName = [ "Basics" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "EQ", { valueCount = 0 } )
                , ( "LT", { valueCount = 0 } )
                , ( "GT", { valueCount = 0 } )
                , ( "True", { valueCount = 0 } )
                , ( "False", { valueCount = 0 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList
                [ "Int"
                , "Float"
                , "toFloat"
                , "round"
                , "floor"
                , "ceiling"
                , "truncate"
                , "max"
                , "min"
                , "compare"
                , "Order"
                , "Bool"
                , "not"
                , "xor"
                , "modBy"
                , "remainderBy"
                , "negate"
                , "abs"
                , "clamp"
                , "sqrt"
                , "logBase"
                , "e"
                , "pi"
                , "cos"
                , "sin"
                , "tan"
                , "acos"
                , "asin"
                , "atan"
                , "atan2"
                , "degrees"
                , "radians"
                , "turns"
                , "toPolar"
                , "fromPolar"
                , "isNaN"
                , "isInfinite"
                , "identity"
                , "always"
                , "Never"
                , "never"
                ]
      }
    , { moduleName = [ "List" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "List" ]
      }
    , { moduleName = [ "Maybe" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "Just", { valueCount = 1 } )
                , ( "Nothing", { valueCount = 0 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Maybe" ]
      }
    , { moduleName = [ "Result" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "Ok", { valueCount = 1 } )
                , ( "Err", { valueCount = 1 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Result" ]
      }
    , { moduleName = [ "String" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "String" ]
      }
    , { moduleName = [ "Char" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Char" ]
      }
    , { moduleName = [ "Tuple" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.empty
      }
    , { moduleName = [ "Debug" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.empty
      }
    , { moduleName = [ "Platform" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Program" ]
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , alias = Just "Cmd"
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Cmd" ]
      }
    , { moduleName = [ "Platform", "Sub" ]
      , alias = Just "Sub"
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Sub" ]
      }
    ]


importsCombine :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
importsCombine syntaxImports =
    importsCombineFrom [] syntaxImports


importsCombineFrom :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
importsCombineFrom soFar syntaxImports =
    case syntaxImports of
        [] ->
            soFar

        [ onlyImport ] ->
            onlyImport :: soFar

        import0 :: import1 :: import2Up ->
            if import0.moduleName == import1.moduleName then
                importsCombineFrom soFar
                    (importsMerge import0 import1
                        :: import2Up
                    )

            else
                importsCombineFrom
                    (import0 :: soFar)
                    (import1 :: import2Up)


importsMerge :
    { moduleName : Elm.Syntax.ModuleName.ModuleName
    , alias : Maybe String
    , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
        FastSet.Set String
    , exposedVariants :
        FastDict.Dict String { valueCount : Int }
    }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
importsMerge earlier later =
    { moduleName = earlier.moduleName
    , alias =
        case earlier.alias of
            Just alias ->
                alias |> Just

            Nothing ->
                later.alias
    , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
        FastSet.union
            earlier.exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes
            later.exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes
    , exposedVariants =
        FastDict.union
            earlier.exposedVariants
            later.exposedVariants
    }


grainTypeContainedRecords :
    GrainType
    ->
        FastSet.Set
            -- sorted field names
            (List String)
grainTypeContainedRecords grainType =
    -- IGNORE TCO
    case grainType of
        GrainTypeVariable _ ->
            FastSet.empty

        GrainTypeFunction typeFunction ->
            FastSet.union
                (typeFunction.input
                    |> listMapToFastSetsAndUnify grainTypeContainedRecords
                )
                (typeFunction.output |> grainTypeContainedRecords)

        GrainTypeTuple typeTuple ->
            (typeTuple.part0 :: typeTuple.part1 :: typeTuple.part2Up)
                |> listMapToFastSetsAndUnify
                    grainTypeContainedRecords

        GrainTypeConstruct typeConstruct ->
            -- TODO instead extract fields from the original Elm.Syntax.TypeAnnotation
            {- GrainTypeRecord fields ->
               FastSet.insert
                   (fields |> FastDict.keys)
                   (fields
                       |> fastDictMapToFastSetAndUnify
                           grainTypeContainedRecords
                   )
            -}
            FastSet.union
                (if typeConstruct.name |> String.startsWith "Generated_" then
                    FastSet.singleton
                        (typeConstruct.name
                            |> String.split "_"
                            |> List.drop 1
                            |> List.filter (\field -> field /= "")
                            |> List.map variableNameDisambiguateFromGrainKeywords
                            |> List.sort
                        )

                 else
                    FastSet.empty
                )
                (typeConstruct.arguments
                    |> listMapToFastSetsAndUnify
                        grainTypeContainedRecords
                )


grainExpressionContainedConstructedRecords :
    GrainExpression
    ->
        FastSet.Set
            -- sorted field names
            (List String)
grainExpressionContainedConstructedRecords syntaxExpression =
    -- IGNORE TCO
    case syntaxExpression of
        GrainExpressionRecord fields ->
            FastSet.singleton
                (fields |> FastDict.keys)

        expressionNotRecord ->
            expressionNotRecord
                |> grainExpressionSubs
                |> listMapToFastSetsAndUnify
                    grainExpressionContainedConstructedRecords


grainExpressionContainedLocalReferences :
    GrainExpression
    -> FastSet.Set String
grainExpressionContainedLocalReferences syntaxExpression =
    -- IGNORE TCO
    case syntaxExpression of
        GrainExpressionReference reference ->
            case reference.moduleOrigin of
                Just _ ->
                    FastSet.empty

                Nothing ->
                    FastSet.singleton reference.name

        expressionNotRecord ->
            expressionNotRecord
                |> grainExpressionSubs
                |> listMapToFastSetsAndUnify
                    grainExpressionContainedLocalReferences


{-| All surface-level child [expression](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression)s.
-}
grainExpressionSubs : GrainExpression -> List GrainExpression
grainExpressionSubs grainExpression =
    case grainExpression of
        GrainExpressionCall call ->
            call.called
                :: call.argument0
                :: call.argument1Up

        GrainExpressionList elements ->
            elements

        GrainExpressionRecord fields ->
            fields |> FastDict.values

        GrainExpressionRecordUpdate recordUpdate ->
            recordUpdate.fields |> FastDict.values

        GrainExpressionIfElse ifThenElse ->
            [ ifThenElse.condition
            , ifThenElse.onTrue
            , ifThenElse.onFalse
            ]

        GrainExpressionWithLetDeclarations letIn ->
            List.foldr
                (\declaration soFar ->
                    case declaration of
                        GrainLetDeclarationValueOrFunction letValueOrFunction ->
                            letValueOrFunction.result :: soFar

                        GrainLetDestructuring letDestructuring ->
                            letDestructuring.expression :: soFar
                )
                [ letIn.result ]
                (letIn.declaration0 :: letIn.declaration1Up)

        GrainExpressionMatch matchWith ->
            matchWith.matched
                :: matchWith.case0.result
                :: (matchWith.case1Up |> List.map .result)

        GrainExpressionLambda lambda ->
            [ lambda.result ]

        GrainExpressionTuple parts ->
            parts.part0
                :: parts.part1
                :: parts.part2Up

        GrainExpressionRecordAccess recordAccess ->
            [ recordAccess.record ]

        GrainExpressionFloat _ ->
            []

        GrainExpressionString _ ->
            []

        GrainExpressionChar _ ->
            []

        GrainExpressionReference _ ->
            []


enumTypeDeclaration :
    ModuleContext
    -> Elm.Syntax.Type.Type
    ->
        Result
            String
            { name : String
            , parameters : List String
            , variants : FastDict.Dict String (List GrainType)
            }
enumTypeDeclaration moduleOriginLookup syntaxEnumType =
    Result.map
        (\variants ->
            { name =
                syntaxEnumType.name
                    |> Elm.Syntax.Node.value
            , parameters =
                syntaxEnumType.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            parameter |> variableNameDisambiguateFromGrainKeywords
                        )
            , variants = variants |> FastDict.fromList
            }
        )
        (syntaxEnumType.constructors
            |> listMapAndCombineOk
                (\(Elm.Syntax.Node.Node _ syntaxVariant) ->
                    Result.map
                        (\values ->
                            ( syntaxVariant.name |> Elm.Syntax.Node.value
                            , values
                            )
                        )
                        (syntaxVariant.arguments
                            |> listMapAndCombineOk
                                (\value ->
                                    value |> type_ moduleOriginLookup
                                )
                        )
                )
        )


grainTypeParametersToString : List String -> String
grainTypeParametersToString grainTypeParameters =
    case grainTypeParameters of
        [] ->
            ""

        parameter0 :: parameter1Up ->
            "<"
                ++ ((parameter0 :: parameter1Up)
                        |> String.join ", "
                   )
                ++ ">"


printGrainEnumTypeDeclaration :
    { name : String
    , parameters : List String
    , variants : FastDict.Dict String (List GrainType)
    }
    -> Print
printGrainEnumTypeDeclaration grainEnumType =
    Print.exactly
        (grainEnumType.name
            ++ (grainEnumType.parameters
                    |> grainTypeParametersToString
               )
            ++ " {"
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (grainEnumType.variants
                            |> FastDict.toList
                            |> Print.listMapAndIntersperseAndFlatten
                                (\( name, values ) ->
                                    printGrainVariant
                                        { name = name
                                        , values = values
                                        }
                                )
                                Print.linebreakIndented
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


printGrainVariant : { name : String, values : List GrainType } -> Print
printGrainVariant grainVariant =
    Print.exactly grainVariant.name
        |> Print.followedBy
            (case grainVariant.values of
                [] ->
                    Print.empty

                value0 :: value1Up ->
                    let
                        valuePrints : List Print
                        valuePrints =
                            (value0 :: value1Up)
                                |> List.map printGrainTypeParenthesizedIfSpaceSeparated

                        fullLineSpread : Print.LineSpread
                        fullLineSpread =
                            valuePrints
                                |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    in
                    Print.exactly "("
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.emptyOrLinebreakIndented fullLineSpread
                                    |> Print.followedBy
                                        (valuePrints
                                            |> Print.listMapAndIntersperseAndFlatten
                                                (\valuePrint ->
                                                    Print.withIndentAtNextMultipleOf4 valuePrint
                                                )
                                                (Print.exactly ","
                                                    |> Print.followedBy
                                                        (Print.spaceOrLinebreakIndented fullLineSpread)
                                                )
                                        )
                                )
                            )
                        |> Print.followedBy
                            (Print.emptyOrLinebreakIndented fullLineSpread)
                        |> Print.followedBy (Print.exactly ")")
            )
        |> Print.followedBy (Print.exactly ",")


typeAliasDeclaration :
    ModuleContext
    -> Elm.Syntax.TypeAlias.TypeAlias
    ->
        Result
            String
            { name : String
            , parameters : List String
            , type_ : GrainType
            }
typeAliasDeclaration moduleOriginLookup syntaxTypeAlias =
    Result.map
        (\aliasedType ->
            { name =
                syntaxTypeAlias.name
                    |> Elm.Syntax.Node.value
            , parameters =
                syntaxTypeAlias.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            parameter |> variableNameDisambiguateFromGrainKeywords
                        )
            , type_ = aliasedType
            }
        )
        (syntaxTypeAlias.typeAnnotation
            |> type_ moduleOriginLookup
        )


printGrainTypeAliasDeclaration :
    { name : String
    , parameters : List String
    , type_ : GrainType
    }
    -> Print
printGrainTypeAliasDeclaration grainTypeAliasDeclaration =
    Print.exactly
        (grainTypeAliasDeclaration.name
            ++ (grainTypeAliasDeclaration.parameters
                    |> grainTypeParametersToString
               )
            ++ " ="
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (grainTypeAliasDeclaration.type_
                            |> printGrainTypeNotParenthesized
                        )
                )
            )


printGrainRecordTypeDeclaration : List String -> Print
printGrainRecordTypeDeclaration grainRecordFields =
    Print.exactly
        ("record "
            ++ generatedGrainRecordTypeAliasName grainRecordFields
            ++ (grainRecordFields
                    |> grainTypeParametersToString
               )
            ++ " "
        )
        |> Print.followedBy
            (case grainRecordFields of
                [] ->
                    Print.exactly "{}"

                grainRecordField0 :: grainRecordField1Up ->
                    let
                        fieldsPrint : Print
                        fieldsPrint =
                            (grainRecordField0 :: grainRecordField1Up)
                                |> Print.listMapAndIntersperseAndFlatten
                                    (\fieldName ->
                                        Print.exactly (fieldName ++ ": " ++ fieldName)
                                    )
                                    (Print.exactly ","
                                        |> Print.followedBy Print.linebreakIndented
                                    )

                        fullLineSpread : Print.LineSpread
                        fullLineSpread =
                            fieldsPrint |> Print.lineSpread
                    in
                    Print.exactly "{"
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.spaceOrLinebreakIndented fullLineSpread
                                    |> Print.followedBy fieldsPrint
                                )
                            )
                        |> Print.followedBy
                            (Print.spaceOrLinebreakIndented fullLineSpread)
                        |> Print.followedBy printExactlyCurlyClosing
            )


type_ :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Result String GrainType
type_ moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxType) =
    -- IGNORE TCO
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Unit ->
            Ok grainTypeVoid

        Elm.Syntax.TypeAnnotation.GenericType variable ->
            Ok (GrainTypeVariable (variable |> variableNameDisambiguateFromGrainKeywords))

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ reference) typedArguments ->
            let
                ( qualification, name ) =
                    reference
            in
            case moduleOriginLookup.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup |> FastDict.get reference of
                Nothing ->
                    Err
                        ("could not find module origin of the type reference "
                            ++ qualifiedToString
                                { qualification = qualification
                                , name = name
                                }
                        )

                Just moduleOrigin ->
                    Result.map
                        (\arguments ->
                            let
                                grainReference : { moduleOrigin : Maybe String, name : String }
                                grainReference =
                                    case
                                        { moduleOrigin = moduleOrigin
                                        , name = name
                                        }
                                            |> referenceToCoreGrain
                                    of
                                        Just coreGrain ->
                                            coreGrain

                                        Nothing ->
                                            { moduleOrigin = Nothing
                                            , name =
                                                { moduleOrigin = moduleOrigin
                                                , name = name
                                                }
                                                    |> referenceToGrainName
                                                    |> stringFirstCharToUpper
                                            }
                            in
                            GrainTypeConstruct
                                { moduleOrigin = grainReference.moduleOrigin
                                , name = grainReference.name
                                , arguments = arguments
                                }
                        )
                        (typedArguments
                            |> listMapAndCombineOk
                                (\argument -> argument |> type_ moduleOriginLookup)
                        )

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    Ok grainTypeVoid

                [ inParens ] ->
                    type_ moduleOriginLookup inParens

                [ tuplePart0, tuplePart1 ] ->
                    Result.map2
                        (\part0 part1 ->
                            GrainTypeTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = []
                                }
                        )
                        (tuplePart0 |> type_ moduleOriginLookup)
                        (tuplePart1 |> type_ moduleOriginLookup)

                [ tuplePart0, tuplePart1, tuplePart2 ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            GrainTypeTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = [ part2 ]
                                }
                        )
                        (tuplePart0 |> type_ moduleOriginLookup)
                        (tuplePart1 |> type_ moduleOriginLookup)
                        (tuplePart2 |> type_ moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.TypeAnnotation.Record recordFields ->
            Result.map
                (\fields ->
                    let
                        fieldAsFastDict : FastDict.Dict String GrainType
                        fieldAsFastDict =
                            FastDict.fromList fields
                    in
                    GrainTypeConstruct
                        { moduleOrigin = Nothing
                        , name =
                            generatedGrainRecordTypeAliasName
                                (fieldAsFastDict |> FastDict.keys)
                        , arguments =
                            fieldAsFastDict
                                |> FastDict.values
                        }
                )
                (recordFields
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ name, valueNode )) ->
                            Result.map
                                (\value ->
                                    ( name |> variableNameDisambiguateFromGrainKeywords
                                    , value
                                    )
                                )
                                (valueNode |> type_ moduleOriginLookup)
                        )
                )

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputNode outputNode ->
            Result.map2
                (\input0 outputExpandedReverse ->
                    -- TODO why reverse ??
                    case outputExpandedReverse |> List.reverse of
                        output :: inputLastTo1 ->
                            GrainTypeFunction
                                { input = input0 :: (inputLastTo1 |> List.reverse)
                                , output = output
                                }

                        -- too lazy to make it non-empty
                        [] ->
                            input0
                )
                (inputNode |> type_ moduleOriginLookup)
                (outputNode
                    |> typeExpandFunctionOutputReverse
                    |> listMapAndCombineOk
                        (\partOfOutput ->
                            type_ moduleOriginLookup partOfOutput
                        )
                )

        Elm.Syntax.TypeAnnotation.GenericRecord _ _ ->
            Err "extensible record types are not supported"


typeExpandFunctionOutputReverse :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeExpandFunctionOutputReverse typeNode =
    typeExpandFunctionOutputIntoReverse [] typeNode
        |> List.reverse


typeExpandFunctionOutputIntoReverse :
    List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeExpandFunctionOutputIntoReverse soFarReverse (Elm.Syntax.Node.Node fullRange syntaxType) =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputNode outputNode ->
            typeExpandFunctionOutputIntoReverse
                (inputNode :: soFarReverse)
                outputNode

        otherType ->
            Elm.Syntax.Node.Node fullRange otherType :: soFarReverse


grainTypeVoid : GrainType
grainTypeVoid =
    GrainTypeConstruct
        { moduleOrigin = Nothing
        , name = "Void"
        , arguments = []
        }


printGrainTypeNotParenthesized : GrainType -> Print
printGrainTypeNotParenthesized grainType =
    -- IGNORE TCO
    case grainType of
        GrainTypeVariable variable ->
            Print.exactly variable

        GrainTypeConstruct typeConstruct ->
            printGrainTypeConstruct typeConstruct

        GrainTypeTuple parts ->
            printGrainTypeTuple parts

        GrainTypeFunction typeFunction ->
            printGrainTypeFunction typeFunction


printGrainTypeFunction :
    { input : List GrainType, output : GrainType }
    -> Print
printGrainTypeFunction typeFunction =
    let
        inputPrints : List Print
        inputPrints =
            typeFunction.input
                |> List.map printGrainTypeNotParenthesized

        outputPrint : Print
        outputPrint =
            printGrainTypeParenthesizedIfSpaceSeparated
                typeFunction.output

        inputLineSpread : Print.LineSpread
        inputLineSpread =
            inputPrints
                |> Print.lineSpreadListMapAndCombine
                    Print.lineSpread

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            inputLineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        outputPrint |> Print.lineSpread
                    )
    in
    Print.exactly "("
        |> Print.followedBy
            (inputPrints
                |> Print.listMapAndIntersperseAndFlatten
                    (\inputPrint -> Print.withIndentIncreasedBy 1 inputPrint)
                    (Print.exactly ","
                        |> Print.followedBy
                            (Print.spaceOrLinebreakIndented inputLineSpread)
                    )
            )
        |> Print.followedBy (Print.exactly ")")
        |> Print.followedBy
            (Print.exactly " =>")
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented fullLineSpread)
        |> Print.followedBy outputPrint


printGrainTypeTuple :
    { part0 : GrainType, part1 : GrainType, part2Up : List GrainType }
    -> Print
printGrainTypeTuple parts =
    let
        part0Print : Print
        part0Print =
            parts.part0 |> printGrainTypeNotParenthesized

        part1Print : Print
        part1Print =
            parts.part1 |> printGrainTypeNotParenthesized

        part2UpPrints : List Print
        part2UpPrints =
            parts.part2Up
                |> List.map printGrainTypeNotParenthesized
    in
    Print.exactly "( "
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                ((part0Print :: part1Print :: part2UpPrints)
                    |> Print.listIntersperseAndFlatten
                        (Print.exactly ","
                            |> Print.followedBy Print.linebreakIndented
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly ")")


printGrainTypeConstruct :
    { moduleOrigin : Maybe String
    , name : String
    , arguments : List GrainType
    }
    -> Print
printGrainTypeConstruct typeConstruct =
    let
        referencePrint : Print
        referencePrint =
            Print.exactly
                (grainReferenceToString
                    { moduleOrigin = typeConstruct.moduleOrigin
                    , name = typeConstruct.name
                    }
                )
    in
    case typeConstruct.arguments of
        [] ->
            referencePrint

        argument0 :: argument1Up ->
            let
                argumentPrints : List Print
                argumentPrints =
                    (argument0 :: argument1Up)
                        |> List.map printGrainTypeNotParenthesized

                fullLineSpread : Print.LineSpread
                fullLineSpread =
                    argumentPrints
                        |> Print.lineSpreadListMapAndCombine Print.lineSpread
            in
            referencePrint
                |> Print.followedBy (Print.exactly "<")
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.emptyOrLinebreakIndented fullLineSpread
                            |> Print.followedBy
                                (argumentPrints
                                    |> Print.listIntersperseAndFlatten
                                        (Print.exactly ","
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented fullLineSpread)
                                        )
                                )
                        )
                    )
                |> Print.followedBy
                    (Print.emptyOrLinebreakIndented fullLineSpread)
                |> Print.followedBy
                    (Print.exactly ">")


typeIsSpaceSeparated : GrainType -> Bool
typeIsSpaceSeparated grainType =
    case grainType of
        GrainTypeVariable _ ->
            False

        GrainTypeConstruct _ ->
            False

        GrainTypeTuple _ ->
            False

        GrainTypeFunction _ ->
            True


printGrainTypeParenthesizedIfSpaceSeparated : GrainType -> Print
printGrainTypeParenthesizedIfSpaceSeparated grainType =
    if grainType |> typeIsSpaceSeparated then
        printParenthesized
            { opening = "("
            , closing = ")"
            , inner = grainType |> printGrainTypeNotParenthesized
            }

    else
        grainType |> printGrainTypeNotParenthesized


grainReferenceToString :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
grainReferenceToString reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleName ->
            moduleName
                ++ "."
                ++ reference.name


qualifiedToString :
    { qualification : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
qualifiedToString reference =
    case reference.qualification of
        [] ->
            reference.name

        qualificationPart0 :: qualificationPart1Up ->
            ((qualificationPart0 :: qualificationPart1Up)
                |> String.join "."
            )
                ++ "."
                ++ reference.name


stringFirstCharToLower : String -> String
stringFirstCharToLower string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toLower headChar) tailString


stringFirstCharToUpper : String -> String
stringFirstCharToUpper string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toUpper headChar) tailString


stringFirstCharIsUpper : String -> Bool
stringFirstCharIsUpper string =
    case string |> String.uncons of
        Nothing ->
            False

        Just ( headChar, _ ) ->
            headChar |> Char.isUpper


printGrainString : String -> Print
printGrainString stringContent =
    let
        singleDoubleQuotedStringContentEscaped : String
        singleDoubleQuotedStringContentEscaped =
            stringContent
                |> String.foldl
                    (\contentChar soFar ->
                        soFar ++ singleDoubleQuotedStringCharToEscaped contentChar ++ ""
                    )
                    ""
    in
    Print.exactly ("\"" ++ singleDoubleQuotedStringContentEscaped ++ "\"")


singleDoubleQuotedStringCharToEscaped : Char -> String
singleDoubleQuotedStringCharToEscaped character =
    case character of
        '"' ->
            "\\\""

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '$' ->
            "\\$"

        '\u{000D}' ->
            "\\r"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


intToHexString : Int -> String
intToHexString int =
    -- IGNORE TCO
    if int < 16 then
        unsafeHexDigitIntToString int

    else
        intToHexString (int // 16)
            ++ unsafeHexDigitIntToString (int |> Basics.remainderBy 16)
            ++ ""


unsafeHexDigitIntToString : Int -> String
unsafeHexDigitIntToString int =
    case int of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "A"

        11 ->
            "B"

        12 ->
            "C"

        13 ->
            "D"

        14 ->
            "E"

        -- 15
        _ ->
            "F"


characterHex : Char -> String
characterHex character =
    String.toUpper
        (intToHexString (Char.toCode character)
            |> String.padLeft 8 '0'
        )


characterIsNotPrint : Char -> Bool
characterIsNotPrint character =
    if
        -- Unicode.getCategory is very expensive so we shortcut if at all possible
        charIsLatinAlphaNumOrUnderscoreFast character
            || (character == ' ')
            || (character == '.')
            || (character == '!')
            || (character == '?')
            || (character == '-')
            || (character == ':')
    then
        False

    else
        case Unicode.getCategory character of
            Nothing ->
                True

            Just category ->
                case category of
                    Unicode.SeparatorLine ->
                        True

                    Unicode.SeparatorParagraph ->
                        True

                    Unicode.OtherControl ->
                        True

                    Unicode.OtherFormat ->
                        True

                    Unicode.OtherSurrogate ->
                        True

                    Unicode.OtherPrivateUse ->
                        True

                    Unicode.OtherNotAssigned ->
                        True

                    Unicode.LetterUppercase ->
                        False

                    Unicode.LetterLowercase ->
                        False

                    Unicode.LetterTitlecase ->
                        False

                    Unicode.MarkNonSpacing ->
                        False

                    Unicode.MarkSpacingCombining ->
                        False

                    Unicode.MarkEnclosing ->
                        False

                    Unicode.NumberDecimalDigit ->
                        False

                    Unicode.NumberLetter ->
                        False

                    Unicode.NumberOther ->
                        False

                    Unicode.SeparatorSpace ->
                        True

                    Unicode.LetterModifier ->
                        False

                    Unicode.LetterOther ->
                        False

                    Unicode.PunctuationConnector ->
                        False

                    Unicode.PunctuationDash ->
                        False

                    Unicode.PunctuationOpen ->
                        False

                    Unicode.PunctuationClose ->
                        False

                    Unicode.PunctuationInitialQuote ->
                        False

                    Unicode.PunctuationFinalQuote ->
                        False

                    Unicode.PunctuationOther ->
                        False

                    Unicode.SymbolMath ->
                        False

                    Unicode.SymbolCurrency ->
                        False

                    Unicode.SymbolModifier ->
                        False

                    Unicode.SymbolOther ->
                        False


charLiteral : Char -> String
charLiteral charContent =
    "'"
        ++ quotedCharToEscaped charContent
        ++ "'"


quotedCharToEscaped : Char -> String
quotedCharToEscaped character =
    case character of
        '\'' ->
            "\\'"

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '\u{000D}' ->
            "\\r"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


charCodeIsLower : Int -> Bool
charCodeIsLower code =
    0x61 <= code && code <= 0x7A


charCodeIsUpper : Int -> Bool
charCodeIsUpper code =
    code <= 0x5A && 0x41 <= code


charCodeIsDigit : Int -> Bool
charCodeIsDigit code =
    code <= 0x39 && 0x30 <= code


charIsLatinAlphaNumOrUnderscoreFast : Char -> Bool
charIsLatinAlphaNumOrUnderscoreFast c =
    let
        code : Int
        code =
            Char.toCode c
    in
    charCodeIsLower code
        || charCodeIsUpper code
        || charCodeIsDigit code
        || -- (c == '_')
           (code == 95)


pattern :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        Result
            String
            { pattern : GrainPattern
            , introducedVariables : FastSet.Set String
            }
pattern moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.AllPattern ->
            Ok
                { pattern = GrainPatternIgnore
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.UnitPattern ->
            Ok
                { pattern = GrainPatternIgnore
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.CharPattern charValue ->
            Ok
                { pattern = GrainPatternChar charValue
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.StringPattern stringValue ->
            Ok
                { pattern = GrainPatternString stringValue
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.IntPattern intValue ->
            Ok
                { pattern = GrainPatternNumber (intValue |> Basics.toFloat)
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.HexPattern intValue ->
            Ok
                { pattern = GrainPatternNumber (intValue |> Basics.toFloat)
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "float pattern is invalid syntax"

        Elm.Syntax.Pattern.VarPattern variableName ->
            let
                disambiguatedVariableName : String
                disambiguatedVariableName =
                    variableName |> variableNameDisambiguateFromGrainKeywords
            in
            Ok
                { pattern =
                    GrainPatternVariable disambiguatedVariableName
                , introducedVariables =
                    FastSet.singleton disambiguatedVariableName
                }

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            pattern moduleOriginLookup inParens

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [] ->
                    -- should be covered by UnitPattern
                    Ok
                        { pattern = GrainPatternIgnore
                        , introducedVariables = FastSet.empty
                        }

                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    pattern moduleOriginLookup inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 ->
                            { pattern =
                                GrainPatternTuple
                                    { part0 = part0.pattern
                                    , part1 = part1.pattern
                                    , part2Up = []
                                    }
                            , introducedVariables =
                                FastSet.union
                                    part0.introducedVariables
                                    part0.introducedVariables
                            }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            { pattern =
                                GrainPatternTuple
                                    { part0 = part0.pattern
                                    , part1 = part1.pattern
                                    , part2Up = [ part2.pattern ]
                                    }
                            , introducedVariables =
                                FastSet.union
                                    part0.introducedVariables
                                    (FastSet.union
                                        part0.introducedVariables
                                        part1.introducedVariables
                                    )
                            }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)
                        (part2Node |> pattern moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    -- invalid syntax
                    Err "too many tuple parts"

        Elm.Syntax.Pattern.RecordPattern fields ->
            let
                fieldNames : FastSet.Set String
                fieldNames =
                    fields
                        |> listMapAndToFastSet
                            (\(Elm.Syntax.Node.Node _ fieldName) ->
                                fieldName |> variableNameDisambiguateFromGrainKeywords
                            )
            in
            Ok
                { pattern = GrainPatternRecordInexhaustive fieldNames
                , introducedVariables = fieldNames
                }

        Elm.Syntax.Pattern.UnConsPattern headPatternNode tailPatternNode ->
            let
                tailExpanded :
                    { initialElements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
                    , tail : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
                    }
                tailExpanded =
                    tailPatternNode |> patternConsExpand
            in
            resultAndThen3
                (\initialElement0 initialElement1Up tailGrainPattern ->
                    let
                        introducedVariables : FastSet.Set String
                        introducedVariables =
                            FastSet.union
                                initialElement0.introducedVariables
                                (FastSet.union
                                    (initialElement1Up |> listMapToFastSetsAndUnify .introducedVariables)
                                    tailGrainPattern.introducedVariables
                                )
                    in
                    Ok
                        { pattern =
                            GrainPatternListCons
                                { initialElement0 = initialElement0.pattern
                                , initialElement1Up =
                                    initialElement1Up
                                        |> List.map .pattern
                                , tailVariable = Just tailGrainPattern.pattern
                                }
                        , introducedVariables = introducedVariables
                        }
                )
                (headPatternNode |> pattern moduleOriginLookup)
                (tailExpanded.initialElements
                    |> listMapAndCombineOk
                        (\initialElement -> initialElement |> pattern moduleOriginLookup)
                )
                (tailExpanded.tail |> pattern moduleOriginLookup)

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            Result.map
                (\elements ->
                    { pattern =
                        GrainPatternListExact (elements |> List.map .pattern)
                    , introducedVariables =
                        elements
                            |> listMapToFastSetsAndUnify .introducedVariables
                    }
                )
                (elementPatterns
                    |> listMapAndCombineOk
                        (\element -> element |> pattern moduleOriginLookup)
                )

        Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            Result.map2
                (\values reference ->
                    { pattern =
                        GrainPatternVariant
                            { moduleOrigin = reference.moduleOrigin
                            , name = reference.name
                            , values = values |> List.map .pattern
                            }
                    , introducedVariables =
                        values
                            |> listMapToFastSetsAndUnify .introducedVariables
                    }
                )
                (argumentPatterns
                    |> listMapAndCombineOk
                        (\argument -> argument |> pattern moduleOriginLookup)
                )
                (case moduleOriginLookup.variantLookup |> FastDict.get ( syntaxQualifiedNameRef.moduleName, syntaxQualifiedNameRef.name ) of
                    Nothing ->
                        Err
                            ("could not find origin choice type for the variant "
                                ++ qualifiedToString
                                    { qualification = syntaxQualifiedNameRef.moduleName
                                    , name = syntaxQualifiedNameRef.name
                                    }
                            )

                    Just variantInfo ->
                        Ok
                            (case { moduleOrigin = variantInfo.moduleOrigin, name = syntaxQualifiedNameRef.name } |> referenceToCoreGrain of
                                Just grainReference ->
                                    grainReference

                                Nothing ->
                                    { moduleOrigin = Nothing
                                    , name =
                                        referenceToGrainName
                                            { moduleOrigin = variantInfo.moduleOrigin
                                            , name = syntaxQualifiedNameRef.name
                                            }
                                            |> stringFirstCharToUpper
                                    }
                            )
                )

        Elm.Syntax.Pattern.AsPattern aliasedPatternNode (Elm.Syntax.Node.Node _ variable) ->
            Result.map
                (\aliasedPattern ->
                    let
                        variableDisambiguated : String
                        variableDisambiguated =
                            variable |> variableNameDisambiguateFromGrainKeywords
                    in
                    { pattern =
                        GrainPatternAs
                            { pattern = aliasedPattern.pattern
                            , variable = variableDisambiguated
                            }
                    , introducedVariables =
                        aliasedPattern.introducedVariables
                            |> FastSet.insert variableDisambiguated
                    }
                )
                (aliasedPatternNode |> pattern moduleOriginLookup)


printGrainPatternListCons :
    { initialElement0 : GrainPattern
    , initialElement1Up : List GrainPattern
    , tailVariable : Maybe GrainPattern
    }
    -> Print
printGrainPatternListCons syntaxCons =
    Print.exactly "["
        |> Print.FollowedBy
            (syntaxCons.initialElement0
                :: syntaxCons.initialElement1Up
                |> Print.listMapAndIntersperseAndFlatten
                    (\initialElement ->
                        printGrainPatternNotParenthesized initialElement
                    )
                    (Print.exactly ", ")
            )
        |> Print.followedBy
            (Print.exactly ", ...")
        |> Print.followedBy
            (case syntaxCons.tailVariable of
                Nothing ->
                    Print.exactly "_"

                Just tailVariable ->
                    printGrainPatternParenthesizedIfSpaceSeparated tailVariable
            )
        |> Print.followedBy
            (Print.exactly "]")


patternConsExpand :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        { initialElements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        , tail : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        }
patternConsExpand patternNode =
    patternConsExpandFromInitialElementsReverse [] patternNode


patternConsExpandFromInitialElementsReverse :
    List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        { initialElements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        , tail : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        }
patternConsExpandFromInitialElementsReverse initialElementsSoFarReverse (Elm.Syntax.Node.Node fulRange syntaxPattern) =
    case syntaxPattern of
        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            patternConsExpandFromInitialElementsReverse
                (headPattern :: initialElementsSoFarReverse)
                tailPattern

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            patternConsExpandFromInitialElementsReverse initialElementsSoFarReverse
                inParens

        Elm.Syntax.Pattern.AllPattern ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange Elm.Syntax.Pattern.AllPattern
            }

        Elm.Syntax.Pattern.UnitPattern ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange Elm.Syntax.Pattern.UnitPattern
            }

        Elm.Syntax.Pattern.CharPattern char ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.CharPattern char)
            }

        Elm.Syntax.Pattern.StringPattern string ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.StringPattern string)
            }

        Elm.Syntax.Pattern.IntPattern int ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.IntPattern int)
            }

        Elm.Syntax.Pattern.HexPattern int ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.HexPattern int)
            }

        Elm.Syntax.Pattern.FloatPattern float ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.FloatPattern float)
            }

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ part0, part1 ] ->
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern [ part0, part1 ])
                    }

                [ part0, part1, part2 ] ->
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern [ part0, part1, part2 ])
                    }

                [] ->
                    -- should be handled by UnitPattern
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail = Elm.Syntax.Node.Node fulRange Elm.Syntax.Pattern.UnitPattern
                    }

                [ inParens ] ->
                    -- should be handled by ParenthesizedPattern
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern [ inParens ])
                    }

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- should be handled by ParenthesizedPattern
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail =
                        Elm.Syntax.Node.Node fulRange
                            (Elm.Syntax.Pattern.TuplePattern (part0 :: part1 :: part2 :: part3 :: part4Up))
                    }

        Elm.Syntax.Pattern.RecordPattern fields ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.RecordPattern fields)
            }

        Elm.Syntax.Pattern.ListPattern elements ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.ListPattern elements)
            }

        Elm.Syntax.Pattern.VarPattern variableName ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.VarPattern variableName)
            }

        Elm.Syntax.Pattern.NamedPattern reference parameters ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.NamedPattern reference parameters)
            }

        Elm.Syntax.Pattern.AsPattern aliasedPattern aliasName ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.AsPattern aliasedPattern aliasName)
            }


referenceToCoreGrain :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    ->
        Maybe
            { moduleOrigin : Maybe String
            , name : String
            }
referenceToCoreGrain reference =
    case reference.moduleOrigin of
        [ "Basics" ] ->
            case reference.name of
                "identity" ->
                    Just { moduleOrigin = Nothing, name = "identity" }

                "always" ->
                    Just { moduleOrigin = Nothing, name = "basics_always" }

                "compare" ->
                    Just { moduleOrigin = Nothing, name = "basics_compare" }

                "max" ->
                    Just { moduleOrigin = Just "Number", name = "max" }

                "min" ->
                    Just { moduleOrigin = Just "Number", name = "min" }

                "Order" ->
                    Just { moduleOrigin = Nothing, name = "Basics_Order" }

                "LT" ->
                    Just { moduleOrigin = Nothing, name = "Basics_LT" }

                "EQ" ->
                    Just { moduleOrigin = Nothing, name = "Basics_EQ" }

                "GT" ->
                    Just { moduleOrigin = Nothing, name = "Basics_GT" }

                "Bool" ->
                    Just { moduleOrigin = Nothing, name = "Bool" }

                "True" ->
                    Just { moduleOrigin = Nothing, name = "true" }

                "False" ->
                    Just { moduleOrigin = Nothing, name = "false" }

                "not" ->
                    Just { moduleOrigin = Nothing, name = "basics_not" }

                "xor" ->
                    Just { moduleOrigin = Nothing, name = "basics_neq" }

                "Int" ->
                    Just { moduleOrigin = Nothing, name = "Number" }

                "Float" ->
                    Just { moduleOrigin = Nothing, name = "Number" }

                "e" ->
                    Just { moduleOrigin = Just "Number", name = "e" }

                "pi" ->
                    Just { moduleOrigin = Just "Number", name = "pi" }

                "ceiling" ->
                    Just { moduleOrigin = Just "Number", name = "ceil" }

                "floor" ->
                    Just { moduleOrigin = Just "Number", name = "floor" }

                "round" ->
                    Just { moduleOrigin = Just "Number", name = "round" }

                "truncate" ->
                    Just { moduleOrigin = Just "Number", name = "trunc" }

                "negate" ->
                    Just { moduleOrigin = Just "Number", name = "neg" }

                "abs" ->
                    Just { moduleOrigin = Just "Number", name = "abs" }

                "toFloat" ->
                    Just { moduleOrigin = Nothing, name = "identity" }

                "isNaN" ->
                    Just { moduleOrigin = Just "Number", name = "isNaN" }

                "isInfinite" ->
                    Just { moduleOrigin = Just "Number", name = "isInfinite" }

                "remainderBy" ->
                    Just { moduleOrigin = Nothing, name = "basics_remainderBy" }

                "modBy" ->
                    Just { moduleOrigin = Nothing, name = "basics_modBy" }

                "sin" ->
                    Just { moduleOrigin = Just "Number", name = "sin" }

                "cos" ->
                    Just { moduleOrigin = Just "Number", name = "cos" }

                "tan" ->
                    Just { moduleOrigin = Just "Number", name = "tan" }

                "asin" ->
                    Just { moduleOrigin = Just "Number", name = "asin" }

                "acos" ->
                    Just { moduleOrigin = Just "Number", name = "acos" }

                "atan" ->
                    Just { moduleOrigin = Just "Number", name = "atan" }

                "atan2" ->
                    Just { moduleOrigin = Just "Number", name = "atan2" }

                "sqrt" ->
                    Just { moduleOrigin = Just "Number", name = "sqrt" }

                _ ->
                    Nothing

        [ "String" ] ->
            case reference.name of
                "String" ->
                    Just { moduleOrigin = Nothing, name = "String" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "String", name = "isEmpty" }

                "length" ->
                    Just { moduleOrigin = Just "String", name = "length" }

                "append" ->
                    Just { moduleOrigin = Just "String", name = "concat" }

                "trim" ->
                    Just { moduleOrigin = Just "String", name = "trim" }

                "trimLeft" ->
                    Just { moduleOrigin = Just "String", name = "trimStart" }

                "trimRight" ->
                    Just { moduleOrigin = Just "String", name = "trimEnd" }

                "left" ->
                    Just { moduleOrigin = Nothing, name = "string_left" }

                "right" ->
                    Just { moduleOrigin = Nothing, name = "string_right" }

                "dropLeft" ->
                    Just { moduleOrigin = Nothing, name = "string_dropLeft" }

                "dropRight" ->
                    Just { moduleOrigin = Nothing, name = "string_dropRight" }

                "padLeft" ->
                    Just { moduleOrigin = Nothing, name = "string_padLeft" }

                "padRight" ->
                    Just { moduleOrigin = Nothing, name = "string_padRight" }

                "toList" ->
                    Just { moduleOrigin = Nothing, name = "string_toList" }

                "join" ->
                    Just { moduleOrigin = Just "List", name = "join" }

                "filter" ->
                    Just { moduleOrigin = Nothing, name = "string_filter" }

                "any" ->
                    Just { moduleOrigin = Nothing, name = "string_any" }

                "all" ->
                    Just { moduleOrigin = Nothing, name = "string_all" }

                "map" ->
                    Just { moduleOrigin = Just "String", name = "map" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "string_repeat" }

                "replace" ->
                    Just { moduleOrigin = Just "String", name = "replaceAll" }

                "lines" ->
                    Just { moduleOrigin = Nothing, name = "string_lines" }

                "startsWith" ->
                    Just { moduleOrigin = Just "String", name = "startsWith" }

                "endsWith" ->
                    Just { moduleOrigin = Just "String", name = "endsWith" }

                "toInt" ->
                    Just { moduleOrigin = Nothing, name = "string_toInt" }

                "toFloat" ->
                    Just { moduleOrigin = Nothing, name = "string_toFloat" }

                "fromInt" ->
                    Just { moduleOrigin = Nothing, name = "toString" }

                "fromFloat" ->
                    Just { moduleOrigin = Nothing, name = "toString" }

                "fromChar" ->
                    Just { moduleOrigin = Just "Char", name = "toString" }

                "cons" ->
                    Just { moduleOrigin = Nothing, name = "string_cons" }

                "slice" ->
                    Just { moduleOrigin = Nothing, name = "string_slice" }

                "split" ->
                    Just { moduleOrigin = Just "String", name = "split" }

                "contains" ->
                    Just { moduleOrigin = Just "String", name = "contains" }

                "reverse" ->
                    Just { moduleOrigin = Just "String", name = "reverse" }

                "toLower" ->
                    Just { moduleOrigin = Just "String", name = "toAsciiLowercase" }

                "toUpper" ->
                    Just { moduleOrigin = Just "String", name = "toAsciUppercase" }

                _ ->
                    Nothing

        [ "Char" ] ->
            case reference.name of
                "Char" ->
                    Just { moduleOrigin = Nothing, name = "Char" }

                "toCode" ->
                    Just { moduleOrigin = Just "Char", name = "code" }

                "fromCode" ->
                    Just { moduleOrigin = Nothing, name = "char_fromCode" }

                "toLower" ->
                    Just { moduleOrigin = Just "Char.Ascii", name = "toLowercase" }

                "toUpper" ->
                    Just { moduleOrigin = Just "Char.Ascii", name = "toUppercase" }

                _ ->
                    Nothing

        [ "List" ] ->
            case reference.name of
                "List" ->
                    Just { moduleOrigin = Nothing, name = "List" }

                "singleton" ->
                    Just { moduleOrigin = Nothing, name = "list_singleton" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "List", name = "isEmpty" }

                "length" ->
                    Just { moduleOrigin = Just "List", name = "length" }

                "member" ->
                    Just { moduleOrigin = Just "List", name = "mem" }

                "sum" ->
                    Just { moduleOrigin = Nothing, name = "list_sum" }

                "minimum" ->
                    Just { moduleOrigin = Nothing, name = "list_minimum" }

                "product" ->
                    Just { moduleOrigin = Nothing, name = "list_product" }

                "append" ->
                    Just { moduleOrigin = Just "List", name = "append" }

                "concat" ->
                    Just { moduleOrigin = Just "List", name = "flatten" }

                "reverse" ->
                    Just { moduleOrigin = Just "List", name = "reverse" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "list_repeat" }

                "all" ->
                    Just { moduleOrigin = Just "List", name = "every" }

                "any" ->
                    Just { moduleOrigin = Just "List", name = "some" }

                "filter" ->
                    Just { moduleOrigin = Just "List", name = "filter" }

                "filterMap" ->
                    Just { moduleOrigin = Just "List", name = "filterMap" }

                "map" ->
                    Just { moduleOrigin = Just "List", name = "map" }

                "map2" ->
                    Just { moduleOrigin = Just "List", name = "zipWith" }

                "zip" ->
                    Just { moduleOrigin = Just "List", name = "zip" }

                "unzip" ->
                    Just { moduleOrigin = Just "List", name = "unzip" }

                "concatMap" ->
                    Just { moduleOrigin = Just "List", name = "flatMap" }

                "sort" ->
                    Just { moduleOrigin = Nothing, name = "list_sort" }

                "sortWith" ->
                    Just { moduleOrigin = Nothing, name = "list_sortWith" }

                "range" ->
                    Just { moduleOrigin = Nothing, name = "list_range" }

                "take" ->
                    Just { moduleOrigin = Just "List", name = "take" }

                "drop" ->
                    Just { moduleOrigin = Just "List", name = "drop" }

                "intersperse" ->
                    Just { moduleOrigin = Nothing, name = "list_intersperse" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "list_foldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "list_foldr" }

                "head" ->
                    Just { moduleOrigin = Just "List", name = "head" }

                "tail" ->
                    Just { moduleOrigin = Just "List", name = "tail" }

                _ ->
                    Nothing

        [ "Parser" ] ->
            case reference.name of
                -- refers to either a type or variant
                "Problem" ->
                    Just { moduleOrigin = Nothing, name = "Parser_Problem" }

                "Expecting" ->
                    Just { moduleOrigin = Nothing, name = "Parser_Expecting" }

                "ExpectingInt" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingInt" }

                "ExpectingHex" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingHex" }

                "ExpectingOctal" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingOctal" }

                "ExpectingBinary" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingBinary" }

                "ExpectingFloat" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingFloat" }

                "ExpectingNumber" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingNumber" }

                "ExpectingVariable" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingVariable" }

                "ExpectingSymbol" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingSymbol" }

                "ExpectingKeyword" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingKeyword" }

                "ExpectingEnd" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingEnd" }

                "UnexpectedChar" ->
                    Just { moduleOrigin = Nothing, name = "Parser_UnexpectedChar" }

                "BadRepeat" ->
                    Just { moduleOrigin = Nothing, name = "Parser_BadRepeat" }

                _ ->
                    Nothing

        [ "Maybe" ] ->
            case reference.name of
                "Maybe" ->
                    Just { moduleOrigin = Nothing, name = "Option" }

                "Nothing" ->
                    Just { moduleOrigin = Nothing, name = "None" }

                "Just" ->
                    Just { moduleOrigin = Nothing, name = "Some" }

                _ ->
                    Nothing

        _ ->
            Nothing


referenceToGrainName :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
referenceToGrainName reference =
    (reference.moduleOrigin
        |> String.concat
        |> stringFirstCharToLower
    )
        ++ "_"
        ++ reference.name


printGrainPatternNotParenthesized : GrainPattern -> Print
printGrainPatternNotParenthesized grainPattern =
    -- IGNORE TCO
    case grainPattern of
        GrainPatternIgnore ->
            printExactlyUnderscore

        GrainPatternNumber floatValue ->
            Print.exactly (grainNumberLiteralToString floatValue)

        GrainPatternChar charValue ->
            Print.exactly (charLiteral charValue)

        GrainPatternString string ->
            printGrainString string

        GrainPatternVariable name ->
            Print.exactly name

        GrainPatternListCons grainPatternListCons ->
            printGrainPatternListCons grainPatternListCons

        GrainPatternListExact elements ->
            printGrainPatternListExact elements

        GrainPatternRecordInexhaustive recordInexhaustiveFieldNames ->
            Print.exactly "{ "
                |> Print.followedBy
                    (recordInexhaustiveFieldNames
                        |> FastSet.toList
                        |> Print.listMapAndIntersperseAndFlatten
                            Print.exactly
                            (Print.exactly ", ")
                    )
                |> Print.followedBy (Print.exactly ", _ }")

        GrainPatternVariant patternVariant ->
            Print.exactly patternVariant.name
                |> Print.followedBy
                    (case patternVariant.values of
                        [] ->
                            Print.empty

                        variantValue0 :: variantValue1Up ->
                            Print.exactly "("
                                |> Print.followedBy
                                    ((variantValue0 :: variantValue1Up)
                                        |> Print.listMapAndIntersperseAndFlatten
                                            printGrainPatternNotParenthesized
                                            (Print.exactly ", ")
                                    )
                                |> Print.followedBy (Print.exactly ")")
                    )

        GrainPatternAs patternAs ->
            printGrainPatternAs patternAs

        GrainPatternTuple parts ->
            Print.exactly "( "
                |> Print.followedBy
                    ((parts.part0 :: parts.part1 :: parts.part2Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            printGrainPatternNotParenthesized
                            (Print.exactly ", ")
                    )
                |> Print.followedBy (Print.exactly " )")


printGrainPatternListExact : List GrainPattern -> Print
printGrainPatternListExact elements =
    case elements of
        [] ->
            Print.exactly "[]"

        element0 :: element1Up ->
            Print.exactly "[ "
                |> Print.followedBy
                    ((element0 :: element1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\elementNode ->
                                Print.withIndentIncreasedBy 2
                                    (printGrainPatternNotParenthesized elementNode)
                            )
                            (Print.exactly ", ")
                    )
                |> Print.followedBy (Print.exactly " ]")


printGrainPatternAs :
    { variable : String
    , pattern : GrainPattern
    }
    -> Print
printGrainPatternAs syntaxAs =
    printGrainPatternParenthesizedIfSpaceSeparated
        syntaxAs.pattern
        |> Print.followedBy
            (Print.exactly (" as " ++ syntaxAs.variable))


printGrainExpressionRecord : FastDict.Dict String GrainExpression -> Print
printGrainExpressionRecord syntaxRecordFields =
    if syntaxRecordFields |> FastDict.isEmpty then
        Print.exactly "{}"

    else
        let
            fieldsPrint : Print
            fieldsPrint =
                Print.withIndentIncreasedBy 2
                    (syntaxRecordFields
                        |> FastDict.toList
                        |> Print.listMapAndIntersperseAndFlatten
                            (\( fieldName, fieldValue ) ->
                                let
                                    fieldValuePrint : Print
                                    fieldValuePrint =
                                        printGrainExpressionNotParenthesized fieldValue
                                in
                                Print.exactly (fieldName ++ ":")
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.spaceOrLinebreakIndented
                                                (fieldValuePrint |> Print.lineSpread)
                                                |> Print.followedBy
                                                    fieldValuePrint
                                            )
                                        )
                            )
                            (Print.exactly ","
                                |> Print.followedBy Print.linebreakIndented
                            )
                    )
        in
        printExactlyCurlyOpeningSpace
            |> Print.followedBy fieldsPrint
            |> Print.followedBy
                (Print.spaceOrLinebreakIndented
                    (fieldsPrint |> Print.lineSpread)
                )
            |> Print.followedBy printExactlyCurlyClosing


printParenthesized : { opening : String, closing : String, inner : Print } -> Print
printParenthesized config =
    Print.exactly config.opening
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                config.inner
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented
                (config.inner |> Print.lineSpread)
            )
        |> Print.followedBy (Print.exactly config.closing)


{-| Transpile a list of [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)s
across multiple modules to value, function and [`GrainTypeDeclaration`](#GrainTypeDeclaration)s.
Declarations that use unsupported stuff like parser kernel code (directly or indirectly)
will not be present in the final declarations.
Their errors can be found alongside the valid transpiled declarations.

The given list of files must also include files from used dependencies
including `elm/core`.

-}
modules :
    List Elm.Syntax.File.File
    ->
        { errors : List String
        , declarations :
            { valuesAndFunctions :
                FastDict.Dict
                    String
                    { parameters : List GrainPattern
                    , result : GrainExpression
                    , type_ : Maybe GrainType
                    }
            , typeAliases :
                FastDict.Dict
                    String
                    { parameters : List String
                    , type_ : GrainType
                    }
            , recordTypes : FastSet.Set (List String)
            , enumTypes :
                FastDict.Dict
                    String
                    { parameters : List String
                    , variants : FastDict.Dict String (List GrainType)
                    }
            }
        }
modules syntaxDeclarationsIncludingOverwrittenOnes =
    let
        syntaxModules : List Elm.Syntax.File.File
        syntaxModules =
            syntaxDeclarationsIncludingOverwrittenOnes
                |> List.filter
                    (\syntaxModule ->
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Basics" ] ->
                                False

                            [ "Array" ] ->
                                False

                            -- https://github.com/elm/core/blob/1.0.5/src/Elm/JsArray.elm
                            [ "Elm", "JsArray" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Char" ] ->
                                False

                            [ "String" ] ->
                                False

                            [ "List" ] ->
                                False

                            [ "Dict" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
                                False

                            [ "Regex" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            -- https://github.com/elm/browser/blob/master/src/Browser/AnimationManager.elm
                            [ "Browser", "AnimationManager" ] ->
                                False

                            -- https://github.com/elm/browser/tree/master/src/Debugger
                            [ "Debugger", "Expando" ] ->
                                False

                            [ "Debugger", "History" ] ->
                                False

                            [ "Debugger", "Main" ] ->
                                False

                            [ "Debugger", "Metadata" ] ->
                                False

                            [ "Debugger", "Overlay" ] ->
                                False

                            [ "Debugger", "Report" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )

        moduleMembers :
            FastDict.Dict
                Elm.Syntax.ModuleName.ModuleName
                { valueOrFunctionOrTypeAliasNames : FastSet.Set String
                , enumTypesExposingVariants :
                    FastDict.Dict String (FastDict.Dict String { valueCount : Int })
                }
        moduleMembers =
            syntaxDeclarationsIncludingOverwrittenOnes
                |> List.filter
                    (\syntaxModule ->
                        -- remove those modules we don't have a replacement for, yet
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Array" ] ->
                                False

                            -- https://github.com/elm/core/blob/1.0.5/src/Elm/JsArray.elm
                            [ "Elm", "JsArray" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
                                False

                            [ "Regex" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            -- https://github.com/elm/browser/blob/master/src/Browser/AnimationManager.elm
                            [ "Browser", "AnimationManager" ] ->
                                False

                            -- https://github.com/elm/browser/tree/master/src/Debugger
                            [ "Debugger", "Expando" ] ->
                                False

                            [ "Debugger", "History" ] ->
                                False

                            [ "Debugger", "Main" ] ->
                                False

                            [ "Debugger", "Metadata" ] ->
                                False

                            [ "Debugger", "Overlay" ] ->
                                False

                            [ "Debugger", "Report" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )
                |> List.foldl
                    (\syntaxModule acrossModulesSoFar ->
                        acrossModulesSoFar
                            |> FastDict.insert
                                (syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName
                                )
                                (syntaxModule.declarations
                                    |> List.foldl
                                        (\(Elm.Syntax.Node.Node _ declaration) membersSoFar ->
                                            case declaration of
                                                Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert
                                                                (syntaxValueOrFunctionDeclaration.declaration
                                                                    |> Elm.Syntax.Node.value
                                                                    |> .name
                                                                    |> Elm.Syntax.Node.value
                                                                )
                                                    , enumTypesExposingVariants =
                                                        membersSoFar.enumTypesExposingVariants
                                                    }

                                                Elm.Syntax.Declaration.CustomTypeDeclaration syntaxEnumTypeDeclaration ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                    , enumTypesExposingVariants =
                                                        membersSoFar.enumTypesExposingVariants
                                                            |> FastDict.insert
                                                                (syntaxEnumTypeDeclaration.name |> Elm.Syntax.Node.value)
                                                                (syntaxEnumTypeDeclaration.constructors
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ variant) variantNamesSoFar ->
                                                                            variantNamesSoFar
                                                                                |> FastDict.insert
                                                                                    (variant.name
                                                                                        |> Elm.Syntax.Node.value
                                                                                    )
                                                                                    { valueCount =
                                                                                        variant.arguments |> List.length
                                                                                    }
                                                                        )
                                                                        FastDict.empty
                                                                )
                                                    }

                                                Elm.Syntax.Declaration.AliasDeclaration typeAlias ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert
                                                                (typeAlias.name
                                                                    |> Elm.Syntax.Node.value
                                                                )
                                                    , enumTypesExposingVariants =
                                                        membersSoFar.enumTypesExposingVariants
                                                    }

                                                Elm.Syntax.Declaration.PortDeclaration _ ->
                                                    -- not supported
                                                    membersSoFar

                                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                                    membersSoFar

                                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                                    -- invalid syntax
                                                    membersSoFar
                                        )
                                        { valueOrFunctionOrTypeAliasNames = FastSet.empty
                                        , enumTypesExposingVariants = FastDict.empty
                                        }
                                )
                    )
                    FastDict.empty

        grainDeclarationsWithoutExtraRecordTypeAliases :
            { errors : List String
            , declarations :
                { valuesAndFunctions :
                    FastDict.Dict
                        String
                        { parameters : List GrainPattern
                        , result : GrainExpression
                        , type_ : Maybe GrainType
                        }
                , typeAliases :
                    FastDict.Dict
                        String
                        { parameters : List String
                        , type_ : GrainType
                        }
                , enumTypes :
                    FastDict.Dict
                        String
                        { parameters : List String
                        , variants : FastDict.Dict String (List GrainType)
                        }
                }
            }
        grainDeclarationsWithoutExtraRecordTypeAliases =
            syntaxModules
                |> List.foldr
                    (\syntaxModule soFarAcrossModules ->
                        let
                            moduleName : Elm.Syntax.ModuleName.ModuleName
                            moduleName =
                                syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName

                            createdModuleContext : ModuleContext
                            createdModuleContext =
                                moduleContextMerge
                                    (syntaxModule.imports |> importsToModuleContext moduleMembers)
                                    (case moduleMembers |> FastDict.get moduleName of
                                        Nothing ->
                                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                                FastDict.empty
                                            , variantLookup = FastDict.empty
                                            }

                                        Just moduleLocalNames ->
                                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                                FastSet.union
                                                    moduleLocalNames.valueOrFunctionOrTypeAliasNames
                                                    (moduleLocalNames.enumTypesExposingVariants
                                                        |> FastDict.foldl
                                                            (\enumTypeName _ soFar ->
                                                                soFar |> FastSet.insert enumTypeName
                                                            )
                                                            FastSet.empty
                                                    )
                                                    |> FastSet.foldl
                                                        (\name soFar ->
                                                            soFar
                                                                |> FastDict.insert ( [], name )
                                                                    moduleName
                                                        )
                                                        FastDict.empty
                                            , variantLookup =
                                                moduleLocalNames.enumTypesExposingVariants
                                                    |> FastDict.foldl
                                                        (\_ variantNames soFarAcrossEnumTypes ->
                                                            variantNames
                                                                |> FastDict.foldl
                                                                    (\name info soFar ->
                                                                        soFar
                                                                            |> FastDict.insert ( [], name )
                                                                                { moduleOrigin = moduleName
                                                                                , valueCount = info.valueCount
                                                                                }
                                                                    )
                                                                    soFarAcrossEnumTypes
                                                        )
                                                        FastDict.empty
                                            }
                                    )
                        in
                        syntaxModule.declarations
                            |> List.foldr
                                (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                                    case declaration of
                                        Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                            case syntaxValueOrFunctionDeclaration |> valueOrFunctionDeclaration createdModuleContext of
                                                Ok grainValueOrFunctionDeclaration ->
                                                    { errors = soFar.errors
                                                    , declarations =
                                                        { typeAliases = soFar.declarations.typeAliases
                                                        , enumTypes = soFar.declarations.enumTypes
                                                        , valuesAndFunctions =
                                                            soFar.declarations.valuesAndFunctions
                                                                |> FastDict.insert
                                                                    ({ moduleOrigin = moduleName
                                                                     , name = grainValueOrFunctionDeclaration.name
                                                                     }
                                                                        |> referenceToGrainName
                                                                    )
                                                                    { parameters = grainValueOrFunctionDeclaration.parameters
                                                                    , result = grainValueOrFunctionDeclaration.result
                                                                    , type_ = grainValueOrFunctionDeclaration.type_
                                                                    }
                                                        }
                                                    }

                                                Err error ->
                                                    { declarations = soFar.declarations
                                                    , errors = error :: soFar.errors
                                                    }

                                        Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
                                            case syntaxTypeAliasDeclaration |> typeAliasDeclaration createdModuleContext of
                                                Ok grainTypeAliasDeclaration ->
                                                    { errors = soFar.errors
                                                    , declarations =
                                                        { valuesAndFunctions = soFar.declarations.valuesAndFunctions
                                                        , enumTypes = soFar.declarations.enumTypes
                                                        , typeAliases =
                                                            soFar.declarations.typeAliases
                                                                |> FastDict.insert
                                                                    ({ moduleOrigin = moduleName
                                                                     , name = grainTypeAliasDeclaration.name
                                                                     }
                                                                        |> referenceToGrainName
                                                                        |> stringFirstCharToUpper
                                                                    )
                                                                    { parameters = grainTypeAliasDeclaration.parameters
                                                                    , type_ = grainTypeAliasDeclaration.type_
                                                                    }
                                                        }
                                                    }

                                                Err error ->
                                                    { declarations = soFar.declarations
                                                    , errors = error :: soFar.errors
                                                    }

                                        Elm.Syntax.Declaration.CustomTypeDeclaration syntaxEnumTypeDeclaration ->
                                            case syntaxEnumTypeDeclaration.name |> Elm.Syntax.Node.value of
                                                "Maybe" ->
                                                    soFar

                                                _ ->
                                                    case syntaxEnumTypeDeclaration |> enumTypeDeclaration createdModuleContext of
                                                        Ok grainTypeAliasDeclaration ->
                                                            { errors = soFar.errors
                                                            , declarations =
                                                                { valuesAndFunctions = soFar.declarations.valuesAndFunctions
                                                                , typeAliases = soFar.declarations.typeAliases
                                                                , enumTypes =
                                                                    soFar.declarations.enumTypes
                                                                        |> FastDict.insert
                                                                            ({ moduleOrigin = moduleName
                                                                             , name = grainTypeAliasDeclaration.name
                                                                             }
                                                                                |> referenceToGrainName
                                                                                |> stringFirstCharToUpper
                                                                            )
                                                                            { parameters = grainTypeAliasDeclaration.parameters
                                                                            , variants =
                                                                                grainTypeAliasDeclaration.variants
                                                                                    |> FastDict.foldl
                                                                                        (\variantName maybeValue variantsSoFar ->
                                                                                            variantsSoFar
                                                                                                |> FastDict.insert
                                                                                                    ({ moduleOrigin = moduleName
                                                                                                     , name = variantName
                                                                                                     }
                                                                                                        |> referenceToGrainName
                                                                                                        |> stringFirstCharToUpper
                                                                                                    )
                                                                                                    maybeValue
                                                                                        )
                                                                                        FastDict.empty
                                                                            }
                                                                }
                                                            }

                                                        Err error ->
                                                            { declarations = soFar.declarations
                                                            , errors = error :: soFar.errors
                                                            }

                                        Elm.Syntax.Declaration.PortDeclaration _ ->
                                            soFar

                                        Elm.Syntax.Declaration.InfixDeclaration _ ->
                                            soFar

                                        Elm.Syntax.Declaration.Destructuring _ _ ->
                                            soFar
                                )
                                soFarAcrossModules
                    )
                    { errors = []
                    , declarations =
                        { valuesAndFunctions = FastDict.empty
                        , typeAliases = FastDict.empty
                        , enumTypes = FastDict.empty
                        }
                    }

        additionalRecordTypeAliases : FastSet.Set (List String)
        additionalRecordTypeAliases =
            FastSet.union
                (grainDeclarationsWithoutExtraRecordTypeAliases.declarations.valuesAndFunctions
                    |> fastDictMapToFastSetAndUnify
                        (\valueOrFunctionInfo ->
                            FastSet.union
                                (valueOrFunctionInfo.result
                                    |> grainExpressionContainedConstructedRecords
                                )
                                (case valueOrFunctionInfo.type_ of
                                    Nothing ->
                                        FastSet.empty

                                    Just valueOrFunctionType ->
                                        valueOrFunctionType |> grainTypeContainedRecords
                                )
                        )
                )
                (FastSet.union
                    (grainDeclarationsWithoutExtraRecordTypeAliases.declarations.typeAliases
                        |> fastDictMapToFastSetAndUnify
                            (\typeAliasInfo ->
                                typeAliasInfo.type_
                                    |> grainTypeContainedRecords
                            )
                    )
                    (grainDeclarationsWithoutExtraRecordTypeAliases.declarations.enumTypes
                        |> fastDictMapToFastSetAndUnify
                            (\enumTypeInfo ->
                                enumTypeInfo.variants
                                    |> fastDictMapToFastSetAndUnify
                                        (\values ->
                                            values
                                                |> listMapToFastSetsAndUnify
                                                    grainTypeContainedRecords
                                        )
                            )
                    )
                )
    in
    { declarations =
        { valuesAndFunctions =
            grainDeclarationsWithoutExtraRecordTypeAliases.declarations.valuesAndFunctions
                |> FastDict.map
                    (\_ valueOrFunctionInfo ->
                        { type_ = valueOrFunctionInfo.type_
                        , parameters = valueOrFunctionInfo.parameters
                        , result = valueOrFunctionInfo.result
                        }
                    )
        , enumTypes =
            grainDeclarationsWithoutExtraRecordTypeAliases.declarations.enumTypes
                |> FastDict.map
                    (\_ typeAliasInfo ->
                        { parameters = typeAliasInfo.parameters
                        , variants = typeAliasInfo.variants
                        }
                    )
        , recordTypes = additionalRecordTypeAliases
        , typeAliases =
            grainDeclarationsWithoutExtraRecordTypeAliases.declarations.typeAliases
                |> FastDict.map
                    (\_ typeAliasInfo ->
                        { parameters = typeAliasInfo.parameters
                        , type_ = typeAliasInfo.type_
                        }
                    )
        }
    , errors = grainDeclarationsWithoutExtraRecordTypeAliases.errors
    }


fastDictMapToFastSetAndUnify :
    (value -> FastSet.Set comparableFastSetElement)
    -> FastDict.Dict key_ value
    -> FastSet.Set comparableFastSetElement
fastDictMapToFastSetAndUnify valueToFastSet fastDict =
    fastDict
        |> FastDict.foldl
            (\_ value soFar ->
                FastSet.union
                    (value |> valueToFastSet)
                    soFar
            )
            FastSet.empty


generatedGrainRecordTypeAliasName : List String -> String
generatedGrainRecordTypeAliasName recordFields =
    "Generated_" ++ (recordFields |> String.join "_")


moduleHeaderName : Elm.Syntax.Module.Module -> Elm.Syntax.ModuleName.ModuleName
moduleHeaderName moduleHeader =
    case moduleHeader of
        Elm.Syntax.Module.NormalModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.PortModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.EffectModule header ->
            header.moduleName |> Elm.Syntax.Node.value


valueOrFunctionDeclaration :
    ModuleContext
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { name : String
            , parameters : List GrainPattern
            , result : GrainExpression
            , type_ : Maybe GrainType
            }
valueOrFunctionDeclaration moduleOriginLookup syntaxDeclarationValueOrFunction =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationValueOrFunction.declaration
                |> Elm.Syntax.Node.value
    in
    resultAndThen2
        (\parameters maybeType ->
            Result.map
                (\result ->
                    { name =
                        implementation.name
                            |> Elm.Syntax.Node.value
                    , type_ = maybeType
                    , parameters =
                        parameters
                            |> List.map .pattern
                    , result =
                        case parameters of
                            [] ->
                                result

                            parameter0 :: parameter1Up ->
                                GrainExpressionLambda
                                    { parameter0 = parameter0.pattern
                                    , parameter1Up = parameter1Up |> List.map .pattern
                                    , result = result
                                    }
                    }
                )
                (implementation.expression
                    |> expression
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            moduleOriginLookup.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                        , variantLookup =
                            moduleOriginLookup.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            parameters
                                |> listMapToFastSetsAndUnify .introducedVariables
                        }
                )
        )
        (implementation.arguments
            |> listMapAndCombineOk (\p -> p |> pattern moduleOriginLookup)
        )
        (case syntaxDeclarationValueOrFunction.signature of
            Nothing ->
                Ok Nothing

            Just (Elm.Syntax.Node.Node _ signature) ->
                Result.map Just
                    (signature.typeAnnotation
                        |> type_ moduleOriginLookup
                    )
        )


variableNameDisambiguateFromGrainKeywords : String -> String
variableNameDisambiguateFromGrainKeywords variableName =
    if grainKeywords |> FastSet.member variableName then
        variableName ++ "_"

    else
        variableName


grainKeywords : FastSet.Set String
grainKeywords =
    -- https://github.com/grain-lang/grain/blob/fbb08bf28683b05fb77a57af6a787907da070ef7/compiler/src/parsing/lexer.re#L252
    FastSet.fromList
        [ "and"
        , "as"
        , "assert"
        , "is"
        , "isnt"
        , "yield"
        , "macro"
        , "mut"
        , "let"
        , "match"
        , "assert"
        , "fail"
        , "exception"
        , "try"
        , "throw"
        , "for"
        , "while"
        , "wasm"
        , "foreign"
        , "primitive"
        , "module"
        , "record"
        , "enum"
        , "type"
        , "if"
        , "else"
        , "when"
        , "return"
        , "break"
        , "continue"
        , "include"
        , "open"
        , "from"
        , "except"
        , "abstract"
        , "provide"
        , "use"
        , "void"
        ]


expressionContextAddVariablesInScope :
    FastSet.Set String
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
expressionContextAddVariablesInScope additionalVariablesInScope context =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
        context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
    , variantLookup =
        context.variantLookup
    , variablesFromWithinDeclarationInScope =
        FastSet.union
            additionalVariablesInScope
            context.variablesFromWithinDeclarationInScope
    }


expression :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Result String GrainExpression
expression context (Elm.Syntax.Node.Node _ syntaxExpression) =
    -- IGNORE TCO
    case syntaxExpression of
        Elm.Syntax.Expression.UnitExpr ->
            Ok grainExpressionVoid

        Elm.Syntax.Expression.Integer intValue ->
            Ok (GrainExpressionFloat (intValue |> Basics.toFloat))

        Elm.Syntax.Expression.Hex intValue ->
            Ok (GrainExpressionFloat (intValue |> Basics.toFloat))

        Elm.Syntax.Expression.Floatable floatValue ->
            Ok (GrainExpressionFloat floatValue)

        Elm.Syntax.Expression.CharLiteral charValue ->
            Ok (GrainExpressionChar charValue)

        Elm.Syntax.Expression.Literal stringValue ->
            Ok (GrainExpressionString stringValue)

        Elm.Syntax.Expression.RecordAccessFunction fieldName ->
            let
                recordVariableName : String
                recordVariableName =
                    "generated_record"
            in
            Ok
                (GrainExpressionLambda
                    { parameter0 = GrainPatternVariable recordVariableName
                    , parameter1Up = []
                    , result =
                        GrainExpressionRecordAccess
                            { record =
                                GrainExpressionReference
                                    { moduleOrigin = Nothing
                                    , name = recordVariableName
                                    }
                            , field =
                                fieldName
                                    |> String.replace "." ""
                                    |> variableNameDisambiguateFromGrainKeywords
                            }
                    }
                )

        Elm.Syntax.Expression.Operator _ ->
            -- invalid syntax
            Err "operator is invalid syntax"

        Elm.Syntax.Expression.PrefixOperator operatorSymbol ->
            Result.map
                (\operationFunctionReference ->
                    GrainExpressionReference operationFunctionReference
                )
                (expressionOperatorToGrainFunctionReference operatorSymbol)

        Elm.Syntax.Expression.GLSLExpression _ ->
            Err "glsl not supported"

        Elm.Syntax.Expression.Application application ->
            case application of
                [] ->
                    Err "application without any parts is invalid"

                [ inParens ] ->
                    -- invalid syntax
                    expression context inParens

                calledNode :: argument0Node :: argument1UpNodes ->
                    Result.map3
                        (\called argument0 argument1Up ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument0
                                , argument1Up = argument1Up
                                }
                        )
                        (calledNode |> expression context)
                        (argument0Node |> expression context)
                        (argument1UpNodes
                            |> listMapAndCombineOk
                                (\argument -> argument |> expression context)
                        )

        Elm.Syntax.Expression.OperatorApplication operatorSymbol _ leftNode rightNode ->
            case operatorSymbol of
                "|>" ->
                    Result.map2
                        (\argument called ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                "<|" ->
                    Result.map2
                        (\called argument ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                "++" ->
                    Result.map2
                        (\left right ->
                            if
                                (left |> grainExpressionIsDefinitelyOfTypeString)
                                    || (right |> grainExpressionIsDefinitelyOfTypeString)
                            then
                                GrainExpressionCall
                                    { called =
                                        GrainExpressionReference
                                            { moduleOrigin = Just "String"
                                            , name = "concat"
                                            }
                                    , argument0 = left
                                    , argument1Up = [ right ]
                                    }

                            else
                                GrainExpressionCall
                                    { called =
                                        GrainExpressionReference
                                            { moduleOrigin = Just "List"
                                            , name = "append"
                                            }
                                    , argument0 = left
                                    , argument1Up = [ right ]
                                    }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                otherOperatorSymbol ->
                    Result.map3
                        (\operationFunctionReference left right ->
                            GrainExpressionCall
                                { called =
                                    GrainExpressionReference operationFunctionReference
                                , argument0 = left
                                , argument1Up = [ right ]
                                }
                        )
                        (expressionOperatorToGrainFunctionReference otherOperatorSymbol)
                        (leftNode |> expression context)
                        (rightNode |> expression context)

        Elm.Syntax.Expression.FunctionOrValue qualification name ->
            let
                asVariableFromWithinDeclaration : Maybe String
                asVariableFromWithinDeclaration =
                    case qualification of
                        _ :: _ ->
                            Nothing

                        [] ->
                            let
                                grainName : String
                                grainName =
                                    name |> variableNameDisambiguateFromGrainKeywords
                            in
                            if
                                context.variablesFromWithinDeclarationInScope
                                    |> FastSet.member grainName
                            then
                                Just grainName

                            else
                                Nothing
            in
            case asVariableFromWithinDeclaration of
                Just variableFromWithinDeclaration ->
                    Ok
                        (GrainExpressionReference
                            { moduleOrigin = Nothing
                            , name = variableFromWithinDeclaration
                            }
                        )

                Nothing ->
                    case context.variantLookup |> FastDict.get ( qualification, name ) of
                        Just variantInfo ->
                            let
                                reference : { moduleOrigin : Maybe String, name : String }
                                reference =
                                    case { moduleOrigin = variantInfo.moduleOrigin, name = name } |> referenceToCoreGrain of
                                        Just grainReference ->
                                            grainReference

                                        Nothing ->
                                            { moduleOrigin = Nothing
                                            , name =
                                                referenceToGrainName
                                                    { moduleOrigin = variantInfo.moduleOrigin
                                                    , name = name
                                                    }
                                                    |> stringFirstCharToUpper
                                            }
                            in
                            Ok
                                (case variantInfo.valueCount of
                                    0 ->
                                        GrainExpressionReference reference

                                    1 ->
                                        GrainExpressionReference reference

                                    valueCountAtLeast2 ->
                                        let
                                            generatedValueVariableReference : Int -> GrainExpression
                                            generatedValueVariableReference valueIndex =
                                                GrainExpressionReference
                                                    { moduleOrigin = Nothing
                                                    , name =
                                                        "generated_"
                                                            ++ (valueIndex |> String.fromInt)
                                                    }

                                            generatedValuePattern : Int -> GrainPattern
                                            generatedValuePattern valueIndex =
                                                GrainPatternVariable
                                                    ("generated_"
                                                        ++ (valueIndex |> String.fromInt)
                                                    )
                                        in
                                        GrainExpressionLambda
                                            { parameter0 = generatedValuePattern 0
                                            , parameter1Up =
                                                generatedValuePattern 1
                                                    :: (List.range 2 (valueCountAtLeast2 - 1)
                                                            |> List.map generatedValuePattern
                                                       )
                                            , result =
                                                GrainExpressionCall
                                                    { called = GrainExpressionReference reference
                                                    , argument0 =
                                                        generatedValueVariableReference 0
                                                    , argument1Up =
                                                        generatedValueVariableReference 1
                                                            :: (List.range 2 (valueCountAtLeast2 - 1)
                                                                    |> List.map generatedValueVariableReference
                                                               )
                                                    }
                                            }
                                )

                        Nothing ->
                            case context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup |> FastDict.get ( qualification, name ) of
                                Just moduleOrigin ->
                                    Ok
                                        (GrainExpressionReference
                                            (case { moduleOrigin = moduleOrigin, name = name } |> referenceToCoreGrain of
                                                Just grainReference ->
                                                    grainReference

                                                Nothing ->
                                                    { moduleOrigin = Nothing
                                                    , name =
                                                        -- TODO should be redundant because variant check
                                                        if name |> stringFirstCharIsUpper then
                                                            referenceToGrainName
                                                                { moduleOrigin = moduleOrigin
                                                                , name = name
                                                                }
                                                                |> stringFirstCharToUpper

                                                        else
                                                            referenceToGrainName
                                                                { moduleOrigin = moduleOrigin
                                                                , name = name
                                                                }
                                                    }
                                            )
                                        )

                                Nothing ->
                                    case qualification of
                                        qualificationPart0 :: qualificationPart1Up ->
                                            Err
                                                ("could not find module origin of the qualified reference "
                                                    ++ (((qualificationPart0 :: qualificationPart1Up) |> String.join ".")
                                                            ++ "."
                                                            ++ name
                                                       )
                                                )

                                        [] ->
                                            -- TODO convert to error
                                            Ok
                                                (GrainExpressionReference
                                                    { moduleOrigin = Nothing
                                                    , name = name |> variableNameDisambiguateFromGrainKeywords
                                                    }
                                                )

        Elm.Syntax.Expression.IfBlock conditionNode onTrueNode onFalseNode ->
            Result.map3
                (\condition onTrue onFalse ->
                    GrainExpressionIfElse
                        { condition = condition
                        , onTrue = onTrue
                        , onFalse = onFalse
                        }
                )
                (conditionNode |> expression context)
                (onTrueNode |> expression context)
                (onFalseNode |> expression context)

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            inParens |> expression context

        Elm.Syntax.Expression.Negation inNegationNode ->
            Result.map
                (\inNegation ->
                    GrainExpressionCall
                        { called =
                            GrainExpressionReference
                                { moduleOrigin = Just "Number", name = "neg" }
                        , argument0 = inNegation
                        , argument1Up = []
                        }
                )
                (inNegationNode |> expression context)

        Elm.Syntax.Expression.RecordAccess recordNode (Elm.Syntax.Node.Node _ fieldName) ->
            Result.map
                (\record ->
                    GrainExpressionRecordAccess
                        { record = record
                        , field =
                            fieldName
                                |> String.replace "." ""
                                |> variableNameDisambiguateFromGrainKeywords
                        }
                )
                (recordNode |> expression context)

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.UnitExpr
                    Ok grainExpressionVoid

                [ inParens ] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.ParenthesizedExpression
                    expression context inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 ->
                            GrainExpressionTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = []
                                }
                        )
                        (part0Node |> expression context)
                        (part1Node |> expression context)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            GrainExpressionTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = [ part2 ]
                                }
                        )
                        (part0Node |> expression context)
                        (part1Node |> expression context)
                        (part2Node |> expression context)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.Expression.ListExpr elementNodes ->
            Result.map (\elements -> GrainExpressionList elements)
                (elementNodes
                    |> listMapAndCombineOk
                        (\element -> element |> expression context)
                )

        Elm.Syntax.Expression.RecordExpr fieldNodes ->
            Result.map (\fields -> GrainExpressionRecord fields)
                (fieldNodes
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValueNode )) ->
                            Result.map
                                (\fieldValue ->
                                    ( fieldName
                                        |> variableNameDisambiguateFromGrainKeywords
                                    , fieldValue
                                    )
                                )
                                (fieldValueNode |> expression context)
                        )
                    |> Result.map FastDict.fromList
                )

        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node _ originalRecordVariable) fieldNodes ->
            Result.map
                (\fields ->
                    GrainExpressionRecordUpdate
                        { originalRecordVariable =
                            referenceToGrainName
                                { moduleOrigin =
                                    case context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup |> FastDict.get ( [], originalRecordVariable ) of
                                        Nothing ->
                                            []

                                        Just moduleOrigin ->
                                            moduleOrigin
                                , name = originalRecordVariable |> variableNameDisambiguateFromGrainKeywords
                                }
                        , fields = fields
                        }
                )
                (fieldNodes
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValueNode )) ->
                            Result.map
                                (\fieldValue ->
                                    ( fieldName, fieldValue )
                                )
                                (fieldValueNode |> expression context)
                        )
                    |> Result.map FastDict.fromList
                )

        Elm.Syntax.Expression.LambdaExpression lambda ->
            case lambda.args of
                [] ->
                    Err "lambda without parameters is invalid syntax"

                parameter0Node :: parameter1UpNodes ->
                    resultAndThen2
                        (\parameter0 parameter1Up ->
                            Result.map
                                (\result ->
                                    GrainExpressionLambda
                                        { parameter0 = parameter0.pattern
                                        , parameter1Up =
                                            parameter1Up |> List.map .pattern
                                        , result =
                                            result
                                        }
                                )
                                (lambda.expression
                                    |> expression
                                        (context
                                            |> expressionContextAddVariablesInScope
                                                (FastSet.union
                                                    parameter0.introducedVariables
                                                    (parameter1Up
                                                        |> listMapToFastSetsAndUnify .introducedVariables
                                                    )
                                                )
                                        )
                                )
                        )
                        (parameter0Node
                            |> pattern
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                                , variantLookup = context.variantLookup
                                }
                        )
                        (parameter1UpNodes
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter
                                        |> pattern
                                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                                            , variantLookup = context.variantLookup
                                            }
                                )
                        )

        Elm.Syntax.Expression.CaseExpression caseOf ->
            case caseOf.cases of
                [] ->
                    Err "case-of without cases invalid syntax"

                case0Node :: case1Node ->
                    Result.map3
                        (\matched case0 case1Up ->
                            GrainExpressionMatch
                                { matched = matched
                                , case0 = case0
                                , case1Up = case1Up
                                }
                        )
                        (caseOf.expression |> expression context)
                        (case0Node |> case_ context)
                        (case1Node
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter |> case_ context
                                )
                        )

        Elm.Syntax.Expression.LetExpression letIn ->
            case letIn.declarations of
                [] ->
                    Err "let-in without declarations is invalid syntax"

                declaration0Node :: declaration1UpNode ->
                    let
                        variablesForWholeLetIn : FastSet.Set String
                        variablesForWholeLetIn =
                            (declaration0Node :: declaration1UpNode)
                                |> listMapToFastSetsAndUnify
                                    (\(Elm.Syntax.Node.Node _ syntaxLetDeclaration) ->
                                        case syntaxLetDeclaration of
                                            Elm.Syntax.Expression.LetFunction letFunction ->
                                                FastSet.singleton
                                                    (letFunction.declaration
                                                        |> Elm.Syntax.Node.value
                                                        |> .name
                                                        |> Elm.Syntax.Node.value
                                                        |> variableNameDisambiguateFromGrainKeywords
                                                    )

                                            Elm.Syntax.Expression.LetDestructuring patternNode _ ->
                                                patternNode
                                                    |> patternBindings
                                                    |> listMapAndToFastSet
                                                        variableNameDisambiguateFromGrainKeywords
                                    )
                    in
                    Result.map3
                        (\declaration0 declaration1Up result ->
                            GrainExpressionWithLetDeclarations
                                { declaration0 = declaration0
                                , declaration1Up = declaration1Up
                                , result = result
                                }
                        )
                        (declaration0Node
                            |> letDeclaration
                                (context
                                    |> expressionContextAddVariablesInScope
                                        variablesForWholeLetIn
                                )
                        )
                        (declaration1UpNode
                            |> listMapAndCombineOk
                                (\letDecl ->
                                    letDecl
                                        |> letDeclaration
                                            (context
                                                |> expressionContextAddVariablesInScope
                                                    variablesForWholeLetIn
                                            )
                                )
                        )
                        (letIn.expression
                            |> expression
                                (context
                                    |> expressionContextAddVariablesInScope
                                        variablesForWholeLetIn
                                )
                        )


grainExpressionVoid : GrainExpression
grainExpressionVoid =
    GrainExpressionReference
        { moduleOrigin = Nothing
        , name = "void"
        }


{-| Recursively find all introduced variables
in the [pattern](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern)
(like `a` and `b` in `( Just a, { b } )`)
-}
patternBindings : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> List String
patternBindings (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.VarPattern name ->
            [ name ]

        Elm.Syntax.Pattern.AsPattern afterAsPattern (Elm.Syntax.Node.Node _ name) ->
            name :: (afterAsPattern |> patternBindings)

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens |> patternBindings

        Elm.Syntax.Pattern.ListPattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.RecordPattern fields ->
            fields |> List.map Elm.Syntax.Node.value

        Elm.Syntax.Pattern.NamedPattern _ patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            (tailPattern |> patternBindings) ++ (headPattern |> patternBindings)

        Elm.Syntax.Pattern.AllPattern ->
            []

        Elm.Syntax.Pattern.UnitPattern ->
            []

        Elm.Syntax.Pattern.CharPattern _ ->
            []

        Elm.Syntax.Pattern.StringPattern _ ->
            []

        Elm.Syntax.Pattern.IntPattern _ ->
            []

        Elm.Syntax.Pattern.HexPattern _ ->
            []

        Elm.Syntax.Pattern.FloatPattern _ ->
            []


resultAndThen2 :
    (a -> b -> Result error c)
    -> Result error a
    -> Result error b
    -> Result error c
resultAndThen2 abToResult aResult bResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    abToResult a b


listMapToFastSetsAndUnify :
    (listElement -> FastSet.Set comparableFastSetElement)
    -> List listElement
    -> FastSet.Set comparableFastSetElement
listMapToFastSetsAndUnify elementToSet list =
    list
        |> List.foldl
            (\element soFar ->
                FastSet.union
                    (element |> elementToSet)
                    soFar
            )
            FastSet.empty


listMapAndToFastSet :
    (a -> comparable)
    -> List a
    -> FastSet.Set comparable
listMapAndToFastSet elementToSetElement list =
    list
        |> List.foldl
            (\element soFar ->
                soFar
                    |> FastSet.insert
                        (element |> elementToSetElement)
            )
            FastSet.empty


condenseExpressionCall :
    { called : GrainExpression
    , argument0 : GrainExpression
    , argument1Up : List GrainExpression
    }
    -> GrainExpression
condenseExpressionCall call =
    case call.called of
        GrainExpressionCall calledCall ->
            condenseExpressionCall
                { called = calledCall.called
                , argument0 = calledCall.argument0
                , argument1Up =
                    calledCall.argument1Up
                        ++ (call.argument0 :: call.argument1Up)
                }

        GrainExpressionLambda calledLambda ->
            case ( calledLambda.parameter0, calledLambda.result ) of
                ( GrainPatternVariable "generated_record", GrainExpressionRecordAccess recordAccess ) ->
                    case call.argument1Up of
                        [] ->
                            GrainExpressionRecordAccess
                                { record = call.argument0
                                , field = recordAccess.field
                                }

                        argument1 :: argument2Up ->
                            GrainExpressionCall
                                { called =
                                    GrainExpressionRecordAccess
                                        { record = call.argument0
                                        , field = recordAccess.field
                                        }
                                , argument0 = argument1
                                , argument1Up = argument2Up
                                }

                _ ->
                    GrainExpressionCall
                        { called = GrainExpressionLambda calledLambda
                        , argument0 = call.argument0
                        , argument1Up = call.argument1Up
                        }

        calledNotCall ->
            GrainExpressionCall
                { called = calledNotCall
                , argument0 = call.argument0
                , argument1Up = call.argument1Up
                }


grainExpressionIsDefinitelyOfTypeString : GrainExpression -> Bool
grainExpressionIsDefinitelyOfTypeString grainExpression =
    case grainExpression of
        GrainExpressionString _ ->
            True

        GrainExpressionCall call ->
            call.called
                == GrainExpressionReference { moduleOrigin = Just "String", name = "concat" }
                && ((call.argument1Up |> List.length) == 1)

        GrainExpressionChar _ ->
            False

        GrainExpressionFloat _ ->
            False

        GrainExpressionReference _ ->
            False

        GrainExpressionRecordAccess _ ->
            False

        GrainExpressionTuple _ ->
            False

        GrainExpressionIfElse _ ->
            False

        GrainExpressionList _ ->
            False

        GrainExpressionRecord _ ->
            False

        GrainExpressionRecordUpdate _ ->
            False

        GrainExpressionLambda _ ->
            False

        GrainExpressionMatch _ ->
            False

        GrainExpressionWithLetDeclarations _ ->
            False


case_ :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Expression.Case
    ->
        Result
            String
            { pattern : GrainPattern, result : GrainExpression }
case_ context ( patternNode, resultNode ) =
    Result.andThen
        (\casePattern ->
            Result.map
                (\result ->
                    { pattern = casePattern.pattern
                    , result = result
                    }
                )
                (resultNode
                    |> expression
                        (context
                            |> expressionContextAddVariablesInScope
                                casePattern.introducedVariables
                        )
                )
        )
        (patternNode
            |> pattern
                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                    context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                , variantLookup = context.variantLookup
                }
        )


letDeclaration :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
    -> Result String GrainLetDeclaration
letDeclaration context (Elm.Syntax.Node.Node _ syntaxLetDeclaration) =
    case syntaxLetDeclaration of
        Elm.Syntax.Expression.LetDestructuring destructuringPatternNode destructuringExpressionNode ->
            Result.map2
                (\destructuringPattern destructuringExpression ->
                    GrainLetDestructuring
                        { pattern = destructuringPattern.pattern
                        , expression = destructuringExpression
                        }
                )
                (destructuringPatternNode
                    |> pattern
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                        , variantLookup = context.variantLookup
                        }
                )
                (destructuringExpressionNode |> expression context)

        Elm.Syntax.Expression.LetFunction letValueOrFunction ->
            Result.map
                GrainLetDeclarationValueOrFunction
                (letValueOrFunction
                    |> letValueOrFunctionDeclaration context
                )


letValueOrFunctionDeclaration :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { name : String
            , result : GrainExpression
            , type_ : Maybe GrainType
            }
letValueOrFunctionDeclaration context syntaxDeclarationValueOrFunction =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationValueOrFunction.declaration |> Elm.Syntax.Node.value
    in
    resultAndThen2
        (\parameters maybeType ->
            Result.map
                (\result ->
                    { name =
                        implementation.name
                            |> Elm.Syntax.Node.value
                            |> variableNameDisambiguateFromGrainKeywords
                    , type_ = maybeType
                    , result =
                        case parameters of
                            [] ->
                                result

                            parameter0 :: parameter1Up ->
                                GrainExpressionLambda
                                    { parameter0 = parameter0.pattern
                                    , parameter1Up = parameter1Up |> List.map .pattern
                                    , result = result
                                    }
                    }
                )
                (implementation.expression
                    |> expression
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                        , variantLookup =
                            context.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            FastSet.union
                                (parameters
                                    |> listMapToFastSetsAndUnify .introducedVariables
                                )
                                context.variablesFromWithinDeclarationInScope
                        }
                )
        )
        (implementation.arguments
            |> listMapAndCombineOk
                (\p ->
                    p
                        |> pattern
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                )
        )
        (case syntaxDeclarationValueOrFunction.signature of
            Nothing ->
                Ok Nothing

            Just (Elm.Syntax.Node.Node _ signature) ->
                Result.map Just
                    (signature.typeAnnotation
                        |> type_
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                    )
        )


expressionOperatorToGrainFunctionReference :
    String
    -> Result String { moduleOrigin : Maybe String, name : String }
expressionOperatorToGrainFunctionReference operatorSymbol =
    case operatorSymbol of
        "+" ->
            Ok { moduleOrigin = Nothing, name = "basics_add" }

        "-" ->
            Ok { moduleOrigin = Nothing, name = "basics_sub" }

        "*" ->
            Ok { moduleOrigin = Nothing, name = "basics_mul" }

        "/" ->
            Ok { moduleOrigin = Nothing, name = "basics_fdiv" }

        "//" ->
            Ok { moduleOrigin = Nothing, name = "basics_idiv" }

        "^" ->
            Ok { moduleOrigin = Nothing, name = "basics_pow" }

        "==" ->
            Ok { moduleOrigin = Nothing, name = "basics_eq" }

        "/=" ->
            Ok { moduleOrigin = Nothing, name = "basics_neq" }

        "||" ->
            Ok { moduleOrigin = Nothing, name = "basics_or" }

        "&&" ->
            Ok { moduleOrigin = Nothing, name = "basics_and" }

        "<" ->
            Ok { moduleOrigin = Nothing, name = "basics_lt" }

        ">" ->
            Ok { moduleOrigin = Nothing, name = "basics_gt" }

        "<=" ->
            Ok { moduleOrigin = Nothing, name = "basics_le" }

        ">=" ->
            Ok { moduleOrigin = Nothing, name = "basics_ge" }

        "::" ->
            Ok { moduleOrigin = Nothing, name = "list_cons" }

        "++" ->
            Ok { moduleOrigin = Just "List", name = "append" }

        unknownOrUnsupportedOperator ->
            Err ("unknown/unsupported operator " ++ unknownOrUnsupportedOperator)


{-| Print a grain value/function declaration
-}
printGrainValueOrFunctionDeclaration :
    { name : String
    , result : GrainExpression
    , type_ : Maybe GrainType
    }
    -> Print
printGrainValueOrFunctionDeclaration grainValueOrFunctionDeclaration =
    Print.exactly grainValueOrFunctionDeclaration.name
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                ((case grainValueOrFunctionDeclaration.type_ of
                    Nothing ->
                        Print.empty

                    Just declaredType ->
                        let
                            typePrint : Print
                            typePrint =
                                printGrainTypeNotParenthesized declaredType

                            fullLineSpread : Print.LineSpread
                            fullLineSpread =
                                typePrint |> Print.lineSpread
                        in
                        Print.exactly ":"
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented fullLineSpread
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            typePrint
                                        )
                                )
                 )
                    |> Print.followedBy
                        (Print.exactly " =")
                    |> Print.followedBy
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (printGrainExpressionParenthesizedIfWithLetDeclarations
                                    grainValueOrFunctionDeclaration.result
                                )
                        )
                )
            )


type GrainValueOrFunctionDependencyBucket element
    = GrainValueOrFunctionDependencySingle element
    | GrainValueOrFunctionDependencyRecursiveBucket (List element)


grainValueOrFunctionDeclarationsGroupByDependencies :
    List
        { name : String
        , result : GrainExpression
        , type_ : Maybe GrainType
        }
    ->
        { mostToLeastDependedOn :
            List
                (GrainValueOrFunctionDependencyBucket
                    { name : String
                    , result : GrainExpression
                    , type_ : Maybe GrainType
                    }
                )
        }
grainValueOrFunctionDeclarationsGroupByDependencies grainValueOrFunctionDeclarations =
    let
        ordered :
            List
                (Data.Graph.SCC
                    { name : String
                    , result : GrainExpression
                    , type_ : Maybe GrainType
                    }
                )
        ordered =
            Data.Graph.stronglyConnComp
                (grainValueOrFunctionDeclarations
                    |> List.map
                        (\grainValueOrFunctionDeclaration ->
                            ( grainValueOrFunctionDeclaration
                            , grainValueOrFunctionDeclaration.name
                            , grainValueOrFunctionDeclaration.result
                                |> grainExpressionContainedLocalReferences
                                |> FastSet.toList
                            )
                        )
                )
    in
    { mostToLeastDependedOn =
        ordered
            |> List.map
                (\grainValueOrFunctionDependencyGroup ->
                    case grainValueOrFunctionDependencyGroup of
                        Data.Graph.CyclicSCC recursiveGroup ->
                            GrainValueOrFunctionDependencyRecursiveBucket recursiveGroup

                        Data.Graph.AcyclicSCC single ->
                            GrainValueOrFunctionDependencySingle single
                )
    }


type GrainEnumTypeOrTypeAliasDeclaration
    = GrainEnumTypeDeclaration
        { name : String
        , parameters : List String
        , variants : FastDict.Dict String (List GrainType)
        }
    | GrainTypeAliasDeclaration
        { name : String
        , parameters : List String
        , type_ : GrainType
        }


grainTypeDeclarationsGroupByDependencies :
    { typeAliases :
        List
            { name : String
            , parameters : List String
            , type_ : GrainType
            }
    , enums :
        List
            { name : String
            , parameters : List String
            , variants : FastDict.Dict String (List GrainType)
            }
    }
    ->
        { mostToLeastDependedOn :
            List
                (GrainValueOrFunctionDependencyBucket
                    GrainEnumTypeOrTypeAliasDeclaration
                )
        }
grainTypeDeclarationsGroupByDependencies grainTypeDeclarations =
    let
        ordered : List (Data.Graph.SCC GrainEnumTypeOrTypeAliasDeclaration)
        ordered =
            Data.Graph.stronglyConnComp
                ((grainTypeDeclarations.typeAliases
                    |> List.map
                        (\aliasDeclaration ->
                            ( GrainTypeAliasDeclaration aliasDeclaration
                            , aliasDeclaration.name
                            , aliasDeclaration.type_
                                |> grainTypeContainedLocalReferences
                                |> FastSet.toList
                            )
                        )
                 )
                    ++ (grainTypeDeclarations.enums
                            |> List.map
                                (\enumDeclaration ->
                                    ( GrainEnumTypeDeclaration enumDeclaration
                                    , enumDeclaration.name
                                    , enumDeclaration.variants
                                        |> FastDict.foldl
                                            (\_ variantValues soFar ->
                                                FastSet.union
                                                    (variantValues
                                                        |> listMapToFastSetsAndUnify
                                                            grainTypeContainedLocalReferences
                                                    )
                                                    soFar
                                            )
                                            FastSet.empty
                                        |> FastSet.toList
                                    )
                                )
                       )
                )
    in
    { mostToLeastDependedOn =
        ordered
            |> List.map
                (\grainValueOrFunctionDependencyGroup ->
                    case grainValueOrFunctionDependencyGroup of
                        Data.Graph.CyclicSCC recursiveGroup ->
                            GrainValueOrFunctionDependencyRecursiveBucket recursiveGroup

                        Data.Graph.AcyclicSCC single ->
                            GrainValueOrFunctionDependencySingle single
                )
    }


grainTypeContainedLocalReferences : GrainType -> FastSet.Set String
grainTypeContainedLocalReferences grainType =
    -- IGNORE TCO
    case grainType of
        GrainTypeVariable _ ->
            FastSet.empty

        GrainTypeTuple parts ->
            FastSet.union
                (parts.part0 |> grainTypeContainedLocalReferences)
                (FastSet.union
                    (parts.part1 |> grainTypeContainedLocalReferences)
                    (parts.part2Up
                        |> listMapToFastSetsAndUnify
                            grainTypeContainedLocalReferences
                    )
                )

        GrainTypeConstruct typeConstruct ->
            FastSet.union
                (case typeConstruct.moduleOrigin of
                    Nothing ->
                        FastSet.singleton typeConstruct.name

                    Just _ ->
                        FastSet.empty
                )
                (typeConstruct.arguments
                    |> listMapToFastSetsAndUnify grainTypeContainedLocalReferences
                )

        GrainTypeFunction typeFunction ->
            FastSet.union
                (typeFunction.input
                    |> listMapToFastSetsAndUnify grainTypeContainedLocalReferences
                )
                (typeFunction.output |> grainTypeContainedLocalReferences)


qualifiedReferenceToGrainName :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
qualifiedReferenceToGrainName reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleOrigin ->
            moduleOrigin
                ++ "."
                ++ reference.name


printGrainExpressionParenthesizedIfSpaceSeparated : GrainExpression -> Print
printGrainExpressionParenthesizedIfSpaceSeparated grainExpression =
    if grainExpression |> grainExpressionIsSpaceSeparated then
        printParenthesized
            { opening = "{"
            , closing = "}"
            , inner = printGrainExpressionNotParenthesized grainExpression
            }

    else
        printGrainExpressionNotParenthesized grainExpression


printGrainExpressionParenthesizedIfWithLetDeclarations : GrainExpression -> Print
printGrainExpressionParenthesizedIfWithLetDeclarations grainExpression =
    case grainExpression of
        GrainExpressionWithLetDeclarations grainExpressionWithLetDeclarations ->
            printParenthesized
                { opening = "{"
                , closing = "}"
                , inner =
                    printGrainExpressionWithLetDeclarations
                        grainExpressionWithLetDeclarations
                }

        grainExpressionNotWithLetDeclarations ->
            printGrainExpressionNotParenthesized grainExpressionNotWithLetDeclarations


grainExpressionIsSpaceSeparated : GrainExpression -> Bool
grainExpressionIsSpaceSeparated grainExpression =
    case grainExpression of
        GrainExpressionChar _ ->
            False

        GrainExpressionFloat _ ->
            False

        GrainExpressionString _ ->
            False

        GrainExpressionReference _ ->
            False

        GrainExpressionRecordAccess _ ->
            False

        GrainExpressionTuple _ ->
            False

        GrainExpressionIfElse _ ->
            True

        GrainExpressionList _ ->
            False

        GrainExpressionRecord _ ->
            False

        GrainExpressionRecordUpdate _ ->
            False

        GrainExpressionCall _ ->
            False

        GrainExpressionLambda _ ->
            True

        GrainExpressionMatch _ ->
            True

        GrainExpressionWithLetDeclarations _ ->
            True


{-| Print a [`GrainExpression`](#GrainExpression)
-}
printGrainExpressionNotParenthesized : GrainExpression -> Print
printGrainExpressionNotParenthesized grainExpression =
    -- IGNORE TCO
    case grainExpression of
        GrainExpressionCall call ->
            printGrainExpressionCall call

        GrainExpressionReference reference ->
            Print.exactly
                (reference |> qualifiedReferenceToGrainName)

        GrainExpressionIfElse ifElse ->
            printGrainExpressionIfElse ifElse

        GrainExpressionChar charValue ->
            Print.exactly (charLiteral charValue)

        GrainExpressionFloat float ->
            Print.exactly (grainNumberLiteralToString float)

        GrainExpressionString string ->
            printGrainString string

        GrainExpressionTuple parts ->
            printGrainExpressionTuple parts

        GrainExpressionWithLetDeclarations expressionWithLetDeclarations ->
            printGrainExpressionWithLetDeclarations expressionWithLetDeclarations

        GrainExpressionMatch syntaxWhenIs ->
            printGrainExpressionMatch syntaxWhenIs

        GrainExpressionLambda syntaxLambda ->
            printGrainExpressionLambda syntaxLambda

        GrainExpressionRecord fields ->
            printGrainExpressionRecord fields

        GrainExpressionList elements ->
            printGrainExpressionList elements

        GrainExpressionRecordAccess syntaxRecordAccess ->
            printGrainExpressionParenthesizedIfSpaceSeparated
                syntaxRecordAccess.record
                |> Print.followedBy
                    (Print.exactly
                        ("." ++ syntaxRecordAccess.field)
                    )

        GrainExpressionRecordUpdate syntaxRecordUpdate ->
            printGrainExpressionRecordUpdate syntaxRecordUpdate


printGrainExpressionTuple :
    { part0 : GrainExpression
    , part1 : GrainExpression
    , part2Up : List GrainExpression
    }
    -> Print
printGrainExpressionTuple parts =
    let
        part0Print : Print
        part0Print =
            printGrainExpressionNotParenthesized
                parts.part0

        part1Print : Print
        part1Print =
            printGrainExpressionNotParenthesized
                parts.part1

        part2UpPrints : List Print
        part2UpPrints =
            parts.part2Up
                |> List.map printGrainExpressionNotParenthesized

        lineSpread : Print.LineSpread
        lineSpread =
            part0Print
                |> Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> part1Print |> Print.lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() ->
                        part2UpPrints
                            |> Print.lineSpreadListMapAndCombine
                                Print.lineSpread
                    )
    in
    Print.exactly "( "
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                ((part0Print :: part1Print :: part2UpPrints)
                    |> Print.listIntersperseAndFlatten
                        (Print.exactly ","
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented lineSpread)
                        )
                )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy (Print.exactly ")")


printGrainExpressionCall :
    { called : GrainExpression
    , argument0 : GrainExpression
    , argument1Up : List GrainExpression
    }
    -> Print
printGrainExpressionCall call =
    let
        calledPrint : Print
        calledPrint =
            printGrainExpressionParenthesizedIfSpaceSeparated
                call.called

        argumentPrints : List Print
        argumentPrints =
            (call.argument0 :: call.argument1Up)
                |> List.map
                    -- I'm also surprised that grain requires extra parens here
                    printGrainExpressionParenthesizedIfSpaceSeparated

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            argumentPrints
                |> Print.lineSpreadListMapAndCombine Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> calledPrint |> Print.lineSpread)
    in
    calledPrint
        |> Print.followedBy (Print.exactly "(")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.emptyOrLinebreakIndented fullLineSpread
                    |> Print.followedBy
                        (argumentPrints
                            |> Print.listIntersperseAndFlatten
                                (Print.exactly ","
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented fullLineSpread)
                                )
                        )
                )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented fullLineSpread)
        |> Print.followedBy (Print.exactly ")")


grainNumberLiteralToString : Float -> String
grainNumberLiteralToString float =
    float |> String.fromFloat


printGrainExpressionList : List GrainExpression -> Print
printGrainExpressionList listElements =
    case listElements of
        [] ->
            Print.exactly "[]"

        element0 :: element1Up ->
            let
                elementsPrint : Print
                elementsPrint =
                    (element0 :: element1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\element ->
                                printGrainExpressionNotParenthesized element
                            )
                            (Print.exactly ","
                                |> Print.followedBy Print.linebreakIndented
                            )
            in
            Print.exactly "[ "
                |> Print.followedBy
                    (Print.withIndentIncreasedBy 2
                        elementsPrint
                    )
                |> Print.followedBy
                    (Print.spaceOrLinebreakIndented
                        (elementsPrint |> Print.lineSpread)
                    )
                |> Print.followedBy
                    (Print.exactly "]")


printGrainExpressionRecordUpdate :
    { originalRecordVariable : String
    , fields : FastDict.Dict String GrainExpression
    }
    -> Print
printGrainExpressionRecordUpdate syntaxRecordUpdate =
    printExactlyCurlyOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (Print.exactly ("..." ++ syntaxRecordUpdate.originalRecordVariable))
            )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy (Print.exactly ", ")
                    |> Print.followedBy
                        (syntaxRecordUpdate.fields
                            |> FastDict.toList
                            |> Print.listMapAndIntersperseAndFlatten
                                (\( fieldName, fieldValue ) ->
                                    Print.withIndentIncreasedBy 2
                                        (Print.exactly (fieldName ++ ":"))
                                        |> Print.followedBy
                                            (Print.withIndentAtNextMultipleOf4
                                                (Print.linebreakIndented
                                                    |> Print.followedBy
                                                        (printGrainExpressionNotParenthesized fieldValue)
                                                )
                                            )
                                )
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (Print.exactly ", ")
                                )
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy printExactlyCurlyClosing


patternIsSpaceSeparated : GrainPattern -> Bool
patternIsSpaceSeparated grainPattern =
    case grainPattern of
        GrainPatternIgnore ->
            False

        GrainPatternNumber _ ->
            False

        GrainPatternChar _ ->
            False

        GrainPatternString _ ->
            False

        GrainPatternVariable _ ->
            False

        GrainPatternAs _ ->
            True

        GrainPatternListCons _ ->
            True

        GrainPatternListExact _ ->
            False

        GrainPatternRecordInexhaustive _ ->
            False

        GrainPatternVariant _ ->
            False

        GrainPatternTuple _ ->
            False


printGrainPatternParenthesizedIfSpaceSeparated : GrainPattern -> Print
printGrainPatternParenthesizedIfSpaceSeparated grainPattern =
    if grainPattern |> patternIsSpaceSeparated then
        printParenthesized
            { opening = "("
            , closing = ")"
            , inner = grainPattern |> printGrainPatternNotParenthesized
            }

    else
        grainPattern |> printGrainPatternNotParenthesized


printGrainExpressionLambda :
    { parameter0 : GrainPattern
    , parameter1Up : List GrainPattern
    , result : GrainExpression
    }
    -> Print
printGrainExpressionLambda syntaxLambda =
    Print.exactly "("
        |> Print.followedBy
            ((syntaxLambda.parameter0 :: syntaxLambda.parameter1Up)
                |> Print.listMapAndIntersperseAndFlatten
                    printGrainPatternParenthesizedIfSpaceSeparated
                    (Print.exactly ", ")
            )
        |> Print.followedBy (Print.exactly ") =>")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printGrainExpressionParenthesizedIfSpaceSeparated
                            syntaxLambda.result
                        )
                )
            )


printGrainExpressionIfElse :
    { condition : GrainExpression
    , onTrue : GrainExpression
    , onFalse : GrainExpression
    }
    -> Print
printGrainExpressionIfElse syntaxIfElse =
    let
        conditionPrint : Print
        conditionPrint =
            printGrainExpressionNotParenthesized syntaxIfElse.condition

        conditionLineSpread : Print.LineSpread
        conditionLineSpread =
            conditionPrint |> Print.lineSpread
    in
    Print.exactly "if ("
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented conditionLineSpread
                    |> Print.followedBy conditionPrint
                )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented conditionLineSpread)
        |> Print.followedBy
            (Print.exactly ")")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printGrainExpressionParenthesizedIfSpaceSeparated
                            syntaxIfElse.onTrue
                        )
                    |> Print.followedBy Print.linebreak
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "else")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printGrainExpressionParenthesizedIfSpaceSeparated
                            syntaxIfElse.onFalse
                        )
                )
            )


printGrainExpressionMatch :
    { matched : GrainExpression
    , case0 : { pattern : GrainPattern, result : GrainExpression }
    , case1Up : List { pattern : GrainPattern, result : GrainExpression }
    }
    -> Print
printGrainExpressionMatch matchWith =
    let
        matchedPrint : Print
        matchedPrint =
            printGrainExpressionNotParenthesized matchWith.matched

        matchedPrintLineSpread : Print.LineSpread
        matchedPrintLineSpread =
            matchedPrint |> Print.lineSpread
    in
    Print.exactly "match ("
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.emptyOrLinebreakIndented matchedPrintLineSpread
                    |> Print.followedBy matchedPrint
                )
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented matchedPrintLineSpread)
        |> Print.followedBy (Print.exactly ") {")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        ((matchWith.case0 :: matchWith.case1Up)
                            |> Print.listMapAndIntersperseAndFlatten
                                printGrainExpressionMatchCase
                                printLinebreakLinebreakIndented
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


printGrainExpressionWithLetDeclarations :
    { declaration0 : GrainLetDeclaration
    , declaration1Up : List GrainLetDeclaration
    , result : GrainExpression
    }
    -> Print
printGrainExpressionWithLetDeclarations syntaxLetIn =
    let
        letDestructurings :
            List
                { pattern : GrainPattern
                , expression : GrainExpression
                }
        letDestructurings =
            (syntaxLetIn.declaration0 :: syntaxLetIn.declaration1Up)
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            GrainLetDestructuring letDestructuring ->
                                Just letDestructuring

                            GrainLetDeclarationValueOrFunction _ ->
                                Nothing
                    )

        letValueOrFunctions :
            List
                { name : String
                , result : GrainExpression
                , type_ : Maybe GrainType
                }
        letValueOrFunctions =
            (syntaxLetIn.declaration0 :: syntaxLetIn.declaration1Up)
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            GrainLetDeclarationValueOrFunction letValueOrFunction ->
                                Just letValueOrFunction

                            GrainLetDestructuring _ ->
                                Nothing
                    )

        ordered :
            { mostToLeastDependedOn :
                List
                    (GrainValueOrFunctionDependencyBucket
                        { name : String
                        , result : GrainExpression
                        , type_ : Maybe GrainType
                        }
                    )
            }
        ordered =
            letValueOrFunctions
                |> grainValueOrFunctionDeclarationsGroupByDependencies
    in
    (letDestructurings
        |> Print.listMapAndIntersperseAndFlatten
            (\letDestructuring ->
                Print.exactly "let "
                    |> Print.followedBy
                        (letDestructuring |> printGrainLetDestructuring)
                    |> Print.followedBy Print.linebreakIndented
                    |> Print.followedBy Print.linebreakIndented
            )
            Print.empty
    )
        |> Print.followedBy
            (ordered.mostToLeastDependedOn
                |> Print.listMapAndIntersperseAndFlatten
                    (\dependencyGroup ->
                        case dependencyGroup of
                            GrainValueOrFunctionDependencySingle letValueOrFunction ->
                                Print.exactly "let "
                                    |> Print.followedBy
                                        (letValueOrFunction |> printGrainValueOrFunctionDeclaration)
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy Print.linebreakIndented

                            GrainValueOrFunctionDependencyRecursiveBucket recursiveGroup ->
                                Print.exactly "let "
                                    -- I would have thought let rec
                                    -- but the compiler disagrees
                                    |> Print.followedBy
                                        (recursiveGroup
                                            |> Print.listMapAndIntersperseAndFlatten
                                                (\letValueOrFunction ->
                                                    (letValueOrFunction |> printGrainValueOrFunctionDeclaration)
                                                        |> Print.followedBy Print.linebreakIndented
                                                )
                                                (Print.exactly "and ")
                                        )
                    )
                    Print.empty
            )
        |> Print.followedBy
            (printGrainExpressionNotParenthesized syntaxLetIn.result)


printGrainLetDestructuring :
    { pattern : GrainPattern, expression : GrainExpression }
    -> Print
printGrainLetDestructuring letDestructuring =
    printGrainPatternParenthesizedIfSpaceSeparated letDestructuring.pattern
        |> Print.followedBy (Print.exactly " =")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printGrainExpressionNotParenthesized letDestructuring.expression)
                )
            )


printGrainExpressionMatchCase :
    { pattern : GrainPattern, result : GrainExpression }
    -> Print
printGrainExpressionMatchCase branch =
    let
        patternPrint : Print
        patternPrint =
            printGrainPatternNotParenthesized branch.pattern
    in
    Print.withIndentIncreasedBy 2
        patternPrint
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented
                (patternPrint |> Print.lineSpread)
            )
        |> Print.followedBy (Print.exactly "=>")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printGrainExpressionParenthesizedIfSpaceSeparated
                            branch.result
                        )
                )
            )
        |> Print.followedBy (Print.exactly ",")


{-| Print value/function declarations into
an F# module called `Elm` in the global namespace that exposes all members.
Will also add some internal wrapper declarations.
-}
grainDeclarationsToModuleString :
    { valuesAndFunctions :
        FastDict.Dict
            String
            { parameters : List GrainPattern
            , result : GrainExpression
            , type_ : Maybe GrainType
            }
    , typeAliases :
        FastDict.Dict
            String
            { parameters : List String
            , type_ : GrainType
            }
    , recordTypes : FastSet.Set (List String)
    , enumTypes :
        FastDict.Dict
            String
            { parameters : List String
            , variants : FastDict.Dict String (List GrainType)
            }
    }
    -> String
grainDeclarationsToModuleString grainDeclarations =
    let
        valueAndFunctionDeclarationsOrdered :
            { mostToLeastDependedOn :
                List
                    (GrainValueOrFunctionDependencyBucket
                        { name : String
                        , result : GrainExpression
                        , type_ : Maybe GrainType
                        }
                    )
            }
        valueAndFunctionDeclarationsOrdered =
            grainDeclarations.valuesAndFunctions
                |> fastDictMapAndToList
                    (\name valueOrFunctionInfo ->
                        { name = name
                        , type_ = valueOrFunctionInfo.type_
                        , result = valueOrFunctionInfo.result
                        }
                    )
                |> grainValueOrFunctionDeclarationsGroupByDependencies

        typeDeclarationsOrdered :
            { mostToLeastDependedOn :
                List
                    (GrainValueOrFunctionDependencyBucket
                        GrainEnumTypeOrTypeAliasDeclaration
                    )
            }
        typeDeclarationsOrdered =
            grainTypeDeclarationsGroupByDependencies
                { typeAliases =
                    grainDeclarations.typeAliases
                        |> fastDictMapAndToList
                            (\name info ->
                                { name = name
                                , parameters = info.parameters
                                , type_ = info.type_
                                }
                            )
                , enums =
                    grainDeclarations.enumTypes
                        |> fastDictMapAndToList
                            (\name info ->
                                { name = name
                                , parameters = info.parameters
                                , variants = info.variants
                                }
                            )
                }
    in
    """module Elm

from "number" include Number
from "char" include Char
from "option" include Option
from "result" include Result
from "list" include List
from "range" include Range
from "array" include Array
from "string" include String

"""
        ++ defaultDeclarations
        ++ """

"""
        ++ (grainDeclarations.recordTypes
                |> FastSet.foldr
                    (\recordTypeFields soFar ->
                        printGrainRecordTypeDeclaration recordTypeFields
                            :: soFar
                    )
                    []
                |> Print.listIntersperseAndFlatten
                    (Print.linebreak
                        |> Print.followedBy Print.linebreak
                    )
                |> Print.toString
           )
        ++ """

"""
        ++ (typeDeclarationsOrdered.mostToLeastDependedOn
                |> Print.listMapAndIntersperseAndFlatten
                    (\typeAliasDeclarationGroup ->
                        case typeAliasDeclarationGroup of
                            GrainValueOrFunctionDependencySingle single ->
                                case single of
                                    GrainEnumTypeDeclaration enumDeclaration ->
                                        Print.exactly "enum "
                                            |> Print.followedBy
                                                (printGrainEnumTypeDeclaration enumDeclaration)

                                    GrainTypeAliasDeclaration aliasDeclaration ->
                                        Print.exactly "type "
                                            |> Print.followedBy
                                                (printGrainTypeAliasDeclaration aliasDeclaration)

                            GrainValueOrFunctionDependencyRecursiveBucket recursiveBucket ->
                                case recursiveBucket of
                                    [] ->
                                        Print.empty

                                    recursiveBucketMember0 :: recursiveBucketMember1Up ->
                                        (case recursiveBucketMember0 of
                                            GrainEnumTypeDeclaration enumDeclaration ->
                                                Print.exactly "enum rec "
                                                    |> Print.followedBy
                                                        (printGrainEnumTypeDeclaration enumDeclaration)

                                            GrainTypeAliasDeclaration aliasDeclaration ->
                                                Print.exactly "type rec "
                                                    |> Print.followedBy
                                                        (printGrainTypeAliasDeclaration aliasDeclaration)
                                        )
                                            |> Print.followedBy
                                                (recursiveBucketMember1Up
                                                    |> Print.listMapAndIntersperseAndFlatten
                                                        (\typeDeclaration ->
                                                            Print.linebreak
                                                                |> Print.followedBy Print.linebreak
                                                                |> Print.followedBy
                                                                    (case typeDeclaration of
                                                                        GrainEnumTypeDeclaration enumDeclaration ->
                                                                            Print.exactly "and enum "
                                                                                |> Print.followedBy
                                                                                    (printGrainEnumTypeDeclaration enumDeclaration)

                                                                        GrainTypeAliasDeclaration aliasDeclaration ->
                                                                            Print.exactly "and type "
                                                                                |> Print.followedBy
                                                                                    (printGrainTypeAliasDeclaration aliasDeclaration)
                                                                    )
                                                        )
                                                        Print.empty
                                                )
                    )
                    (Print.linebreak
                        |> Print.followedBy Print.linebreak
                    )
                |> Print.toString
           )
        ++ """


"""
        ++ ((valueAndFunctionDeclarationsOrdered.mostToLeastDependedOn
                |> Print.listMapAndIntersperseAndFlatten
                    (\dependencyGroup ->
                        case dependencyGroup of
                            GrainValueOrFunctionDependencySingle letValueOrFunction ->
                                Print.exactly "provide let "
                                    |> Print.followedBy
                                        (letValueOrFunction |> printGrainValueOrFunctionDeclaration)
                                    |> Print.followedBy Print.linebreak
                                    |> Print.followedBy Print.linebreakIndented

                            GrainValueOrFunctionDependencyRecursiveBucket recursiveGroup ->
                                case recursiveGroup of
                                    [ onlyLetValueOrFunction ] ->
                                        case onlyLetValueOrFunction.result of
                                            GrainExpressionLambda _ ->
                                                Print.exactly "provide let rec "
                                                    |> Print.followedBy
                                                        ((onlyLetValueOrFunction |> printGrainValueOrFunctionDeclaration)
                                                            |> Print.followedBy Print.linebreak
                                                            |> Print.followedBy Print.linebreakIndented
                                                        )

                                            _ ->
                                                Print.exactly "provide let "
                                                    |> Print.followedBy
                                                        ((onlyLetValueOrFunction |> printGrainValueOrFunctionDeclaration)
                                                            |> Print.followedBy Print.linebreak
                                                            |> Print.followedBy Print.linebreakIndented
                                                        )

                                    recursiveGroupNotSingle ->
                                        Print.exactly "provide let "
                                            |> Print.followedBy
                                                (recursiveGroupNotSingle
                                                    |> Print.listMapAndIntersperseAndFlatten
                                                        (\letValueOrFunction ->
                                                            (letValueOrFunction |> printGrainValueOrFunctionDeclaration)
                                                                |> Print.followedBy Print.linebreak
                                                                |> Print.followedBy Print.linebreakIndented
                                                        )
                                                        (Print.exactly "and ")
                                                )
                    )
                    Print.empty
            )
                |> Print.toString
           )
        ++ """
"""


defaultDeclarations : String
defaultDeclarations =
    """let basics_always: ( kept, ignored ) => kept = ( kept, _ ) => kept

let basics_eq: (a, a) => Bool = (a, b) => a == b
let basics_neq: (a, a) => Bool = (a, b) => a != b
let basics_lt: (Number, Number) => Bool = (a, b) => a < b
let basics_gt: (Number, Number) => Bool = (a, b) => a > b
let basics_le: (Number, Number) => Bool = (a, b) => a <= b
let basics_ge: (Number, Number) => Bool = (a, b) => a >= b

enum Basics_Order {
  Basics_LT,
  Basics_EQ,
  Basics_GT,
}
let intToBasics_Order: Number => Basics_Order = comparisonNumber =>
  if (comparisonNumber < 0) {
    Basics_LT
  } else if (comparisonNumber > 0) {
    Basics_GT
  } else {
    Basics_EQ
  }
let basics_OrderToInt: Basics_Order => int = order => match (order) {
  Basics_LT => -1,
  Basics_EQ => 0,
  Basics_GT => 1,
}
let basics_compare: (a, a) => Basics_Order = (a, b) =>
  intToBasics_Order(compare(a, b))

let basics_add: (Number, Number) => Number = ( a, b ) => a + b
let basics_sub: (Number, Number) => Number = ( a, b ) => a - b
let basics_mul: (Number, Number) => Number = ( a, b ) => a * b
let basics_pow: (Number, Number) => Number = ( base, exponent ) =>
    base ** exponent
let basics_fdiv: (Number, Number) => Number = ( toDivide, divisor ) =>
    toDivide / divisor
let basics_idiv: (Number, Number) => Number = ( toDivide, divisor ) =>
    Number.trunc(toDivide / divisor)
let basics_modBy: (Number, Number) => Number = (divisor, toDivide) =>
  toDivide % divisor
let basics_remainderBy: (Number, Number) => Number = (divisor, toDivide) => {
  let modulus = toDivide % divisor

  if ({
    modulus > 0 && divisor < 0
  } || {
    modulus < 0 && divisor > 0
  }) {
    modulus - toDivide
  } else {
    modulus
  }
}

let basics_or: (Bool, Bool) => Bool = (a, b) => a || b
let basics_and: (Bool, Bool) => Bool = (a, b) => a && b
let basics_not: Bool => Bool = bool => !bool

let char_fromCode: Number => Char = charCode =>
  if (Char.isValid(charCode)) {
    Char.fromCode(charCode)
  } else {
    Char.fromCode(0)
  }

let list_singleton: a => List<a> = onlyElement => [onlyElement]
let list_cons: ( a, List<a> ) => List<a> = ( newHead, tail ) =>
    [newHead, ...tail]
let list_sort: List<a> => List<a> = list => List.sort(compare=compare, list)
let list_sortWith: ((a, a) => Basics_Order, List<a>) => List<a> = (
  elementCompare,
  list,
) => List.sort(compare=(a, b) => basics_OrderToInt(elementCompare(a, b)), list)
let list_range: (Number, Number) => List<Number> = (
  startInclusive,
  endInclusive,
) =>
  Range.Inclusive.map(
    identity,
    { rangeStart: startInclusive, rangeEnd: endInclusive }
  )
let list_intersperse: (a, List<a>) => List<a> = (sep, list) => match (list) {
  [] => [],
  [listHead, ...listTail] =>
    List.reduceRight((x, soFar) => [x, sep, ...soFar], [listHead], listTail),
}
let list_sum: List<Number> => Number = numbers => List.reduce((+), 0, numbers)
let list_product: List<Number> => Number = numbers =>
  List.reduce((*), 1, numbers)
let list_minimum: List<a> => Option<a> = list => match (list) {
  [head, ...tail] => Some(List.reduce(Number.min, head, tail)),
  _ => None,
}
let list_maximum: List<a> => Option<a> = list => match (list) {
  [head, ...tail] => Some(List.reduce(Number.max, head, tail)),
  _ => None,
}
let list_repeat: (Number, a) => List<a> = (count, element) =>
  List.init(count, (_) => element)
let list_foldl: ((a, state) => state, state, List<a>) => state = (
  reduce,
  initialState,
  list,
) => {
  List.reduce((soFar, element) => reduce(element, soFar), initialState, list)
}
let list_foldr: ((a, state) => state, state, List<a>) => state = (
  reduce,
  initialState,
  list,
) => {
  List.reduceRight(
    (soFar, element) => reduce(element, soFar),
    initialState,
    list
  )
}

let string_toList: String => List<Char> = string =>
  Array.toList(String.explode(string))
let string_fromList: List<Char> => String = chars =>
  String.implode(Array.fromList(chars))
let string_concat: List<String> => String = strings =>
  List.join(separator="", strings)
let string_repeat: (Number, String) => String = (count, segment) =>
  string_concat(list_repeat(count, segment))
let string_slice: (Number, Number, String) => String = (
  startInclusivePossiblyNegative,
  endExclusivePossiblyNegative,
  string,
) => {
  let startInclusive = if (startInclusivePossiblyNegative < 0)
    String.length(string) - startInclusivePossiblyNegative - 1
  else
    startInclusivePossiblyNegative
  and endExclusive = if (endExclusivePossiblyNegative < 0)
    String.length(string) - endExclusivePossiblyNegative
  else
    endExclusivePossiblyNegative

  String.slice(start=startInclusive, end=endExclusive, string)
}
let string_cons: (Char, String) => String = (newHeadChar, tail) =>
  String.concat(Char.toString(newHeadChar), tail)
let string_toInt: String => Option<Number> = string =>
  Result.toOption(Number.parseInt(string, radix=10))
let string_toFloat: String => Option<Number> = string =>
  Result.toOption(Number.parseFloat(string))
let string_filter: (Char => Bool, String) => String = (shouldBeKept, string) =>
  String.implode(Array.filter(shouldBeKept, String.explode(string)))
let string_lines: String => List<String> = string =>
  Array.toList(
    Array.flatMap(
      lineWithPotentialLoneLineBreaks => {
        String.split("\\n", lineWithPotentialLoneLineBreaks)
      },
      String.split("\\u{000D}", string)
    )
  )
let string_foldl: ((Char, state) => state, state, String) => state = (
  reduce,
  initialState,
  string,
) => {
  Array.reduce(
    (soFar, element) => reduce(element, soFar),
    initialState,
    String.explode(string)
  )
}
let string_foldr: ((Char, state) => state, state, String) => state = (
  reduce,
  initialState,
  string,
) => {
  Array.reduceRight(
    (soFar, element) => reduce(element, soFar),
    initialState,
    String.explode(string)
  )
}
let string_left: (Number, String) => String = (charCountToTake, string) =>
  String.slice(start=0, end=charCountToTake, string)
let string_right: (Number, String) => String = (charCountToTake, string) =>
  String.slice(start=String.length(string) - charCountToTake - 1, string)
let string_dropLeft: (Number, String) => String = (charCountToDrop, string) =>
  String.slice(start=charCountToDrop, string)
let string_dropRight: (Number, String) => String = (charCountToDrop, string) =>
  String.slice(start=0, end=String.length(string) - charCountToDrop, string)
let string_any: (Char => Bool, String) => Bool = (isFound, string) =>
  Array.some(isFound, String.explode(string))
let string_all: (Char => Bool, String) => Bool = (isFound, string) =>
  Array.every(isFound, String.explode(string))
"""


resultAndThen3 :
    (a -> b -> c -> Result error d)
    -> Result error a
    -> Result error b
    -> Result error c
    -> Result error d
resultAndThen3 abToResult aResult bResult cResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    case cResult of
                        Err error ->
                            Err error

                        Ok c ->
                            abToResult a b c


fastDictMapAndToList :
    (key -> value -> element)
    -> FastDict.Dict key value
    -> List element
fastDictMapAndToList keyValueToElement fastDict =
    fastDict
        |> FastDict.foldr
            (\key value soFar ->
                keyValueToElement key value
                    :: soFar
            )
            []


listMapAndCombineOk : (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOk elementToResult list =
    listMapAndCombineOkFrom [] elementToResult list


listMapAndCombineOkFrom : List ok -> (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOkFrom soFar elementToResult list =
    case list of
        [] ->
            Ok (soFar |> List.reverse)

        head :: tail ->
            case head |> elementToResult of
                Err headErr ->
                    Err headErr

                Ok headOk ->
                    listMapAndCombineOkFrom (headOk :: soFar)
                        elementToResult
                        tail


printLinebreakLinebreakIndented : Print.Print
printLinebreakLinebreakIndented =
    Print.linebreak
        |> Print.followedBy Print.linebreakIndented


printExactlyCurlyOpeningSpace : Print.Print
printExactlyCurlyOpeningSpace =
    Print.exactly "{ "


printExactlyCurlyClosing : Print
printExactlyCurlyClosing =
    Print.exactly "}"


printExactlyUnderscore : Print
printExactlyUnderscore =
    Print.exactly "_"
