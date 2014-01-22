;;; shm-ast-documentation.el --- Documentation of the Haskell AST

;; Copyright (c) 2013, Niklas Broberg, Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Documentation for parts of the AST.

;;; Code:

(defvar shm-ast-documentation
  '(("Module" "A complete Haskell source module"
     ("Module" "An ordinary Haskell module"))

    ("ModuleHead" "The head of a module, including the name and export specification")

    ("WarningText" "Warning text to optionally use in the module header of e.g. a deprecated module")

    ("ExportSpecList" "An explicit export specification")

    ("ExportSpec" "An item in a module's export specification"
     ("EVar" "Variable")
     ("EAbs" "T: a class or datatype exported abstractly, or a type synonym")
     ("EThingAll" "T(..): a class exported with all of its methods, or a datatype exported with all of its constructors")
     ("EThingWith" "T(C_1,...,C_n): a class exported with some of its methods, or a datatype exported with some of its constructors")
     ("EModuleContents" "module M: re-export a module"))

    ("ImportDecl" "An import declaration")

    ("ImportSpecList" "An explicit import specification list")

    ("ImportSpec" "An import specification, representing a single explicit item imported (or hidden) from a module"
     ("IVar" "Variable")
     ("IAbs" "T: the name of a class, datatype or type synonym")
     ("IThingAll" "T(..): a class imported with all of its methods, or a datatype imported with all of its constructors")
     ("IThingWith" "T(C_1,...,C_n): a class imported with some of its methods, or a datatype imported with some of its constructors"))

    ("Decl" "A top-level declaration"
     ("TypeDecl" "A type declaration")
     ("TypeFamDecl" "A type family declaration")
     ("DataDecl" "A data OR newtype declaration")
     ("GDataDecl" "A data OR newtype declaration, GADT style")
     ("DataFamDecl" "A data family declaration")
     ("TypeInsDecl" "A type family instance declaration")
     ("DataInsDecl" "A data family instance declaration")
     ("GDataInsDecl" "A data family instance declaration, GADT style")
     ("ClassDecl" "A declaration of a type class")
     ("InstDecl" "An declaration of a type class instance")
     ("DerivDecl" "A standalone deriving declaration")
     ("InfixDecl" "A declaration of operator fixity")
     ("DefaultDecl" "A declaration of default types")
     ("SpliceDecl" "Template Haskell splicing declaration")
     ("TypeSig" "A type signature declaration")
     ("FunBind" "A set of function binding clauses")
     ("PatBind" "A pattern binding")
     ("ForImp" "A foreign import declaration")
     ("ForExp" "A foreign export declaration")
     ("RulePragmaDecl" "A RULES pragma")
     ("DeprPragmaDecl" "A DEPRECATED pragma")
     ("WarnPragmaDecl" "A WARNING pragma")
     ("InlineSig" "An INLINE pragma")
     ("InlineConlikeSig" "An INLINE CONLIKE pragma")
     ("SpecSig" "A SPECIALISE pragma")
     ("SpecInlineSig" "A SPECIALISE INLINE pragma")
     ("InstSig" "A SPECIALISE instance pragma")
     ("AnnPragma" "An ANN pragma"))

    ("DeclHead" "The head of a type or class declaration")

    ("InstHead" "The head of an instance declaration")

    ("Binds" "A binding group inside a let or where clause"
     ("BDecls" "An ordinary binding group")
     ("IPBinds" "A binding group for implicit parameters"))

    ("IPBind" "A binding of an implicit parameter")

    ("ClassDecl" "Declarations inside a class declaration"
     ("ClsDecl" "Ordinary declaration")
     ("ClsDataFam" "Declaration of an associated data type")
     ("ClsTyFam" "Declaration of an associated type synonym")
     ("ClsTyDef" "Default choice for an associated type synonym"))

    ("InstDecl" "Declarations inside an instance declaration"
     ("InsDecl" "Ordinary declaration")
     ("InsType" "An associated type definition")
     ("InsData" "An associated data type implementation")
     ("InsGData" "An associated data type implemented using GADT style"))

    ("Deriving" "A deriving clause following a data type declaration")

    ("ConDecl" "Declaration of an ordinary data constructor"
     ("ConDecl" "Ordinary data constructor")
     ("InfixConDecl" "Infix data constructor")
     ("RecDecl" "Record constructor"))

    ("FieldDecl" "Declaration of a (list of) named field(s)")

    ("QualConDecl" "A single constructor declaration within a data type declaration, which may have an existential quantification binding")

    ("GadtDecl" "A single constructor declaration in a GADT data type declaration")

    ("BangType" "The type of a constructor argument or field, optionally including a strictness annotation"
     ("BangedTy" "Strict component, marked with \"!\"")
     ("UnBangedTy" "Non-strict component")
     ("UnpackedTy" "Unboxed component, marked with an UNPACK pragma"))

    ("Match" "Clauses of a function binding"
     ("Match" "A clause defined with prefix notation, i.e. the function name followed by its argument patterns, the right-hand side and an optional where clause")
     ("InfixMatch" "A clause defined with infix notation, i.e. first its first argument pattern, then the function name, then its following argument(s), the right-hand side and an optional where clause. Note that there can be more than two arguments to a function declared infix, hence the list of pattern arguments"))

    ("Rhs" "The right hand side of a function or pattern binding"
     ("UnGuardedRhs" "Unguarded right hand side (exp)")
     ("GuardedRhss" "Guarded right hand side (gdrhs)"))

    ("GuardedRhs" "A guarded right hand side | stmts = exp. The guard is a series of statements when using pattern guards, otherwise it will be a single qualifier expression")

    ("Context" "A context is a set of assertions")

    ("FunDep" "A functional dependency, given on the form l1 l2 ... ln -> r2 r3 .. rn")

    ("Asst" "Class assertion. In Haskell 98, the argument would be a tyvar, but this definition allows multiple parameters, and allows them to be types. Also extended with support for implicit parameters and equality constraints"
     ("ClassA" "Ordinary class assertion")
     ("InfixA" "Class assertion where the class name is given infix")
     ("IParam" "Implicit parameter assertion")
     ("EqualP" "Type equality constraint"))

    ("Type" "A type qualified with a context. An unqualified type has an empty context"
     ("TyForall" "Qualified type")
     ("TyFun" "Function type")
     ("TyTuple" "Tuple type, possibly boxed")
     ("TyList" "List syntax, e.g. [a], as opposed to [] a")
     ("TyApp" "Application of a type constructor")
     ("TyVar" "Type variable")
     ("TyCon" "Named type or type constructor")
     ("TyParen" "Type surrounded by parentheses")
     ("TyInfix" "Infix type constructor")
     ("TyKind" "Type with explicit kind signature"))

    ("Kind" "An explicit kind annotation"
     ("KindStar" "* , the kind of types")
     ("KindBang" "!, the kind of unboxed types")
     ("KindFn" "->, the kind of a type constructor")
     ("KindParen" "A parenthesised kind")
     ("KindVar" "A kind variable (as-of-yet unsupported by compilers)"))

    ("TyVarBind" "A type variable declaration, optionally with an explicit kind annotation"
     ("KindedVar" "Variable binding with kind annotation")
     ("UnkindedVar" "Ordinary variable binding"))

    ("Exp" "Haskell expression"
     ("Var" "Variable")
     ("IPVar" "Implicit parameter variable")
     ("Con" "Data constructor")
     ("Lit" "Literal constant")
     ("InfixApp" "Infix application")
     ("App" "Ordinary application")
     ("NegApp" "Negation expression -exp (unary minus)")
     ("Lambda" "Lambda expression")
     ("Let" "Local declarations with let ... in .")
     ("If" "if exp then exp else exp")
     ("Case" "case exp of alts")
     ("Do" "do-expression: the last statement in the list should be an expression")
     ("MDo" "mdo-expression")
     ("Tuple" "Tuple expression")
     ("TupleSection" "Tuple section expression, e.g. (,,3)")
     ("List" "List expression")
     ("Paren" "Parenthesised expression")
     ("LeftSection" "Left section (exp qop)")
     ("RightSection" "Right section (qop exp)")
     ("RecConstr" "Record construction expression")
     ("RecUpdate" "Record update expression")
     ("EnumFrom" "Unbounded arithmetic sequence, incrementing by 1: [from ..]")
     ("EnumFromTo" "Bounded arithmetic sequence, incrementing by 1 [from .. to]")
     ("EnumFromThen" "Unbounded arithmetic sequence, with first two elements given [from, then ..]")
     ("EnumFromThenTo" "Bounded arithmetic sequence, with first two elements given [from, then .. to]")
     ("ListComp" "Ordinary list comprehension")
     ("ParComp" "Parallel list comprehension")
     ("ExpTypeSig" "Expression with explicit type signature")
     ("VarQuote" "'x for template haskell reifying of expressions")
     ("TypQuote" "''T for template haskell reifying of types")
     ("BracketExp" "Template haskell bracket expression")
     ("SpliceExp" "Template haskell splice expression")
     ("QuasiQuote" "Quasi-quotaion: [$name| string |]")
     ("XTag" "Xml element, with attributes and children")
     ("XETag" "Empty xml element, with attributes")
     ("XPcdata" "PCDATA child element")
     ("XExpTag" "Escaped haskell expression inside xml")
     ("XChildTag" "Children of an xml element")
     ("CorePragma" "CORE pragma")
     ("SCCPragma" "SCC pragma")
     ("GenPragma" "GENERATED pragma")
     ("Proc" "Arrows proc: proc pat -> exp")
     ("LeftArrApp" "Arrow application (from left): exp -< exp")
     ("RightArrApp" "Arrow application (from right): exp >- exp")
     ("LeftArrHighApp" "Higher-order arrow application (from left): exp -<< exp")
     ("RightArrHighApp" "Higher-order arrow application (from right): exp >>- exp"))

    ("Stmt" "A statement, representing both a stmt in a do-expression, an ordinary qual in a list comprehension, as well as a stmt in a pattern guard"
     ("Generator" "A generator: pat <- exp")
     ("Qualifier" "An exp by itself: in a do-expression, an action whose result is discarded; in a list comprehension and pattern guard, a guard expression")
     ("LetStmt" "Local bindings")
     ("RecStmt" "A recursive binding group for arrows"))

    ("QualStmt" "A general transqual in a list comprehension, which could potentially be a transform of the kind enabled by TransformListComp"
     ("QualStmt" "An ordinary statement")
     ("ThenTrans" "then exp")
     ("ThenBy" "then exp by exp")
     ("GroupBy" "then group by exp")
     ("GroupUsing" "then group using exp")
     ("GroupByUsing" "then group by exp using exp"))

    ("FieldUpdate" "An fbind in a labeled construction or update expression"
     ("FieldUpdate" "Ordinary label-expresion pair")
     ("FieldPun" "Record field pun")
     ("FieldWildcard" "Record field wildcard"))

    ("Alt" "An alt alternative in a case expression")

    ("GuardedAlts" "The right-hand sides of a case alternative, which may be a single right-hand side or a set of guarded ones"
     ("UnGuardedAlt" "-> exp")
     ("GuardedAlts" "gdpat"))

    ("GuardedAlt" "A guarded case alternative | stmts -> exp")

    ("Pat" "A pattern, to be matched against a value"
     ("PVar" "Variable")
     ("PLit" "Literal constant")
     ("PNeg" "Negated pattern")
     ("PNPlusK" "Integer n+k pattern")
     ("PInfixApp" "Pattern with an infix data constructor")
     ("PApp" "Data constructor and argument patterns")
     ("PTuple" "Tuple pattern")
     ("PList" "List pattern")
     ("PParen" "Parenthesized pattern")
     ("PRec" "Labelled pattern, record style")
     ("PAsPat" "@-pattern")
     ("PWildCard" "Wildcard pattern: _")
     ("PIrrPat" "Irrefutable pattern: ~pat")
     ("PatTypeSig" "Pattern with type signature")
     ("PViewPat" "View patterns of the form (exp -> pat)")
     ("PRPat" "Regular list pattern")
     ("PXTag" "XML element pattern")
     ("PXETag" "XML singleton element pattern")
     ("PXPcdata" "XML PCDATA pattern")
     ("PXPatTag" "XML embedded pattern")
     ("PXRPats" "XML regular list pattern")
     ("PExplTypeArg" "Explicit generics style type argument e.g. f {| Int |} x = .")
     ("PQuasiQuote" "String quasi quote pattern: [$name| string |]")
     ("PBangPat" "Strict (bang) pattern: f !x = .."))

    ("PatField" "An fpat in a labeled record pattern"
     ("PFieldPat" "Ordinary label-pattern pair")
     ("PFieldPun" "Record field pun")
     ("PFieldWildcard" "Record field wildcard"))

    ("Literal" "Literal Values of this type hold the abstract value of the literal, along with the precise string representation used. For example, 10, 0o12 and 0xa have the same value representation, but each carry a different string representation"
     ("Char" "Character literal")
     ("String" "String literal")
     ("Int" "Integer literal")
     ("Frac" "Floating point literal")
     ("PrimInt" "Unboxed integer literal")
     ("PrimWord" "Unboxed word literal")
     ("PrimFloat" "Unboxed float literal")
     ("PrimDouble" "Unboxed double literal")
     ("PrimChar" "Character literal")
     ("PrimString" "String literal"))

    ("ModuleName" "The name of a Haskell module")

    ("QName" "This type is used to represent qualified variables, and also qualified constructors"
     ("Qual" "Name qualified with a module name")
     ("UnQual" "Unqualified local name")
     ("Special" "Built-in constructor with special syntax"))

    ("Name" "This type is used to represent variables, and also constructors"
     ("Ident" "varid or conid")
     ("Symbol" "varsym or consym"))

    ("QOp" "Possibly qualified infix operators (qop), appearing in expressions"
     ("QVarOp" "Variable operator (qvarop)")
     ("QConOp" "Constructor operator (qconop)"))

    ("Op" "Operators appearing in infix declarations are never qualified"
     ("VarOp" "Variable operator (varop)")
     ("ConOp" "Constructor operator (conop)"))

    ("SpecialCon" "Constructors with special syntax. These names are never qualified, and always refer to builtin type or data constructors"
     ("UnitCon" "Unit type and data constructor ()")
     ("ListCon" "List type constructor []")
     ("FunCon" "Function type constructor ->")
     ("TupleCon" "N-ary tuple type and data constructors (,) etc, possibly boxed (#,#)")
     ("Cons" "Data constructor (:)")
     ("UnboxedSingleCon" "Unboxed singleton tuple constructor (# #)"))

    ("CName" "A name (cname) of a component of a class or data type in an import or export specification"
     ("VarName" "Name of a method or field")
     ("ConName" "Name of a data constructor"))

    ("IPName" "An implicit parameter name"
     ("IPDup" "?ident, non-linear implicit parameter")
     ("IPLin" "%ident, linear implicit parameter"))

    ("Bracket" "A template haskell bracket expression"
     ("ExpBracket" "Expression bracket: [| ... |]")
     ("PatBracket" "Pattern bracket: [p| ... |]")
     ("TypeBracket" "Type bracket: [t| ... |]")
     ("DeclBracket" "Declaration bracket: [d| ... |]"))

    ("Splice" "A template haskell splice expression"
     ("IdSplice" "Variable splice: $var")
     ("ParenSplice" "Parenthesised expression splice: $(exp)"))

    ("Safety" "The safety of a foreign function call"
     ("PlayRisky" "Unsafe")
     ("PlaySafe" "Safe (False) or threadsafe (True)")
     ("PlayInterruptible" "Interruptible"))

    ("CallConv" "The calling convention of a foreign function call")

    ("ModulePragma" "A top level options pragma, preceding the module header"
     ("LanguagePragma" "LANGUAGE pragma")
     ("OptionsPragma" "OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC")
     ("AnnModulePragma" "ANN pragma with module scope"))

    ("Rule" "The body of a RULES pragma")
    ("RuleVar" "Variables used in a RULES pragma, optionally annotated with types")
    ("Activation" "Activation clause of a RULES pragma")

    ("Annotation" "An annotation through an ANN pragma"
     ("Ann" "An annotation for a declared name")
     ("TypeAnn" "An annotation for a declared type")
     ("ModuleAnn" "An annotation for the defining module")))
  "Documentation describing every node type and every constructor in the AST")

(provide 'shm-ast-documentation)

;;; shm-ast-documentation.el ends here
