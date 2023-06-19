%{
#define YYINITDEPTH 50000 // Older bisons ignore YYMAXDEPTH
#define YYMAXDEPTH 50000

#include "cppvalue.h"
#include "xtendcode_p.h"
#include "xtendcode.h"
#include "message.h"

#include <stdio.h>
#include <stdlib.h>

int xtendcodeYYerror(yyscan_t yyscanner, const char *s)
{
  struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
  if (yyextra->currentMemberDef && yyextra->sourceFileDef)
  {
    printf("-> Xtend: Source Parser Error: %s inside member '%s' inside file '%s'.\n", s, yyextra->currentMemberDef->name().data(), yyextra->sourceFileDef->name().data());
  }
  else
  {
    if(yyextra->sourceFileDef)
      printf("-> Xtend: Source Parser Error: %s inside unknown member (probably a field initialization) inside file '%s'.\n", s, yyextra->sourceFileDef->name().data());
  
    else
      printf("-> Xtend: Source Parser Error: %s inside unknown member (probably a field initialization) inside unknown file.\n", s);
  }
  return -1;
}

static void startFontClass(yyscan_t yyscanner, const char *s)
{
  struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
  endFontClass(yyscanner);
  yyextra->code->startFontClass(s);
  yyextra->currentFontClass=s;
}

static void endFontClass(yyscan_t yyscanner)
{
  struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
  if (yyextra->currentFontClass)
  {
    yyextra->code->endFontClass();
    yyextra->currentFontClass=0;
  }
}

static void nextCodeLine(yyscan_t yyscanner)
{
  struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
  const char * fc = yyextra->currentFontClass;
  endCodeLine(yyscanner);
  if (yyextra->yyLineNr<yyextra->inputLines)
  {
    yyextra->currentFontClass = fc;
    startCodeLine(yyscanner);
  }
}

static void codifyLines(yyscan_t yyscanner,const char *text)
{
  struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
  //printf("codifyLines(%d,\"%s\")\n",yyextra->yyLineNr,text);
  const char *p=text,*sp=p;
  char c;
  bool done=FALSE;
  if (text == 0) done=TRUE;
  while (!done)
  {
    sp=p;
    while ((c=*p++) && c!='\n' && c!=';') { yyextra->yyColNr++; }
    if (c=='\n')
    {
      yyextra->yyLineNr++;
      yyextra->yyColNr=1;
      int l = (int)(p-sp-1);
      char *tmp = (char*)malloc(l+1);
      memcpy(tmp,sp,l);
      tmp[l]='\0';
      yyextra->code->codify(tmp);
      yyextra->codifiedPosition += l+1;
      free(tmp);
      nextCodeLine(yyscanner);
    }
    else if (c==';')
    {
      int l = (int)(p-sp-1);
      char *tmp = (char*)malloc(l+1);
      memcpy(tmp,sp,l);
      tmp[l]='\0';
      yyextra->code->codify(tmp);
      yyextra->code->endFontClass();
      yyextra->code->codify(";");
      yyextra->codifiedPosition += l+1;
      startFontClass(yyscanner, yyextra->currentFontClass);
      free(tmp);
    }
    else
    {
      yyextra->code->codify(sp);
      yyextra->codifiedPosition += strlen(sp);
      done=TRUE;
    }
  }
}

static void startCodeLine(yyscan_t yyscanner)
{
  struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
  if (yyextra->sourceFileDef && yyextra->lineNumbers)
  {
    const Definition *d = yyextra->sourceFileDef->getSourceDefinition(yyextra->yyLineNr);
    if (!yyextra->includeCodeFragment && d)
    {
      yyextra->currentDefinition = d;
      yyextra->currentMemberDef = yyextra->sourceFileDef->getSourceMember(yyextra->yyLineNr);
      QCString lineAnchor;
      lineAnchor.sprintf("l%05d",yyextra->yyLineNr);
      if (yyextra->currentMemberDef)
      {
        yyextra->code->writeLineNumber(yyextra->currentMemberDef->getReference(),
                                yyextra->currentMemberDef->getOutputFileBase(),
                                yyextra->currentMemberDef->anchor(),yyextra->yyLineNr);
        //setCurrentDoc(yyscanner,lineAnchor);
      }
      else if (d->isLinkableInProject())
      {
        yyextra->code->writeLineNumber(d->getReference(),
                                d->getOutputFileBase(),
                                0,yyextra->yyLineNr);
        //setCurrentDoc(yyscanner,lineAnchor);
      }
    }
    else
    {
      yyextra->code->writeLineNumber(0,0,0,yyextra->yyLineNr);
    }
  }
  yyextra->code->startCodeLine(yyextra->sourceFileDef && yyextra->lineNumbers);
  if (yyextra->currentFontClass)
  {
    yyextra->code->startFontClass(yyextra->currentFontClass);
  }
}

static void endCodeLine(yyscan_t yyscanner)
{
  struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
  //DBG_CTX((stderr,"endCodeLine(%d)\n",yyextra->yyLineNr));
  endFontClass(yyscanner);
  yyextra->code->endCodeLine();
}

static void codify(yyscan_t yyscanner, const char *text, const char *style)
{
  //printf("codifying: %s\n", text);
  struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
  
  if (!yyextra->comment.isEmpty())
  {
    //printf("codifying comment inbetween: %s\n", yyextra->comment);
    startFontClass(yyscanner, "comment");
    codifyLines(yyscanner, yyextra->comment);
    yyextra->comment = "";
  }
  
  startFontClass(yyscanner, style);
  codifyLines(yyscanner, text);
  endFontClass(yyscanner);
  yyextra->strToken = "";
}

%}

%name-prefix "xtendcodeYY"
%define api.pure full
%lex-param {yyscan_t yyscanner}
%parse-param {yyscan_t yyscanner}

%token TOK_STRING
%token TOK_CHAR
%token TOK_NUMLIT
%token TOK_BOOLEANLIT
%token TOK_NULLLIT
%token TOK_TYPEOF
%token TOK_CLASS
%token TOK_COMMONMODIFIER
%token TOK_METHODMODIFIER
%left TOK_CURLYOPEN
%token TOK_CURLYCLOSED
%token TOK_SQUAREOPEN
%token TOK_SQUARECLOSED
%token TOK_ROUNDOPEN
%token TOK_ROUNDCLOSED
%token TOK_HASH
%token TOK_VARID
%token TOK_TEMP
%token TOK_EXTENSION
%token TOK_ASSIGN
%token TOK_ASTERISK
%token TOK_AND
%token TOK_DOT
%token TOK_LESSTHAN
%token TOK_GREATERTHAN
%token TOK_COMMA
%token TOK_BAR
%token TOK_NULLSAFE
%token TOK_THREEDOTS
%token TOK_QUESTIONMARK
%token TOK_COLON
%token TOK_DOUBLECOLON
%token TOK_SEMICOLON
%token TOK_AS
%token TOK_INSTANCEOF
%token TOK_SHARP
%token TOK_NEW
%token TOK_IF
%token TOK_ELSE
%token TOK_SWITCH
%token TOK_CASE
%token TOK_DEFAULT
%token TOK_FOR
%token TOK_WHILE
%token TOK_DO
%token TOK_RETURN
%token TOK_THROW
%token TOK_TRY
%token TOK_CATCH
%token TOK_FINALLY
%token TOK_ABSTRACT
%token TOK_ANNOTATION
%token TOK_CREATE
%token TOK_DEF
%token TOK_DISPATCH
%token TOK_ENUM
%token TOK_EXTENDS
%token TOK_FINAL
%token TOK_ID
%token TOK_IMPLEMENTS
%token TOK_IMPORT
%token TOK_INTERFACE
%token TOK_NATIVE
%token TOK_OVERRIDE
%token TOK_PACKAGE
%token TOK_PRIVATE
%token TOK_PROTECTED
%token TOK_PUBLIC
%token TOK_STATIC
%token TOK_STRICTFP
%token TOK_SYNCHRONIZED
%token TOK_THROWS
%token TOK_TRANSIENT
%token TOK_VOLATILE
%token TOK_THIS
%token TOK_IT
%token TOK_SELF
%token TOK_SUPER
%token TOK_VAL
%token TOK_VAR
%token TOK_OPINFIX
%token TOK_OPPREFIX
%token TOK_OPPOSTFIX
%token TOK_DOUBLEARROW

%token TOK_TEMPBEGIN
%token TOK_TEMPPART
%token TOK_TEMPEND

%token TOK_TEMPIF
%token TOK_TEMPELSEIF
%token TOK_TEMPELSE
%token TOK_TEMPENDIF

%token TOK_TEMPFOR
%token TOK_TEMPBEFORE
%token TOK_TEMPSEPARATOR
%token TOK_TEMPAFTER
%token TOK_TEMPENDFOR

%%

Start:                Expression
                      | AssignT Expression
                      ;

Expressions:          Expression Expressions
                      | Expression
                      | ReturnT /* Expressions inside a block or a lambda can end with a single return statement */
                      ;

Expression:           SingleExpression OpInfixT Expression
                      | SingleExpression SpecialInfix Expression
                      | OpInfixT Expression
                      | SingleExpression QuestionmarkT Expression ColonT Expression
                      | SingleExpression AssignT Expression
                      | SingleExpression NullSafeT Expression
                      | SingleExpression DotT Expression
                      | SingleExpression DoubleColonT Expression
                      | SingleExpression ExpressionFollowed
                      | SingleExpression
                      ;

ExpressionFollowed:   InstanceofT JvmTypeReference ExpressionAdditional
                      | Cast ExpressionAdditional
                      | OpPostfixT ExpressionAdditional
                      ;

ExpressionAdditional: OpInfixT Expression
                      | SpecialInfix Expression
                      | QuestionmarkT Expression ColonT Expression
                      | AssignT Expression
                      | NullSafeT Expression
                      | DotT Expression
                      | DoubleColonT Expression
                      | ExpressionFollowed
                      |
                      ;

SingleExpression:     ExprInsideRound
                      | Literal
                      | Block
                      | VarDecl
                      | OpPrefixT Expression
                      | FeatureCall
                      | CtorCall
                      | Closure
                      | IfExpr
                      | SwitchExpr
                      | ForLoop
                      | WhileLoop
                      | DoWhileLoop
                      | ReturnExp
                      | ThrowExp
                      | TryCatchFinallyExpr
                      | SynchronizedExpr
                      | TemplateExpr
                      ;

Cast:                 AsT JvmTypeReference Cast
                      | AsT JvmTypeReference
                      ;

ExprInsideRound:      RoundOpenT Expression RoundClosedT
                      ;

Literal:              StringLiteralT
                      | CharLiteralT
                      | NumberLiteralT
                      | BooleanLiteralT
                      | NullLiteralT
                      | CollectionLiteral
                      | TypeLiteralT
                      ;

CollectionLiteral:    HashT SquareOpenT SquareClosedT
                      | HashT SquareOpenT CommaSepExpressions SquareClosedT
                      | HashT CurlyOpenT CurlyClosedT
                      | HashT CurlyOpenT CommaSepExpressions CurlyClosedT
                      ;

TypeLiteralT:         TypeofT RoundOpenT QualifiedName ArrayBracketsO RoundClosedT
                      ;

Block:                CurlyOpenT Expressions CurlyClosedT
                      | CurlyOpenT CurlyClosedT
                      ;

                      /*
                        Rule for variable declarations without type reference should be:
                            VarModifier ValidId
                        but is replaced by the rule:
                            VarModifier JvmTypeReference
                        to avoid conflicts.
                      */
VarDecl:              VarModifier JvmTypeReference
                      | VarModifier JvmTypeReference ValidId
                      ;

VarModifier:          FieldModifier
                      | FieldModifier ExtensionT
                      | ExtensionT FieldModifier
                      ;

FeatureCall:          SharpJvmArgumentTypeReferencesO IdOrSuper ClosureO
                      | SharpJvmArgumentTypeReferencesO IdOrSuper RoundOpenT RoundClosedT ClosureO
                      | SharpJvmArgumentTypeReferencesO IdOrSuper RoundOpenT Arguments RoundClosedT ClosureO
                      ;

ClosureO:             Closure
                      |
                      ;

CtorCall:             NewT QualifiedName SharpJvmArgumentTypeReferencesO ClosureO AnonymousClass
                      | NewT QualifiedName SharpJvmArgumentTypeReferencesO RoundOpenT RoundClosedT ClosureO AnonymousClass
                      | NewT QualifiedName SharpJvmArgumentTypeReferencesO RoundOpenT Arguments RoundClosedT ClosureO AnonymousClass
                      ;

// -------------------------------------------------------------
/* Anonymous class rules */
// -------------------------------------------------------------

AnonymousClass:       CurlyOpenT Members CurlyClosedT
                      |
                      ;

/* Members of anonymous classes */

Members:              CommonModifiersO Member Members
                      |
                      ;

Member:               FieldMemberFull
                      | MethodMember
                      | CtorMember
                      | ClassMember
                      | InterfaceMember
                      | EnumMember
                      | AnnotationMember
                      ;

/* Field member of an anonymous class */

FieldMemberFull:      FieldMember
                      | FieldMember AssignT Expression
                      ;

FieldMember:          FieldModifier CommonModifiersO JvmTypeReference
                      | FieldModifier CommonModifiersO JvmTypeReference ValidId
                      | ExtensionT CommonFieldMods JvmTypeReference
                      | ExtensionT CommonFieldMods JvmTypeReference ValidId
                      | FieldModifier CommonModifiersO ExtensionT CommonModifiersO JvmTypeReference
                      | FieldModifier CommonModifiersO ExtensionT CommonModifiersO JvmTypeReference ValidId
                      | JvmTypeReference ValidId
                      ;

CommonFieldMods:      CommonModifier CommonFieldMods
                      | FieldModifier CommonFieldMods
                      |
                      ;

/* Method member of an anonymous class */

MethodMember:         MethodModifier CommonMethodModsO SharpJvmTypeParametersO JvmTypeReference CreateExtensionInfo ValidId MemberParam MemberThrowsO MethodBodyO
                      | MethodModifier CommonMethodModsO SharpJvmTypeParametersO JvmTypeReference FunctionId MemberParam MemberThrowsO MethodBodyO
                      | MethodModifier CommonMethodModsO SharpJvmTypeParametersO CreateExtensionInfo ValidId MemberParam MemberThrowsO MethodBodyO
                      | MethodModifier CommonMethodModsO SharpJvmTypeParametersO FunctionId MemberParam MemberThrowsO MethodBodyO
                      ;

CommonMethodModsO:    CommonModifier CommonMethodModsO
                      | MethodModifier CommonMethodModsO
                      |
                      ;

CreateExtensionInfo:  CreateT Expression
                      | CreateT ValidId ColonT Expression
                      ;

MethodBodyO:          Block
                      | TemplateExpr
                      |
                      ;

/* Constructor member of an anonymous class */

CtorMember:           NewT SharpJvmTypeParametersO MemberParam MemberThrowsO Block;

MemberParam:          RoundOpenT RoundClosedT
                      | RoundOpenT Parameters RoundClosedT
                      ;

Parameters:           Parameter CommaT Parameters
                      | Parameter
                      ;

Parameter:            JvmTypeReference ValidId
                      | ExtensionT JvmTypeReference ValidId
                      | JvmTypeReference ThreeDotsT ValidId
                      | ExtensionT JvmTypeReference ThreeDotsT ValidId
                      ;

MemberThrowsO:        ThrowsT JvmTypeReferences
                      |
                      ;

/* Class member of an anonymous class */

ClassMember:          ClassT ValidId SharpJvmTypeParametersO ClassExtendsO ClassImplementsO CurlyOpenT Members CurlyClosedT
                      ;

ClassExtendsO:        ExtendsT JvmParameterizedTypeReference
                      |
                      ;

ClassImplementsO:     ImplementsT JvmParameterizedTypeReferences
                      |
                      ;

/* Interface member of an anonymous class */

InterfaceMember:      InterfaceT ValidId SharpJvmTypeParametersO InterfaceExtendsO CurlyOpenT Members CurlyClosedT
                      ;

InterfaceExtendsO:    ExtendsT JvmParameterizedTypeReferences
                      |
                      ;

/* Enum member of an anonymous class */

EnumMember:           EnumT ValidId CurlyOpenT EnumLiteralsO CurlyClosedT
                      ;

EnumLiteralsO:        ValidId CommaT EnumLiteralsO
                      | ValidId
                      |
                      ;

/* Annotation member of an anonymous class */

AnnotationMember:     AnnotationT ValidId CurlyOpenT Members CurlyClosedT
                      ;

// -------------------------------------------------------------
/* End of anonymous class rules */
// -------------------------------------------------------------

Closure:              SquareOpenT SquareClosedT
                      | SquareOpenT Expressions SquareClosedT
                      /*| SquareOpenT BarT Expressions SquareClosedT*/
                      /*| SquareOpenT JvmFormalParameters BarT Expressions SquareClosedT*/
                      | SquareOpenT JvmFormalParametersOrExpressions BarT Expressions SquareClosedT
                      | SquareOpenT BarT Expressions SquareClosedT
                      ;

JvmFormalParametersOrExpressions:
                      /*
                        The following two rules for a JvmFormalParameter:
                            ValidId
                            JvmTypeReference ValidId
                        are covered by a single expression and a single expression followed by ValidId respectively
                      */
                      Expression CommaT JvmFormalParametersOrExpressions /* Expression used instead of ValidId */
                      | Expression Expression CommaT JvmFormalParametersOrExpressions /* Expression ValidId used instead of JvmTypeReference ValidId */
                      | ExtensionT JvmTypeReference ValidId CommaT JvmFormalParametersOrExpressions /* True JvmFormalParameter */
                      | ExtensionT ValidId CommaT JvmFormalParametersOrExpressions /* True JvmFormalParameter */
                      | Expression
                      | Expression Expression
                      | ExtensionT JvmTypeReference ValidId
                      | ExtensionT ValidId
                      ;

Arguments:            ShortClosure
                      | CommaSepExpressions
                      ;

/* Use a reduced version of the JvmFormalParameters rule since some parts are already covered by rule CommaSepExpressions  */
ShortClosure:         /*JvmFormalParameters BarT Expression*/
                      JvmFormalParametersOrExpressions BarT Expression
                      | BarT Expression
                      ;

IfExpr:               IfT RoundOpenT Expression RoundClosedT Expression
                      | IfT RoundOpenT Expression RoundClosedT ReturnT ElseT Expression
                      | IfT RoundOpenT Expression RoundClosedT Expression ElseT Expression
                      ;

SwitchExpr:           SwitchT SwitchClause CurlyOpenT CasePartO DefaultPartO CurlyClosedT
                      ;

SwitchClause:         /*RoundOpenT JvmFormalParameter ColonT Expression RoundClosedT*/
                      RoundOpenT ExtensionT JvmTypeReference ValidId ColonT Expression RoundClosedT
                      | RoundOpenT ExtensionT ValidId ColonT Expression RoundClosedT
                      | RoundOpenT Expression ValidId ColonT Expression RoundClosedT
                      | RoundOpenT Expression ColonT Expression RoundClosedT
                      /*| JvmFormalParameter ColonT*/ 
                      | ExtensionT JvmTypeReference ValidId ColonT Expression
                      | ExtensionT ValidId ColonT Expression
                      | Expression ValidId ColonT Expression
                      | Expression ColonT Expression
                      | Expression
                      ;

CasePartO:            TypeGuardO CaseExpressionO ColonT Expression CasePartO
                      | TypeGuardO CaseExpressionO CommaT CasePartO
                      |
                      ;

TypeGuardO:           MultiTypeReference
                      |
                      ;

CaseExpressionO:      CaseT Expression
                      |
                      ;

DefaultPartO:         DefaultT ColonT Expression
                      | DefaultT ColonT ReturnT
                      |
                      ;

ForLoop:              ForEachLoop
                      | BasicForLoop
                      ;

ForEachLoop:          ForT RoundOpenT JvmFormalParameter ColonT Expression RoundClosedT Expression
                      ;

BasicForLoop:         ForT RoundOpenT CommaSepExpressions SemicolonT Expression SemicolonT CommaSepExpressions RoundClosedT Expression
                      | ForT RoundOpenT CommaSepExpressions SemicolonT Expression SemicolonT RoundClosedT Expression
                      | ForT RoundOpenT SemicolonT Expression SemicolonT CommaSepExpressions RoundClosedT Expression
                      | ForT RoundOpenT CommaSepExpressions SemicolonT SemicolonT CommaSepExpressions RoundClosedT Expression
                      | ForT RoundOpenT CommaSepExpressions SemicolonT SemicolonT RoundClosedT Expression
                      | ForT RoundOpenT SemicolonT Expression SemicolonT RoundClosedT Expression
                      | ForT RoundOpenT SemicolonT SemicolonT CommaSepExpressions RoundClosedT Expression
                      | ForT RoundOpenT SemicolonT SemicolonT RoundClosedT Expression
                      ;

CommaSepExpressions:  Expression CommaT CommaSepExpressions
                      | Expression
                      ;

WhileLoop:            WhileT RoundOpenT Expression RoundClosedT Expression
                      ;

DoWhileLoop:          DoT Expression WhileT RoundOpenT Expression RoundClosedT
                      ;

ReturnExp:            ReturnT Expression
                      ;

ThrowExp:             ThrowT NewT Expression
                      ;

TryCatchFinallyExpr:  TryClause Expression 
                      | TryClause Expression CatchClause
                      | TryClause Expression CatchClause FinallyT Expression
                      | TryClause Expression FinallyT Expression
                      ;

TryClause:            TryT RoundOpenT InitVarDeclaration RoundClosedT
                      | TryT
                      ;

InitVarDeclaration:   VarDecl AssignT Expression SemicolonT InitVarDeclaration
                      | VarDecl AssignT Expression SemicolonT
                      | VarDecl AssignT Expression
                      ;

SynchronizedExpr:     SynchronizedT RoundOpenT Expression RoundClosedT Expression
                      ;

CatchClause:          CatchT RoundOpenT FullJvmFormalParameter RoundClosedT Expression CatchClause
                      | CatchT RoundOpenT FullJvmFormalParameter RoundClosedT Expression
                      ;

TemplateExpr:         TempBeginT TemplateRecursive TempEndT
                      ;

TemplateRecursive:    TempPart Expression TemplateRecursive
                      | TempPart TemplateIf TemplateRecursive
                      | TempPart TemplateFor TemplateRecursive
                      | TempPart
                      |
                      ;

TempPart:             TempPartT TempPart
                      | TempPartT
                      ;

TemplateIf:           TempIfT Expression TemplateRecursive TemplateElseif TempEndifT
                      | TempIfT Expression TemplateRecursive TemplateElseif TempElseT TemplateRecursive TempEndifT
                      ;

TemplateElseif:       TempElseifT Expression TemplateRecursive TemplateElseif
                      |
                      ;

TemplateFor:          TempForT JvmFormalParameter ColonT Expression TemplateForSpecifier TemplateRecursive TempEndforT
                      ;

TemplateForSpecifier: TempBeforeT Expression TempSeparatorT Expression TempAfterT Expression
                      | TempBeforeT Expression TempSeparatorT Expression
                      | TempSeparatorT Expression TempAfterT Expression
                      | TempBeforeT Expression TempAfterT Expression
                      | TempBeforeT Expression
                      | TempSeparatorT Expression
                      | TempAfterT Expression
                      |
                      ;


// -------------------------------------------------------------
/* Helpers */
// -------------------------------------------------------------

/* keyword "import" is treated like a normal id */

ValidId:              IdT /*| CreateT | AnnotationT | TempAfterT | TempBeforeT | TempSeparatorT*/ | ThisT | ItT | SelfT | ClassT
                      ;

FunctionId:           ValidId | OpPrefixT | OpInfixT | OpPostfixT
                      ;

FeatureCallId:        InnerVarId /*| ExtensionT */
                      ;

IdOrSuper:            FeatureCallId | SuperT
                      ;

InnerVarId:           IdT | AbstractT | AnnotationT | ClassT | CreateT | DefT | DispatchT | EnumT | ExtendsT
	                    | FinalT | ImplementsT | InterfaceT | OverrideT | PackageT | PublicT | PrivateT
                  	  | ProtectedT | StaticT | ThrowsT | StrictfpT | NativeT | VolatileT /*| SynchronizedT*/ | TransientT
                      | TempAfterT | TempBeforeT | TempSeparatorT | ThisT | ItT | SelfT;


CommonModifiersO:     CommonModifier CommonModifiersO
                      |
                      ;

CommonModifier:       PublicT | PrivateT | ProtectedT | PackageT | AbstractT | StaticT | DispatchT | FinalT
                      | StrictfpT | NativeT | VolatileT | SynchronizedT | TransientT
                      ;

MethodModifier:       DefT | OverrideT
                      ;

FieldModifier:        ValT | VarT
                      ;

SpecialInfix:         LessThanT
                      | GreaterThanT
                      | AsteriskT
                      | DoubleArrowT
                      | LessThanT LessThanT
                      | GreaterThanT GreaterThanT
                      | LessThanT LessThanT LessThanT
                      | GreaterThanT GreaterThanT GreaterThanT
                      ;

// -------------------------------------------------------------
/* Java basic rules */
// -------------------------------------------------------------

/* Inside sharp brackets */
SharpJvmTypeParametersO:
                      LessThanT JvmTypeParameters GreaterThanT
                      |
                      ;

SharpJvmArgumentTypeReferencesO:
                      LessThanT JvmArgumentTypeReferences GreaterThanT
                      |
                      ;

/* Comma separated */
JvmParameterizedTypeReferences:
                      JvmParameterizedTypeReference CommaT JvmParameterizedTypeReferences
                      | JvmParameterizedTypeReference
                      ;

JvmTypeReferences:    JvmTypeReference CommaT JvmTypeReferences
                      | JvmTypeReference
                      ;

JvmArgumentTypeReferences:
                      JvmArgumentTypeReference CommaT JvmArgumentTypeReferences
                      | JvmArgumentTypeReference
                      ;

JvmTypeParameters:    JvmTypeParameter CommaT JvmTypeParameters
                      | JvmTypeParameter
                      ;

/*JvmFormalParameters:  JvmFormalParameter CommaT JvmFormalParameters
                      | JvmFormalParameter
                      ;
*/

/* Single rules */

JvmTypeReference:     JvmParameterizedTypeReference ArrayBracketsO
                      | FunctionTypeRef
                      ;

ArrayBracketsO:       SquareOpenT SquareClosedT ArrayBracketsO
                      |
                      ;

FunctionTypeRef:      RoundOpenT JvmTypeReferences RoundClosedT DoubleArrowT JvmTypeReference
                      | DoubleArrowT JvmTypeReference
                      ;

JvmParameterizedTypeReference:
                      QualifiedName
                      | QualifiedName LessThanT JvmArgumentTypeReferences GreaterThanT
                      | QualifiedName LessThanT JvmArgumentTypeReferences GreaterThanT JvmParameterizedTypeReferenceAdditional
                      ;

JvmParameterizedTypeReferenceAdditional:
                      DotT ValidId JvmParameterizedTypeReferenceAdditional
                      | DotT ValidId LessThanT JvmArgumentTypeReferences GreaterThanT JvmParameterizedTypeReferenceAdditional
                      | DotT ValidId
                      | DotT ValidId LessThanT JvmArgumentTypeReferences GreaterThanT
                      ;

JvmArgumentTypeReference:
                      JvmTypeReference
                      | JvmWildcardTypeReference
                      ;

JvmWildcardTypeReference:
                      QuestionmarkT
                      | QuestionmarkT JvmUpperBound
                      | QuestionmarkT JvmUpperBound JvmBoundAnded
                      | QuestionmarkT JvmLowerBound
                      | QuestionmarkT JvmLowerBound JvmBoundAnded
                      ;

JvmUpperBound:        ExtendsT JvmTypeReference
                      ;

JvmLowerBound:        SuperT JvmTypeReference
                      ;

JvmBoundAnded:        AndT JvmTypeReference JvmBoundAnded
                      | AndT JvmTypeReference
                      ;

JvmTypeParameter:     ValidId
                      | ValidId JvmUpperBound
                      | ValidId JvmUpperBound JvmBoundAnded
                      ;

QualifiedName:        ValidId DotT QualifiedName
                      | ValidId
                      | AsteriskT
                      ;

JvmFormalParameter:   ExtensionT JvmTypeReference ValidId
                      | JvmTypeReference ValidId
                      | ExtensionT ValidId
                      | ValidId
                      ;

FullJvmFormalParameter:
                      MultiTypeReference ValidId
                      | ExtensionT MultiTypeReference ValidId
                      ;

MultiTypeReference:   JvmTypeReference
                      | JvmTypeReference BarSepJvmTypeReferences
                      ;

BarSepJvmTypeReferences:
                      BarT JvmTypeReference BarSepJvmTypeReferences
                      | BarT JvmTypeReference
                      ;


// -------------------------------------------------------------
/* Terminal Symbols */
// -------------------------------------------------------------

StringLiteralT:       TOK_STRING
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "stringliteral");
                      }
NumberLiteralT:       TOK_NUMLIT
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
BooleanLiteralT:      TOK_BOOLEANLIT
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
NullLiteralT:         TOK_NULLLIT
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
TypeofT:              TOK_TYPEOF
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
CharLiteralT:         TOK_CHAR
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "charliteral");
                      }
                      ;
CurlyOpenT:           TOK_CURLYOPEN
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
CurlyClosedT:         TOK_CURLYCLOSED
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
RoundOpenT:           TOK_ROUNDOPEN
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
RoundClosedT:         TOK_ROUNDCLOSED
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
SquareOpenT:          TOK_SQUAREOPEN
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
SquareClosedT:        TOK_SQUARECLOSED
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
HashT:                TOK_HASH
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
ReturnT:              TOK_RETURN
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
ThrowT:               TOK_THROW
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
CatchT:               TOK_CATCH
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
TryT:                 TOK_TRY
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
FinallyT:             TOK_FINALLY
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
IdT:                  TOK_ID
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
ThisT:                TOK_THIS
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
ItT:                  TOK_IT
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
SelfT:                TOK_SELF
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
SuperT:               TOK_SUPER
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
ClassT:               TOK_CLASS
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
AbstractT:            TOK_ABSTRACT
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
AnnotationT:          TOK_ANNOTATION
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
CreateT:              TOK_CREATE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
DefT:                 TOK_DEF
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
DispatchT:            TOK_DISPATCH
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
EnumT:                TOK_ENUM
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
ExtendsT:             TOK_EXTENDS
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
FinalT:               TOK_FINAL
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
ImplementsT:          TOK_IMPLEMENTS
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
InterfaceT:           TOK_INTERFACE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
OverrideT:            TOK_OVERRIDE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
PackageT:             TOK_PACKAGE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
PublicT:              TOK_PUBLIC
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
PrivateT:             TOK_PRIVATE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
ProtectedT:           TOK_PROTECTED
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
StaticT:              TOK_STATIC
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
ThrowsT:              TOK_THROWS
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
StrictfpT:            TOK_STRICTFP
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
NativeT:              TOK_NATIVE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
VolatileT:            TOK_VOLATILE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
SynchronizedT:        TOK_SYNCHRONIZED
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
TransientT:           TOK_TRANSIENT
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
NewT:                 TOK_NEW
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
IfT:                  TOK_IF
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
ElseT:                TOK_ELSE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
SwitchT:              TOK_SWITCH
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
CaseT:                TOK_CASE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
DefaultT:             TOK_DEFAULT
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
ForT:                 TOK_FOR
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
WhileT:               TOK_WHILE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
DoT:                  TOK_DO
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
AssignT:              TOK_ASSIGN
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
LessThanT:            TOK_LESSTHAN
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                        yyextra->strToken = "";
                      }
                      ;
GreaterThanT:         TOK_GREATERTHAN
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                        yyextra->strToken = "";
                      }
                      ;
AsteriskT:            TOK_ASTERISK
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                        yyextra->strToken = "";
                      }
                      ;
AndT:                 TOK_AND
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                        yyextra->strToken = "";
                      }
                      ;
DotT:                 TOK_DOT
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                        yyextra->strToken = "";
                      }
                      ;
CommaT:               TOK_COMMA
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
BarT:                 TOK_BAR
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
NullSafeT:            TOK_NULLSAFE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
ThreeDotsT:           TOK_THREEDOTS
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
QuestionmarkT:        TOK_QUESTIONMARK
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
ColonT:               TOK_COLON
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                        yyextra->strToken = "";
                      }
                      ;
DoubleColonT:         TOK_DOUBLECOLON
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
SemicolonT:           TOK_SEMICOLON
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
OpInfixT:             TOK_OPINFIX
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
OpPrefixT:            TOK_OPPREFIX
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
OpPostfixT:           TOK_OPPOSTFIX
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
DoubleArrowT:         TOK_DOUBLEARROW
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "");
                      }
                      ;
AsT:                  TOK_AS
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
InstanceofT:          TOK_INSTANCEOF
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
ValT:                 TOK_VAL
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
VarT:                 TOK_VAR
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
ExtensionT:           TOK_EXTENSION
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keyword");
                      }
                      ;
TempBeginT:           TOK_TEMPBEGIN
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "stringliteral");
                      }
                      ;
TempPartT:            TOK_TEMPPART
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "stringliteral");
                      }
                      ;
TempEndT:             TOK_TEMPEND
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "stringliteral");
                      }
                      ;
TempIfT:              TOK_TEMPIF
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
TempElseifT:          TOK_TEMPELSEIF
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
TempElseT:            TOK_TEMPELSE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
TempEndifT:           TOK_TEMPENDIF
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
TempForT:             TOK_TEMPFOR
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
TempBeforeT:          TOK_TEMPBEFORE
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
TempSeparatorT:       TOK_TEMPSEPARATOR
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
TempAfterT:           TOK_TEMPAFTER
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;
TempEndforT:          TOK_TEMPENDFOR
                      {
                        struct xtendcodeYY_state* yyextra = xtendcodeYYget_extra(yyscanner);
                        codify(yyscanner, yyextra->strToken, "keywordflow");
                      }
                      ;