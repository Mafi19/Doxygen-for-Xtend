/*****************************************************************************
 *
 *
 *
 * Copyright (C) 1997-2021 by Dimitri van Heesch.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation under the terms of the GNU General Public License is hereby
 * granted. No representations are made about the suitability of this software
 * for any purpose. It is provided "as is" without express or implied warranty.
 * See the GNU General Public License for more details.
 *
 * Documents produced by Doxygen are derivative works derived from the
 * input used in their production; they are not affected by this license.
 *
 */
%option never-interactive
%option prefix="xtendscannerYY"
%option reentrant
%option extra-type="struct xtendscannerYY_state *"
%option noyywrap

%top{
#include <stdint.h>
}

%{

/*
 *      includes
 */

#include <algorithm>
#include <vector>
#include <utility>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <qregexp.h>

#include "config.h"
#include "xtendscanner.h"
#include "commentscan.h"
#include "entry.h"
#include "message.h"
#include "language.h"
#include "util.h"
#include "defargs.h"
#include "markdown.h"
#include "arguments.h"

#define YY_NO_INPUT 1
#define YY_NO_UNISTD_H 1

#define USE_STATE2STRING 0

struct xtendscannerYY_state
{
  OutlineParserInterface *thisParser;
  CommentScanner   commentScanner;
  const char *     inputString = 0;
  int              inputPosition = 0;
  int              lastContext = 0;
  int              lastCContext = 0;
  int              lastDocContext = 0;
  int              lastCPPContext = 0;
  int              lastSkipSharpContext = 0;
  int              lastSkipRoundContext = 0;
  int              lastStringContext = 0;
  int              lastCurlyContext = 0;
  int              lastRoundContext = 0;
  int              lastSquareContext = 0;
  int              lastInitializerContext = 0;
  int              lastClassTemplSpecContext = 0;
  int              lastPreLineCtrlContext = 0;
  int              lastSkipVerbStringContext = 0;
  int              lastCommentInArgContext = 0;
  int              lastRawStringContext = 0;
  int              lastCSConstraint = 0;
  int              lastHereDocContext = 0;
  int              lastDefineContext = 0;
  int              lastC11AttributeContext = 0;
  int              lastModifierContext = 0;
  Protection       protection = Public;
  Protection       baseProt = Public;
  int              sharpCount   = 0 ;
  int              roundCount   = 0 ;
  int              curlyCount   = 0 ;
  int              squareCount  = 0 ;
  int              padCount     = 0 ;
  std::shared_ptr<Entry> current;
  std::shared_ptr<Entry> current_root;
  std::shared_ptr<Entry> previous;
  std::shared_ptr<Entry> tempEntry;
  std::shared_ptr<Entry> firstTypedefEntry;
  std::shared_ptr<Entry> memspecEntry;
  int              yyLineNr     = 1 ;
  int              yyBegLineNr  = 1 ;
  int              yyColNr      = 1 ;
  int              yyBegColNr   = 1 ;
  QCString         yyFileName;
  MethodTypes      mtype = Method;
  bool             stat = false;
  Specifier        virt = Normal;
  Specifier        baseVirt = Normal;
  QCString         msType;
  QCString         msName;
  QCString         msArgs;
  bool             isTypedef = false;
  QCString         funcPtrType;
  QCString         templateStr;
  QCString         aliasName;
  QCString         baseName;
  QCString*        specName = 0;

  SrcLangExt       language = SrcLangExt_Unknown;

  bool             insideCppQuote = false;
  bool             insideProtocolList = false;

  int              argRoundCount = 0;
  int              argSharpCount = 0;
  int              currentArgumentContext = 0;
  int              lastCopyArgStringContext = 0;
  int              lastCopyArgContext = 0;
  QCString        *copyArgString = 0;
  QCString         fullArgString;
  QCString         dummyRawString;

  ArgumentList    *currentArgumentList = 0;
  char             lastCopyArgChar = '\0';

  QCString        *pCopyQuotedString = 0;
  QCString        *pCopyRoundString = 0;
  QCString        *pCopyCurlyString = 0;
  QCString        *pCopyRawString = 0;
  QGString        *pCopyCurlyGString = 0;
  QGString        *pCopyRoundGString = 0;
  QGString        *pCopySquareGString = 0;
  QGString        *pCopyQuotedGString = 0;
  QGString        *pCopyHereDocGString = 0;
  QGString        *pCopyRawGString = 0;
  QGString        *pSkipVerbString = 0;

  bool             insideFormula = false;
  bool             insideTryBlock = false;
  bool             insideCode = false;
  bool             needsSemi = false;

  int              initBracketCount = 0;

  QCString         oldStyleArgType;
  QCString         docBackup;
  QCString         briefBackup;

  int              docBlockContext = 0;
  QGString         docBlock;
  QCString         docBlockName;
  bool             docBlockInBody = false;
  bool             docBlockAutoBrief = false;
  char             docBlockTerm = '\0';

  QCString         idlAttr;
  QCString         idlProp;
  bool             odlProp = false;

  bool             lexInit = false;

  QCString         delimiter;

  int              column = 0;

  uint             fencedSize = 0;
  bool             nestedComment = false;
  std::vector< std::pair<Entry*,std::shared_ptr<Entry> > > outerScopeEntries;

  // Xtend:

  bool            accessModified = false;
  bool            insideXtendMethod = false;
  int             XtendFieldNames = 0;
  char            beginningChar = '\0';
  int             XtendSharpCount = 0;
  int             XtendRoundCount = 0;
  int             XtendSquareCount = 0;
  int             XtendCurlyCount = 0;
  bool            isTemplateMethod = 0;
  bool            extensionMember = FALSE;
  QCString        XtendInferredType = "noType";
  bool            XtendBlockInferredType = FALSE;
  bool            XtendForceBooleanType = FALSE;
  int             templateCount = 0;
  int             lastTemplateContext = 0;
  int             lastInnerTemplateContext = 0;
  bool            InnerTemplateString = FALSE;
  int             innerTemplatelastCommentContext = 0;
};

#if USE_STATE2STRING
static const char *stateToString(int state);
#endif
//-----------------------------------------------------------------------------

// forward declarations for stateless functions
static inline int computeIndent(const char *s,int startIndent);
static QCString stripQuotes(const char *s);
static char* extractClassName(char* fullName);

// forward declarations for stateful functions
static void addType(yyscan_t yyscanner);
static void initParser(yyscan_t yyscanner);
static void initEntry(yyscan_t yyscanner);
static void lineCount(yyscan_t yyscanner);
static void prependScope(yyscan_t yyscanner);
static void startCommentBlock(yyscan_t yyscanner,bool);
static void handleCommentBlock(yyscan_t yyscanner,const QCString &doc,bool brief);
static void handleParametersCommentBlocks(yyscan_t yyscanner,ArgumentList &al);
static yy_size_t yyread(yyscan_t yyscanner,char *buf,yy_size_t max_size);

/* ----------------------------------------------------------------- */
#undef  YY_INPUT
#define YY_INPUT(buf,result,max_size) result=yyread(yyscanner,buf,max_size);

%}

       /* start command character */
CMD       ("\\"|"@")
BN        [ \t\n\r]
BL        [ \t\r]*"\n"
B         [ \t]
ID        "\^"?"$"?[a-z_A-Z\x80-\xFF][a-z_A-Z0-9\x80-\xFF]*
SCOPENAME "$"?(({ID}?{BN}*("::"|"."){BN}*)*)(((~|!){BN}*)?{ID})
TSCOPE    {ID}("<"[a-z_A-Z0-9 \t\*\&,:]*">")?
CSSCOPENAME (({ID}?{BN}*"."{BN}*)*)((~{BN}*)?{ID})
PRE       [pP][rR][eE]
CODE      [cC][oO][dD][eE]
CHARLIT   (("'"\\[0-7]{1,3}"'")|("'"\\."'")|("'"[^'\\\n]{1,4}"'"))
IDLATTR   ("["[^\]]*"]"){BN}*
TYPEDEFPREFIX (("typedef"{BN}+)?)((("volatile"|"const"){BN}+)?)
RAWBEGIN  (u|U|L|u8)?R\"[^ \t\(\)\\]{0,16}"("
RAWEND    ")"[^ \t\(\)\\]{0,16}\"
ARITHOP   "+"|"-"|"/"|"*"|"%"|"--"|"++"
ASSIGNOP  "="|"*="|"/="|"%="|"+="|"-="|"<<="|">>="|"&="|"^="|"|="
LOGICOP   "=="|"!="|">"|"<"|">="|"<="|"&&"|"||"|"!"|"<=>"
BITOP     "&"|"|"|"^"|"<<"|">>"|"~"
OPERATOR  "operator"{B}*({ARITHOP}|{ASSIGNOP}|{LOGICOP}|{BITOP})

XTENDOPERATOR "+="|"-="|"||"|"&&"|"=="|"!="|"==="|"!=="|"<"|">"|"<="|">="|"->"|".."|">.."|"..<"|"=>"|"<<"|">>"|"<<<"|">>>"|"<>"|"?:"|"<=>"|"+"|"-"|"*"|"/"|"%"|"**"|"!"|"$"
XTENDOPERATORARITH "<<"|">>"|"<<<"|">>>"|"+"|"-"|"*"|"/"|"%"|"**"
XTENDOPERATORBOOL "||"|"&&"|"=="|"!="|"==="|"!=="|"<"|">"|"<="|">="

XTENDINT       ("-"{BN}*)?[0-9\_]+
XTENDINTHEX    ("-"{BN}*)?"0x"[a-fA-F0-9\_]+
XTENDLONG      ("-"{BN}*)?[0-9\_]+(\.[0-9\_]+)?[lL]
XTENDLONGHEX   ("-"{BN}*)?"0x"[a-fA-F0-9\_]+"#"[lL]
XTENDBIGINT    ("-"{BN}*)?[0-9\_]+(\.[0-9\_]+)?[bB][iI]
XTENDBIGINTHEX ("-"{BN}*)?"0x"[a-fA-F0-9\_]+"#"[bB][iI]

XTENDDOUBLEDEF ("-"{BN}*)?[0-9\_]+\.[0-9\_]+
XTENDDOUBLE    ("-"{BN}*)?[0-9\_]+("."[0-9\_]+)?[dD]
XTENDFLOAT     ("-"{BN}*)?[0-9\_]+("."[0-9\_]+)?[fF]
XTENDBIGDEC    ("-"{BN}*)?[0-9\_]+("."[0-9\_]+)?[bB][dD]

  /* language parsing states */

%x      Define
%x      DefineEnd
%x      CompoundName
%x      ClassVar
%x      ClassCategory
%x      ClassTemplSpec
%x      Bases
%x      BasesProt
%x      NextSemi
%x      EnumBaseType
%x      FindMembers
%x      FindMemberName
%x      FindFields
%x      FindFieldArg
%x      Function
%x      FuncRound
%x      ExcpRound
%x      ExcpList
%x      FuncQual
%x      TrailingReturn
%x      Operator
%x      Array
%x      ReadBody
%x      ReadNSBody
%x      ReadBodyIntf
%x      Using
%x      UsingAlias
%x      UsingAliasEnd
%x      UsingDirective
%x      SkipCurly
%x      SkipCurlyEndDoc
%x      SkipString
%x      SkipPHPString
%x      SkipInits
%x      SkipC11Inits
%x      SkipComment
%x      SkipCxxComment
%x      SkipCurlyBlock
%x      SkipRoundBlock
%x      SharpPre
%x      Sharp
%x      XtendRound
%x      SkipRound
%x      SkipSquare
%x      SkipRemainder
%x      DeclType
%x      TypedefName
%x      TryFunctionBlock
%x      TryFunctionBlockEnd
%x      Comment
%x      PackageName
%x      JavaImport
%x      CSAccessorDecl
%x      CSGeneric
%x      PreLineCtrl
%x      DefinePHP
%x      DefinePHPEnd
%x      SkipVerbString
%x      ReadInitializer
%x      UNOIDLAttributeBlock
%x      GetCallType
%x      CppQuote
%x      EndCppQuote
%x      EndTemplate
%x      FuncPtr
%x      FuncPtrOperator
%x      EndFuncPtr
%x      ReadFuncArgType
%x      ReadTempArgs
%x      NSAliasName
%x      NSAliasArg
%x      CopyString
%x      CopyGString
%x      CopyRound
%x      CopyCurly
%x      GCopyRound
%x      GCopySquare
%x      GCopyCurly
%x      Specialization
%x      SpecializationSingleQuote
%x      SpecializationDoubleQuote
%x      FuncPtrInit
%x      FuncFunc
%x      FuncFuncEnd
%x      FuncFuncType
%x      FuncFuncArray
%x      CopyArgString
%x      CopyArgPHPString
%x      CopyArgRound
%x      CopyArgSharp
%x      CopyArgComment
%x      CopyArgCommentLine
%x      CopyArgVerbatim
%x      HereDoc
%x      HereDocEnd
%x      CopyHereDoc
%x      CopyHereDocEnd
%x      RawString
%x      RawGString
%x      CSString

%x      IDLAttribute
%x      IDLProp
%x      IDLPropName

 /** Prototype scanner states */

%x      Prototype
%x      PrototypePtr
%x      PrototypeQual
%x      PrototypeExc
%x      PrototypeSkipLine

 /** comment parsing states */

%x      DocLine
%x      DocBlock
%x      DocCopyBlock

/** Xtend parsing states **/
%x      XtendPackageFinish
%x      XtendCreateMethod
%x      XtendCreateMethodConstr
%x      XtendCreateMethodConstrBracket
%x      XtendMethod
%x      XtendFindFieldInitOrEnd
%x      XtendInitializerEnd
%x      XtendInitSkipSharp
%x      XtendInitSkipRound
%x      XtendInitSkipRoundExpr
%x      XtendInitSkipSquare
%x      XtendInitSkipCurly
%x      XtendInitSkipTemplate
%x      CopyTemplate
%x      CopyTemplateComment
%x      CopyInnerTemplate
%x      CopyLineComment
%x      CopyComment
%x      SkipTemplate
%x      SkipTemplateComment
%x      SkipInnerTemplate

%%

<NextSemi>"{"                           {
                                          yyextra->curlyCount=0;
                                          yyextra->needsSemi = TRUE;
                                          BEGIN(SkipCurlyBlock);
                                        }
<NextSemi>"("                           {
                                          yyextra->roundCount=0;
                                          BEGIN(SkipRoundBlock);
                                        }
<SkipRoundBlock>"("                     {
                                          ++yyextra->roundCount;
                                        }
<SkipRoundBlock>")"                     {
                                          if (yyextra->roundCount )
                                            --yyextra->roundCount ;
                                          else
                                            BEGIN( NextSemi ) ;
                                        }
<SkipCurlyBlock>"{"                     {
                                          ++yyextra->curlyCount ;
                                        }
<SkipCurlyBlock>"}"                     {
                                          if( yyextra->curlyCount )
                                          {
                                            --yyextra->curlyCount ;
                                          }
                                          else if (yyextra->needsSemi)
                                          {
                                            BEGIN( NextSemi );
                                          }
                                          else
                                          {
                                            BEGIN( FindMembers );
                                          }
                                        }
<NextSemi>{CHARLIT}                     { }
<NextSemi>\'                            |
<NextSemi>\"                            {
                                          yyextra->lastStringContext=NextSemi;
                                          yyextra->beginningChar = yytext[0];
                                          BEGIN(SkipString);
                                        }
<NextSemi>[;,]                          {
                                          unput(*yytext);
                                          BEGIN( FindMembers );
                                        }
<EnumBaseType>[{;,]                     {
                                          yyextra->current->args = yyextra->current->args.simplifyWhiteSpace();
                                          unput(*yytext);
                                          BEGIN( ClassVar );
                                        }
<FindMembers,XtendMethod>"@"({ID}".")*{ID}{BN}*"(" {
                                          lineCount(yyscanner);
                                          yyextra->lastSkipRoundContext = YY_START;
                                          yyextra->roundCount=0;
                                          BEGIN( SkipRound );
                                        }
<FindMembers,XtendMethod>"@"{ID}        {
                                          // skip annotation
                                        }
<PackageName>{ID}(("."|"\\"){ID})*      {
                                          yyextra->isTypedef=FALSE;
                                          //printf("Found namespace %s lang=%d\n",yytext,yyextra->current->lang);
                                          yyextra->current->name = yytext;
                                          //yyextra->current->name = substitute(yyextra->current->name,".","::");
                                          //yyextra->current->name = substitute(yyextra->current->name,"\\","::");
                                          yyextra->current->name = substitute(yyextra->current->name,"^",""); // Xtend: escape character "^" shall be replaced
                                          yyextra->current->section = Entry::NAMESPACE_SEC;
                                          yyextra->current->type = "namespace" ;
                                          yyextra->current->fileName  = yyextra->yyFileName;
                                          yyextra->current->startLine = yyextra->yyLineNr;
                                          yyextra->current->startColumn = yyextra->yyColNr;
                                          yyextra->current->bodyLine  = yyextra->yyLineNr;
                                          yyextra->current->bodyColumn = yyextra->yyColNr;
                                          lineCount(yyscanner);
                                          BEGIN(XtendPackageFinish);
                                        }
<PackageName,XtendPackageFinish>{BN}*   {
                                          lineCount(yyscanner);
                                        }
<XtendPackageFinish>";"                 {
                                          std::shared_ptr<Entry> tmp = yyextra->current;
                                          yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
                                          yyextra->current_root = tmp;
                                          initEntry(yyscanner);
                                          BEGIN(FindMembers);
                                        }
<XtendPackageFinish>.                   {
                                          unput(*yytext);
                                          std::shared_ptr<Entry> tmp = yyextra->current;
                                          yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
                                          yyextra->current_root = tmp;
                                          initEntry(yyscanner);
                                          BEGIN(FindMembers);
                                        }
<XtendPackageFinish>"//"|"/*"           {
                                          unput(yytext[1]);
                                          unput(yytext[0]);
                                          std::shared_ptr<Entry> tmp = yyextra->current;
                                          yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
                                          yyextra->current_root = tmp;
                                          initEntry(yyscanner);
                                          BEGIN(FindMembers);
                                        }
<FindMembers,XtendMethod>{B}*"static"{BN}+ { yyextra->current->type += " static ";
                                          yyextra->current->stat = TRUE;
                                          lineCount(yyscanner);
                                        }
<FindMembers,XtendMethod>{B}*"abstract"{BN}+ {
                                          yyextra->current->type += " abstract ";
                                          yyextra->current->spec|=Entry::Abstract;
                                          lineCount(yyscanner);
                                        }
<FindMembers>{BN}*((("disp")?"interface")|"valuetype"){BN}+ { // M$/Corba/UNO IDL/Java/Slice interface
                                          lineCount(yyscanner);
                                          yyextra->isTypedef=FALSE;
                                          yyextra->current->section = Entry::CLASS_SEC;
                                          yyextra->current->spec = Entry::Interface |
                                              // preserve UNO IDL [optional], published, Slice local
                                              (yyextra->current->spec & (Entry::Optional|Entry::Published|Entry::Local));
                                          addType(yyscanner);
                                          yyextra->current->type += " interface" ;
                                          yyextra->current->fileName  = yyextra->yyFileName;
                                          yyextra->current->startLine = yyextra->yyLineNr;
                                          yyextra->current->startColumn = yyextra->yyColNr;
                                          yyextra->current->bodyLine  = yyextra->yyLineNr;
                                          yyextra->current->bodyColumn = yyextra->yyColNr;
                                          BEGIN( CompoundName );
                                        }
<FindMembers,XtendMethod>{BN}{1,80}     {
                                          lineCount(yyscanner);
                                        }
<FindMembers>{B}*"annotation"{BN}+      |
<FindMembers>{B}*"@interface"{BN}+      { // Java: attribute
                                          // Xtend: annotation
                                          lineCount(yyscanner);
                                          yyextra->isTypedef=FALSE;
                                          yyextra->current->section = Entry::CLASS_SEC;
                                          yyextra->current->protection = yyextra->protection = Public ;
                                          addType(yyscanner);
                                          QCString tmp=yytext;
                                          if (qstrcmp(removeRedundantWhiteSpace(tmp),"annotation")==0)
                                          {
                                            yyextra->current->type += " annotation" ;
                                            yyextra->current->spec = Entry::Annotation;
                                          }
                                          else
                                          {
                                            yyextra->current->type += " interface" ;
                                            yyextra->current->spec = Entry::Interface;
                                          }
                                          yyextra->current->fileName  = yyextra->yyFileName;
                                          yyextra->current->startLine = yyextra->yyLineNr;
                                          yyextra->current->startColumn = yyextra->yyColNr;
                                          yyextra->current->bodyLine  = yyextra->yyLineNr;
                                          yyextra->current->bodyColumn = yyextra->yyColNr;
                                          BEGIN( CompoundName );
                                        }
<FindMembers>{B}*{TYPEDEFPREFIX}"class{" |
<FindMembers>{B}*{TYPEDEFPREFIX}"class"{BN}+ {
                                          //printf("DEBUG:<FindMembers>%s\n", yytext);
                                          QCString decl = yytext;
                                          bool isConst=decl.find("const")!=-1;
                                          bool isVolatile=decl.find("volatile")!=-1;
                                          yyextra->current->section = Entry::CLASS_SEC;
                                          addType(yyscanner);
                                          uint64 spec = yyextra->current->spec;
                                          if (isConst)
                                          {
                                            yyextra->current->type += " const";
                                          }
                                          else if (isVolatile)
                                          {
                                            yyextra->current->type += " volatile";
                                          }
                                          yyextra->current->type += " class" ;
                                          yyextra->current->fileName  = yyextra->yyFileName;
                                          yyextra->current->startLine = yyextra->yyLineNr;
                                          yyextra->current->startColumn = yyextra->yyColNr;
                                          yyextra->current->bodyLine  = yyextra->yyLineNr;
                                          yyextra->current->bodyColumn = yyextra->yyColNr;
                                          lineCount(yyscanner) ;
                                          if (yytext[yyleng-1]=='{') unput('{');
                                          yyextra->accessModified = false; // Xtend
                                          BEGIN( CompoundName ) ;
                                        }
<FindMembers>{B}*{TYPEDEFPREFIX}{IDLATTR}?"enum"({BN}+("class"|"struct"))?"{" |
<FindMembers>{B}*{TYPEDEFPREFIX}{IDLATTR}?"enum"({BN}+("class"|"struct"))?{BN}+ { // for IDL: typedef [something] enum
                                          QCString text=yytext;
                                          bool isStrongEnum = text.find("struct")!=-1 || text.find("class")!=-1;
                                          yyextra->current->section = Entry::CLASS_SEC;
                                          yyextra->current->spec    = Entry::Enum;
                                          addType(yyscanner);
                                          yyextra->current->type += " enum";
                                          if (isStrongEnum)
                                          {
                                            yyextra->current->spec |= Entry::Strong;
                                          }
                                          yyextra->current->fileName  = yyextra->yyFileName;
                                          yyextra->current->startLine = yyextra->yyLineNr;
                                          yyextra->current->startColumn = yyextra->yyColNr;
                                          yyextra->current->bodyLine  = yyextra->yyLineNr;
                                          yyextra->current->bodyColumn = yyextra->yyColNr;
                                          lineCount(yyscanner) ;
                                          if (yytext[yyleng-1]=='{') unput('{');
                                          BEGIN( CompoundName ) ;
                                        }
<JavaImport>"static"|"extension"        {}
<JavaImport>({ID}{BN}*"."{BN}*)+"*"     { // package import => add as a using directive
                                          lineCount(yyscanner);
                                          QCString scope=yytext;
                                          yyextra->current->name=removeRedundantWhiteSpace(substitute(scope.left(scope.length()-1),".","::"));
                                          yyextra->current->fileName = yyextra->yyFileName;
                                          yyextra->current->section=Entry::USINGDIR_SEC;
                                          yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
                                          initEntry(yyscanner);
                                          BEGIN(Using);
                                        }
<JavaImport>({ID}{BN}*"."{BN}*)+{ID}    { // class import => add as a using declaration
                                          lineCount(yyscanner);
                                          QCString scope=yytext;
                                          yyextra->current->name=removeRedundantWhiteSpace(substitute(scope,".","::"));
                                          yyextra->current->fileName = yyextra->yyFileName;
                                          //printf("import name = %s -> %s\n",yytext,yyextra->current->name.data());
                                          yyextra->current->section=Entry::USINGDECL_SEC;
                                          yyextra->previous = yyextra->current;
                                          yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
                                          initEntry(yyscanner);
                                          BEGIN(Using);
                                        } 
                                        /*
<Using>({ID}{BN}*("::"|"."){BN}*)*({ID}|{OPERATOR}) {
                                          lineCount(yyscanner);
                                          yyextra->current->name=yytext;
                                          yyextra->current->fileName = yyextra->yyFileName;
                                          yyextra->current->section=Entry::USINGDECL_SEC;
                                          yyextra->current->startLine = yyextra->yyLineNr;
                                          yyextra->previous = yyextra->current;
                                          yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
                                          initEntry(yyscanner);
                                          BEGIN(Using);
                                        }
                                        */
<Using>{BN}*                            {
                                          lineCount(yyscanner);
                                        }
<Using>";"                              {
                                          BEGIN(FindMembers);
                                        }
<Using>.                                {
                                          unput(*yytext);
                                          BEGIN(FindMembers);
                                        }
<Using>"//"|"/*"                        {
                                          unput(yytext[1]);
                                          unput(yytext[0]);
                                          BEGIN(FindMembers);
                                        }
<ClassTemplSpec,EndTemplate>"<<"        {
                                          yyextra->current->name+=yytext;
                                          // *yyextra->currentTemplateSpec+=yytext;
                                        }
<ClassTemplSpec,EndTemplate>">>"        {
                                          unput('>');
                                          unput(' ');
                                          unput('>');
                                        }
<ClassTemplSpec,EndTemplate>"("         { yyextra->current->name+=*yytext;
                                          yyextra->roundCount++;
                                        }
<ClassTemplSpec,EndTemplate>")"         { yyextra->current->name+=*yytext;
                                          if (yyextra->roundCount>0) yyextra->roundCount--;
                                        }
<FindMembers,XtendMethod>{XTENDOPERATOR} |
<FindMembers,XtendMethod>{SCOPENAME}    {
                                          if (yytext[0] == '<')
                                          {
                                            if (qstrcmp(yytext,"<=>") != 0
                                                && qstrcmp(yytext,"<>") != 0
                                                && qstrcmp(yytext,"<=") != 0
                                                && qstrcmp(yytext,"<<") != 0
                                                && qstrcmp(yytext,"<<<") != 0) REJECT;
                                          }
                                          yyextra->yyBegColNr=yyextra->yyColNr;
                                          yyextra->yyBegLineNr=yyextra->yyLineNr;
                                          lineCount(yyscanner);
                                          addType(yyscanner);
                                          if (yyleng==6 && qstrcmp(yytext,"import")==0)
                                          {
                                            BEGIN(JavaImport);
                                          }
                                          else if (qstrcmp(yytext,"package")==0)
                                          {
                                            if (yyextra->current_root->name.isEmpty() == 1) // Xtend: package declaration has empty root
                                            {
                                              lineCount(yyscanner);
                                              BEGIN(PackageName);
                                            }
                                            else // Xtend: otherwise access modifier
                                            {
                                              yyextra->XtendFieldNames = 0;
                                              yyextra->current->protection = Package;
                                              yyextra->accessModified = true;
                                            }
                                          }
                                          else if (yyextra->insideTryBlock && qstrcmp(yytext,"catch")==0)
                                          {
                                            yyextra->insideTryBlock=FALSE;
                                            BEGIN(TryFunctionBlock);
                                          }
                                          else
                                          {
                                            addType(yyscanner);
                                            if (qstrcmp(yytext,"public")==0)
                                            {
                                              yyextra->XtendFieldNames = 0;
                                              yyextra->current->protection = Public;
                                              yyextra->accessModified = true;
                                            }
                                            else if (qstrcmp(yytext,"protected")==0)
                                            {
                                              yyextra->XtendFieldNames = 0;
                                              yyextra->current->protection = Protected;
                                              yyextra->accessModified = true;
                                            }
                                            else if (qstrcmp(yytext,"private")==0)
                                            {
                                              yyextra->XtendFieldNames = 0;
                                              yyextra->current->protection = Private;
                                              yyextra->accessModified = true;
                                            }
                                            else if (qstrcmp(yytext,"static")==0)
                                            {
                                              yyextra->XtendFieldNames = 0;
                                              yyextra->current->stat = TRUE;
                                            }
                                            else if (qstrcmp(yytext,"new")==0) // Xtend: constructor
                                            {
                                              yyextra->XtendFieldNames = 0;
                                              yyextra->current->name = extractClassName(yyextra->current_root->name.rawData());
                                              if (!yyextra->accessModified) // Xtend: constructors are public per default
                                                yyextra->current->protection = Public;
                                              else
                                                yyextra->accessModified = false;
                                              BEGIN(XtendMethod);
                                            }
                                            else if (qstrcmp(yytext,"def")==0 || qstrcmp(yytext,"override")==0) // Xtend: method
                                            {
                                              yyextra->XtendFieldNames = 0;
                                              if (yyextra->current->type.isEmpty())
                                                yyextra->current->type = yytext;
                                              else
                                                yyextra->current->type = yyextra->current->type + " " + yytext;
                                              if (!yyextra->accessModified) // Xtend: methods are public per default
                                                yyextra->current->protection = Public;
                                              else
                                                yyextra->accessModified = false;
                                              yyextra->insideXtendMethod = true;
                                              BEGIN(XtendMethod);
                                            }
                                            else if (qstrcmp(yytext,"dispatch")==0) // Xtend: dispatch methods
                                            {
                                              yyextra->XtendFieldNames = 0;
                                              if (yyextra->current->type.isEmpty())
                                                yyextra->current->type = yytext;
                                              else
                                                yyextra->current->type = yyextra->current->type + " " + yytext;
                                              yyextra->insideXtendMethod = true;
                                              BEGIN(XtendMethod); // Xtend: dispatch methods are considered to syntactically behave much like def methods
                                            }
                                            else if (qstrcmp(yytext,"create")==0)
                                            {
                                              yyextra->XtendFieldNames = 0;
                                              if (yyextra->current->type.isEmpty())
                                                yyextra->current->type = yytext;
                                              else
                                                yyextra->current->type = yyextra->current->type + " " + yytext;
                                              yyextra->insideXtendMethod = true;
                                              BEGIN(XtendCreateMethod); // Xtend: create methods are syntactically different so use different states to parse them
                                            }
                                            else if (qstrcmp(yytext,"var")==0 || qstrcmp(yytext,"val")==0 || qstrcmp(yytext,"strictfp")==0 || qstrcmp(yytext,"synchronized")==0
                                                    || qstrcmp(yytext,"volatile")==0 || qstrcmp(yytext,"transient")==0 || qstrcmp(yytext,"final")==0 || qstrcmp(yytext,"native")==0)
                                            {
                                              yyextra->XtendFieldNames = 0;
                                              if (yyextra->current->type.isEmpty())
                                                yyextra->current->type = yytext;
                                              else
                                                yyextra->current->type = yyextra->current->type + " " + yytext;
                                            }
                                            else if (qstrcmp(yytext,"extension")==0)
                                            {
                                              yyextra->XtendFieldNames = 0;
                                              yyextra->extensionMember = TRUE;
                                              if (yyextra->current->type.isEmpty())
                                                yyextra->current->type = yytext;
                                              else
                                                yyextra->current->type = yyextra->current->type + " " + yytext;
                                            }
                                            else
                                            {
                                              if (!yyextra->insideXtendMethod) {
                                                yyextra->XtendFieldNames++;
                                                //if (yyextra->extensionMember)
                                                //{
                                                //  if (yyextra->XtendFieldNames >= 1) // Xtend: this should probably be the next member
                                                //  {
                                                //    yyextra->extensionMember = FALSE;
                                                //    yyextra->XtendFieldNames = 0;
                                                //    BEGIN(XtendFindFieldInitOrEnd);
                                                //  }
                                                //}
                                                //else
                                                //{
                                                  if (yyextra->XtendFieldNames >= 2) // Xtend: this should probably be the next member
                                                  {
                                                    yyextra->XtendFieldNames = 0;
                                                    BEGIN(XtendFindFieldInitOrEnd);
                                                  }
                                                //}
                                              }

                                              if (yyextra->current->name.isEmpty())
                                                yyextra->current->name = yytext;
                                              else
                                                yyextra->current->name += yytext;

                                              if (yyextra->current->name.left(6)=="const ") // not used in Java or Xtend at the moment
                                              {
                                                if (yyextra->current->type.isEmpty())
                                                {
                                                  yyextra->current->type="const";
                                                }
                                                else
                                                {
                                                  yyextra->current->type+="const ";
                                                }
                                                yyextra->current->name=yyextra->current->name.mid(6);
                                              }
                                            }
                                          }
                                        }
<XtendCreateMethod>({SCOPENAME}{BN}*":"{BN}*)?"new"{BN}+ { // create method
                                          yyextra->yyBegColNr=yyextra->yyColNr;
                                          yyextra->yyBegLineNr=yyextra->yyLineNr;
                                          lineCount(yyscanner);
                                          addType(yyscanner);
                                          yyextra->current->name += yytext;
                                        }
<XtendCreateMethod>{SCOPENAME}{BN}*     { // This is always a high level data type/constructor
                                          yyextra->yyBegColNr=yyextra->yyColNr;
                                          yyextra->yyBegLineNr=yyextra->yyLineNr;
                                          lineCount(yyscanner);
                                          addType(yyscanner);
                                          yyextra->current->name += yytext;
                                          BEGIN(XtendCreateMethodConstr);
                                        }
<XtendCreateMethodConstr>{SCOPENAME}    { // No round constructor bracket found, example: def create new String method()
                                          // This must be the method name
                                          yyextra->yyBegColNr=yyextra->yyColNr;
                                          yyextra->yyBegLineNr=yyextra->yyLineNr;
                                          lineCount(yyscanner);
                                          addType(yyscanner);
                                          yyextra->current->name += yytext;
                                          BEGIN(XtendMethod);
                                        }
<XtendCreateMethodConstr>"("            { // Constructor bracket, add all until ')' is found
                                          // example: def create new Person("name") method()
                                          yyextra->yyBegColNr=yyextra->yyColNr;
                                          yyextra->yyBegLineNr=yyextra->yyLineNr;
                                          lineCount(yyscanner);
                                          addType(yyscanner);
                                          yyextra->current->name += yytext;
                                          BEGIN(XtendCreateMethodConstrBracket);
                                        }
<XtendCreateMethodConstrBracket>")"     { // End of constructor, scan method name in state FindMembers
                                          yyextra->yyBegColNr=yyextra->yyColNr;
                                          yyextra->yyBegLineNr=yyextra->yyLineNr;
                                          lineCount(yyscanner);
                                          addType(yyscanner);
                                          yyextra->current->name += yytext;
                                          BEGIN(XtendMethod);
                                        }
<XtendCreateMethodConstrBracket>{SCOPENAME}|. { // The scopename is parsed as a whole to increase performance
                                          yyextra->yyBegColNr=yyextra->yyColNr;
                                          yyextra->yyBegLineNr=yyextra->yyLineNr;
                                          lineCount(yyscanner);
                                          addType(yyscanner);
                                          yyextra->current->name += yytext;
                                        }
<DeclType>"("                           {
                                          yyextra->current->type+=yytext;
                                          yyextra->lastRoundContext=FindMembers;
                                          yyextra->pCopyRoundString=&yyextra->current->type;
                                          yyextra->roundCount=0;
                                          BEGIN(CopyRound);
                                        }
<DeclType>{BN}+                         { lineCount(yyscanner); }
<DeclType>.                             {
                                          unput(*yytext);
                                          BEGIN(FindMembers);
                                        }
<FindMembers,XtendMethod,XtendFindFieldInitOrEnd>[0-9]{ID} { // some number where we did not expect one
                                        }
<FindMembers,XtendMethod,XtendFindFieldInitOrEnd>"." {
                                          yyextra->current->name+=".";
                                        }
<FindMembers,XtendMethod,XtendFindFieldInitOrEnd>"::" {
                                          yyextra->current->name+=yytext;
                                        }
<CppQuote>"("{B}*"\""                   {
                                          yyextra->insideCppQuote=TRUE;
                                          BEGIN(FindMembers);
                                        }
<TryFunctionBlock>\n                    { lineCount(yyscanner); }
<TryFunctionBlock>"{"                   {
                                          yyextra->curlyCount=0;
                                          yyextra->lastCurlyContext = TryFunctionBlockEnd ;
                                          BEGIN( SkipCurly );
                                        }
<TryFunctionBlock>.
<TryFunctionBlockEnd>{BN}*"catch"       { lineCount(yyscanner); BEGIN(TryFunctionBlock); // {BN}* added to fix bug 611193
                                        }
<TryFunctionBlockEnd>\n                 { unput(*yytext); // rule added to fix bug id 601138
                                          BEGIN( FindMembers );
                                        }
<TryFunctionBlockEnd>.                  { unput(*yytext);
                                          BEGIN( FindMembers );
                                        }
<EndCppQuote>")"                        {
                                          yyextra->insideCppQuote=FALSE;
                                          BEGIN(FindMembers);
                                        }
<FindMembers,ReadBody,ReadNSBody,ReadBodyIntf,SkipCurly,XtendMethod,XtendFindFieldInitOrEnd>{B}*"#"{B}+[0-9]+{B}+/"\"" { /* line control directive */
                                          yyextra->yyLineNr = atoi(&yytext[1]);
                                          //printf("setting line number to %d\n",yyextra->yyLineNr);
                                          yyextra->lastPreLineCtrlContext = YY_START;
                                          if (YY_START==ReadBody ||
                                              YY_START==ReadNSBody ||
                                              YY_START==ReadBodyIntf)
                                          {
                                            yyextra->current->program+=yytext;
                                          }
                                          BEGIN( PreLineCtrl );
                                        }
<PreLineCtrl>"\""[^\n\"]*"\""           {
                                          yyextra->yyFileName = stripQuotes(yytext);
                                          if (yyextra->lastPreLineCtrlContext==ReadBody ||
                                              yyextra->lastPreLineCtrlContext==ReadNSBody ||
                                              yyextra->lastPreLineCtrlContext==ReadBodyIntf)
                                          {
                                            yyextra->current->program+=yytext;
                                          }
                                        }
<PreLineCtrl>.                          {
                                          if (yyextra->lastPreLineCtrlContext==ReadBody ||
                                              yyextra->lastPreLineCtrlContext==ReadNSBody ||
                                              yyextra->lastPreLineCtrlContext==ReadBodyIntf)
                                          {
                                            yyextra->current->program+=yytext;
                                          }
                                        }
<PreLineCtrl>\n                         {
                                          if (yyextra->lastPreLineCtrlContext==ReadBody ||
                                              yyextra->lastPreLineCtrlContext==ReadNSBody ||
                                              yyextra->lastPreLineCtrlContext==ReadBodyIntf)
                                          {
                                            yyextra->current->program+=yytext;
                                          }
                                          lineCount(yyscanner);
                                          BEGIN( yyextra->lastPreLineCtrlContext );
                                        }
<FindMembers,XtendMethod,XtendFindFieldInitOrEnd>[*&]+ {
                                          yyextra->current->name += yytext ;
                                          addType(yyscanner);
                                        }
<FindMembers,NextSemi,EnumBaseType,ReadInitializer,DefinePHPEnd,XtendMethod,XtendFindFieldInitOrEnd>";"{BN}*("/**"|"//!"|"/*!"|"///")"<" {
                                          //printf("DEBUG:<FindMembers,NextSemi,EnumBaseType,ReadInitializer,DefinePHPEnd>%s\n", yytext);
                                          if (yyextra->current->bodyLine==-1)
                                          {
                                            yyextra->current->bodyLine=yyextra->yyLineNr;
                                            yyextra->current->bodyColumn = yyextra->yyColNr;
                                          }
                                          yyextra->docBlockContext   = YY_START;
                                          yyextra->docBlockInBody    = FALSE;
                                          yyextra->docBlockAutoBrief = ( yytext[yyleng-2]=='*' && Config_getBool(JAVADOC_AUTOBRIEF) ) ||
                                                              ( yytext[yyleng-2]=='!' && Config_getBool(QT_AUTOBRIEF) );

                                          QCString indent;
                                          indent.fill(' ',computeIndent(yytext,yyextra->column));
                                          yyextra->docBlock=indent;
                                          //printf("indent=%d\n",computeIndent(yytext+1,yyextra->column));
                                          lineCount(yyscanner);

                                          yyextra->docBlockTerm = ';';
                                          if (YY_START==EnumBaseType && yyextra->current->section==Entry::ENUM_SEC)
                                          {
                                            yyextra->current->bitfields = ":"+yyextra->current->args;
                                            yyextra->current->args.resize(0);
                                            yyextra->current->section=Entry::VARIABLE_SEC;
                                          }
                                          if (yytext[yyleng-3]=='/')
                                          {
                                            startCommentBlock(yyscanner,TRUE);
                                            BEGIN( DocLine );
                                          }
                                          else
                                          {
                                            startCommentBlock(yyscanner,FALSE);
                                            BEGIN( DocBlock );
                                          }
                                        }
<FindFields,FindMembers,NextSemi,EnumBaseType,ReadInitializer,XtendMethod,XtendFindFieldInitOrEnd>","{BN}*("/**"|"//!"|"/*!"|"///")"<" {
                                          yyextra->docBlockContext   = YY_START;
                                          yyextra->docBlockInBody    = FALSE;
                                          yyextra->docBlockAutoBrief = ( yytext[yyleng-2]=='*' && Config_getBool(JAVADOC_AUTOBRIEF) ) ||
                                                              ( yytext[yyleng-2]=='!' && Config_getBool(QT_AUTOBRIEF) );

                                          QCString indent;
                                          indent.fill(' ',computeIndent(yytext,yyextra->column));
                                          yyextra->docBlock=indent;
                                          lineCount(yyscanner);

                                          yyextra->docBlockTerm = ',';
                                          if (YY_START==EnumBaseType && yyextra->current->section==Entry::ENUM_SEC)
                                          {
                                            yyextra->current->bitfields = ":"+yyextra->current->args;
                                            yyextra->current->args.resize(0);
                                            yyextra->current->section=Entry::VARIABLE_SEC;
                                          }
                                          if (yytext[yyleng-3]=='/')
                                          {
                                            startCommentBlock(yyscanner,TRUE);
                                            BEGIN( DocLine );
                                          }
                                          else
                                          {
                                            startCommentBlock(yyscanner,FALSE);
                                            BEGIN( DocBlock );
                                          }
                                        }
<DefineEnd,FindFields,FindFieldArg,ReadInitializer,XtendMethod,XtendFindFieldInitOrEnd>{BN}*("/**"|"//!"|"/*!"|"///")"<" {
                                          if (yyextra->current->bodyLine==-1)
                                          {
                                            yyextra->current->bodyLine=yyextra->yyLineNr;
                                            yyextra->current->bodyColumn = yyextra->yyColNr;
                                          }
                                          yyextra->docBlockContext   = YY_START;
                                          yyextra->docBlockInBody    = FALSE;
                                          yyextra->docBlockAutoBrief = ( yytext[yyleng-2]=='*' && Config_getBool(JAVADOC_AUTOBRIEF) ) ||
                                                              ( yytext[yyleng-2]=='!' && Config_getBool(QT_AUTOBRIEF) );
                                          QCString indent;
                                          indent.fill(' ',computeIndent(yytext,yyextra->column));
                                          yyextra->docBlock=indent;
                                          lineCount(yyscanner);

                                          yyextra->docBlockTerm = 0;
                                          if (yytext[yyleng-3]=='/')
                                          {
                                            startCommentBlock(yyscanner,TRUE);
                                            BEGIN( DocLine );
                                          }
                                          else
                                          {
                                            startCommentBlock(yyscanner,FALSE);
                                            BEGIN( DocBlock );
                                          }
                                        }

<FindMembers,FindFields,XtendMethod,XtendFindFieldInitOrEnd>("//"([!/]){B}*{CMD}"{")|("/*"([!*]){B}*{CMD}"{") {
                                          //handleGroupStartCommand(yyextra->current->name);
                                          if (yyextra->previous && yyextra->previous->section==Entry::GROUPDOC_SEC)
                                          {
                                            // link open command to the group defined in the yyextra->previous entry
                                            yyextra->commentScanner.open(yyextra->previous.get(),yyextra->yyFileName,yyextra->yyLineNr);
                                          }
                                          else
                                          {
                                            // link open command to the yyextra->current entry
                                            yyextra->commentScanner.open(yyextra->current.get(),yyextra->yyFileName,yyextra->yyLineNr);
                                          }
                                          //yyextra->current = tmp;
                                          initEntry(yyscanner);
                                          if (yytext[1]=='/')
                                          {
                                            if (yytext[2]=='!' || yytext[2]=='/')
                                            {
                                              yyextra->docBlockContext   = YY_START;
                                              yyextra->docBlockInBody    = FALSE;
                                              yyextra->docBlockAutoBrief = FALSE;
                                              yyextra->docBlock.resize(0);
                                              yyextra->docBlockTerm = 0;
                                              startCommentBlock(yyscanner,TRUE);
                                              BEGIN(DocLine);
                                            }
                                            else
                                            {
                                              yyextra->lastCContext=YY_START;
                                              BEGIN(SkipCxxComment);
                                            }
                                          }
                                          else
                                          {
                                            if (yytext[2]=='!' || yytext[2]=='*')
                                            {
                                              yyextra->docBlockContext   = YY_START;
                                              yyextra->docBlockInBody    = FALSE;
                                              yyextra->docBlock.resize(0);
                                              yyextra->docBlockAutoBrief = ( yytext[yyleng-2]=='*' && Config_getBool(JAVADOC_AUTOBRIEF) ) ||
                                                                  ( yytext[yyleng-2]=='!' && Config_getBool(QT_AUTOBRIEF) );
                                              yyextra->docBlockTerm = 0;
                                              startCommentBlock(yyscanner,FALSE);
                                              BEGIN(DocBlock);
                                            }
                                            else
                                            {
                                              yyextra->lastCContext=YY_START;
                                              BEGIN(SkipComment);
                                            }
                                          }
                                        }
<FindMembers,FindFields,ReadInitializer,XtendMethod,XtendFindFieldInitOrEnd>"//"([!/]){B}*{CMD}"}".*|"/*"([!*]){B}*{CMD}"}"[^*]*"*/"     {
                                          bool insideEnum = YY_START==FindFields || (YY_START==ReadInitializer && yyextra->lastInitializerContext==FindFields); // see bug746226
                                          yyextra->commentScanner.close(yyextra->current.get(),yyextra->yyFileName,yyextra->yyLineNr,insideEnum);
                                          lineCount(yyscanner);
                                        }
<FindMembers,XtendFindFieldInitOrEnd>"=" { // in PHP code this could also be due to "<?="
                                          //printf("DEBUG:EQUAL<FindMembers>%s\n", yytext);
                                          yyextra->XtendFieldNames = 0;
                                          yyextra->current->bodyLine = yyextra->yyLineNr;
                                          yyextra->current->bodyColumn = yyextra->yyColNr;
                                          yyextra->current->initializer = yytext;
                                          yyextra->lastInitializerContext = XtendFindFieldInitOrEnd;
                                          yyextra->initBracketCount=0;
                                          BEGIN(ReadInitializer);
                                        }
  /* Read initializer rules */
 /*
<ReadInitializer>"(" {
                                          yyextra->lastRoundContext=YY_START;
                                          yyextra->pCopyRoundGString=&yyextra->current->initializer;
                                          yyextra->roundCount=0;
                                          yyextra->current->initializer+=*yytext;
                                          BEGIN(GCopyRound);
                                        }
 */
 /*
<ReadInitializer>"{" {
                                          yyextra->lastCurlyContext=YY_START;
                                          yyextra->pCopyCurlyGString=&yyextra->current->initializer;
                                          yyextra->curlyCount=0;
                                          yyextra->current->initializer+=*yytext;
                                          BEGIN(GCopyCurly);
                                        }
*/
<XtendInitializerEnd>{BN}*              {
                                          yyextra->current->initializer+=yytext;
                                          lineCount(yyscanner);
                                        }
<XtendInitializerEnd>"++"|"--"          {
                                          yyextra->current->initializer+=yytext;
                                        }
<XtendInitializerEnd>"{"                | /* Xtend: anonymous class */
<XtendInitializerEnd>"["                { /* Xtend: method calls last parameter is a lambda */
                                          yyextra->XtendBlockInferredType = TRUE;
                                          unput(*yytext);
                                          BEGIN(ReadInitializer);
                                        }
<XtendInitializerEnd>"catch"{BN}*"("    { // Xtend: try catch finally
                                          yyextra->XtendBlockInferredType = TRUE;
                                          yyextra->current->initializer+=yytext;
                                          yyextra->XtendRoundCount = 1;
                                          BEGIN(XtendInitSkipRoundExpr);
                                        }
<XtendInitializerEnd>{XTENDOPERATORBOOL} {
                                          yyextra->current->initializer+=yytext;
                                          yyextra->XtendForceBooleanType = TRUE;
                                          BEGIN(ReadInitializer);
                                        }
<XtendInitializerEnd>{XTENDOPERATORARITH} {
                                          yyextra->current->initializer+=yytext;
                                          BEGIN(ReadInitializer);
                                        }
<XtendInitializerEnd>"?"|":"|"."        | /* Xtend: ternary operator */
<XtendInitializerEnd>"else"             |
<XtendInitializerEnd>"="                | /* Xtend: assignment */
<XtendInitializerEnd>"as"               | /* Xtend: type cast */
<XtendInitializerEnd>"finally"          | /* Xtend: try catch finally */
<XtendInitializerEnd>{XTENDOPERATOR}    { /* Xtend: operator */
                                          yyextra->current->initializer+=yytext;
                                          yyextra->XtendBlockInferredType = TRUE;
                                          // proceed with initializer parsing
                                          BEGIN(ReadInitializer);
                                        }
<XtendInitializerEnd><<EOF>>            |
<XtendInitializerEnd>("//"{B}*)?"/**"/[^/*] |
<XtendInitializerEnd>"///"/[^/]         |
<XtendInitializerEnd>"//!"              |
<XtendInitializerEnd>.                  |
<ReadInitializer>[;]                    { // finished initializer parsing
                                          //printf(">> initializer '%s' <<\n",yyextra->current->initializer.data());
                                          if (YY_START==ReadInitializer && (*yytext==';') && (yyextra->current_root->spec&Entry::Enum))
                                          {
                                            yyextra->current->fileName   = yyextra->yyFileName;
                                            yyextra->current->startLine  = yyextra->yyLineNr;
                                            yyextra->current->startColumn = yyextra->yyColNr;
                                            yyextra->current->args       = yyextra->current->args.simplifyWhiteSpace();
                                            yyextra->current->name       = yyextra->current->name.stripWhiteSpace();
                                            yyextra->current->section    = Entry::VARIABLE_SEC;
                                            yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
                                            initEntry(yyscanner);
                                            BEGIN(FindMembers);
                                          }
                                          else if (*yytext==';' || YY_START==XtendInitializerEnd || (yyextra->lastInitializerContext==FindFields && yyextra->initBracketCount==0)) // yyextra->initBracketCount==0 was added for bug 665778
                                          {
                                            if (yyextra->XtendForceBooleanType)
                                            {
                                              yyextra->XtendInferredType = " boolean";
                                            }
                                            if (!yyextra->XtendBlockInferredType && qstrcmp(yyextra->XtendInferredType, "noType") != 0)
                                            {
                                              yyextra->current->type += yyextra->XtendInferredType;
                                            }
                                            for(int i = 0; i < yyleng; i++)
                                              unput(yytext[yyleng - 1 -i]);
                                            BEGIN(yyextra->lastInitializerContext);
                                          }
                                          else
                                          {
                                            yyextra->current->initializer+=*yytext;
                                          }
                                          yyextra->XtendBlockInferredType = FALSE;
                                          yyextra->XtendForceBooleanType = FALSE;
                                          yyextra->XtendInferredType = "noType";
                                        }
<RawGString>{RAWEND}                    {
                                          *yyextra->pCopyRawGString+=yytext;
                                          QCString delimiter = yytext+1;
                                          delimiter=delimiter.left(delimiter.length()-1);
                                          if (delimiter==yyextra->delimiter)
                                          {
                                            BEGIN(yyextra->lastRawStringContext);
                                          }
                                        }
<RawGString>[^)\n]+                     {
                                          *yyextra->pCopyRawGString+=yytext;
                                        }
<RawGString>.                           {
                                          *yyextra->pCopyRawGString+=yytext;
                                        }
<RawGString>\n                          {
                                          *yyextra->pCopyRawGString+=yytext;
                                          lineCount(yyscanner);
                                        }
<RawString>{RAWEND}                     {
                                          *yyextra->pCopyRawString+=yytext;
                                          yyextra->fullArgString+=yytext;
                                          QCString delimiter = yytext+1;
                                          delimiter=delimiter.left(delimiter.length()-1);
                                          if (delimiter==yyextra->delimiter)
                                          {
                                            BEGIN(yyextra->lastRawStringContext);
                                          }
                                        }
<RawString>[^)]+                        {
                                          *yyextra->pCopyRawString+=yytext;
                                          yyextra->fullArgString+=yytext;
                                        }
<RawString>.                            {
                                          *yyextra->pCopyRawString+=yytext;
                                          yyextra->fullArgString+=yytext;
                                        }
<RawString>\n                           {
                                          *yyextra->pCopyRawString+=yytext;
                                          yyextra->fullArgString+=yytext;
                                          lineCount(yyscanner);
                                        }
<ReadInitializer>\'[^']\'               |
<ReadInitializer>\".\"                  { // single char
                                          yyextra->current->initializer+=yytext;
                                          if (yyextra->current->type.find("char")==-1 && yyextra->current->type.find("String")==-1)
                                          {
                                            if(qstrcmp(yyextra->XtendInferredType, "noType") == 0)
                                              yyextra->XtendInferredType =" char"; // trivial char assignment
                                            else
                                              yyextra->XtendInferredType =" String"; // cast operator separated literals to String
                                          }
                                          BEGIN(XtendInitializerEnd);
                                        }
<ReadInitializer>\'                     |
<ReadInitializer>\"                     {
                                          yyextra->lastStringContext=XtendInitializerEnd;
                                          yyextra->current->initializer+=yytext;
                                          yyextra->pCopyQuotedGString=&yyextra->current->initializer;
                                          if (yyextra->current->type.find("String")==-1)
                                            yyextra->XtendInferredType =" String";
                                          yyextra->beginningChar = *yytext;
                                          BEGIN(CopyGString);
                                        }
<ReadInitializer>\n                     {
                                          yyextra->current->initializer+=*yytext;
                                          lineCount(yyscanner);
                                        }
<SkipVerbString>[^\n"]+                 {
                                          *yyextra->pSkipVerbString+=yytext;
                                        }
<SkipVerbString>"\"\""                  { // quote escape
                                          *yyextra->pSkipVerbString+=yytext;
                                        }
<SkipVerbString>"\""                    {
                                          *yyextra->pSkipVerbString+=*yytext;
                                          BEGIN(yyextra->lastSkipVerbStringContext);
                                        }
<SkipVerbString>\n                      {
                                          *yyextra->pSkipVerbString+=*yytext;
                                          lineCount(yyscanner);
                                        }
<SkipVerbString>.                       {
                                          *yyextra->pSkipVerbString+=*yytext;
                                        }
<ReadInitializer>"true"                 |
<ReadInitializer>"false"                {
                                          yyextra->current->initializer+=yytext;
                                          if (yyextra->current->type.find("boolean")==-1)
                                          {
                                            if(qstrcmp(yyextra->XtendInferredType, "noType") == 0)
                                              yyextra->XtendInferredType =" boolean"; 
                                            else if(yyextra->XtendInferredType.find("char")!=-1)
                                              yyextra->XtendInferredType = " String";
                                          }
                                          BEGIN(XtendInitializerEnd);
                                        }
<ReadInitializer>"null"                 {
                                          yyextra->current->initializer+=yytext;
                                          BEGIN(XtendInitializerEnd);
                                        }
<ReadInitializer>{XTENDBIGINTHEX}       |
<ReadInitializer>{XTENDBIGINT}          {
                                          yyextra->current->initializer+=yytext;
                                          if (yyextra->current->type.find("BigInteger")==-1)
                                          {
                                            if(qstrcmp(yyextra->XtendInferredType, "noType") == 0)
                                              yyextra->XtendInferredType =" BigInteger";
                                            else if(yyextra->XtendInferredType.find("char")!=-1)
                                              yyextra->XtendInferredType = " String";
                                          }
                                          BEGIN(XtendInitializerEnd);
                                        }
<ReadInitializer>{XTENDLONGHEX}         |
<ReadInitializer>{XTENDLONG}            {
                                          yyextra->current->initializer+=yytext;
                                          if (yyextra->current->type.find("float")==-1 && yyextra->current->type.find("double")==-1 && yyextra->current->type.find("long")==-1)
                                          {
                                            if(qstrcmp(yyextra->XtendInferredType, "noType") == 0 || yyextra->XtendInferredType.find("int")!=-1)
                                              yyextra->XtendInferredType =" long";
                                            else if(yyextra->XtendInferredType.find("char")!=-1)
                                              yyextra->XtendInferredType = " String";
                                          }
                                          BEGIN(XtendInitializerEnd);
                                        }
<ReadInitializer>{XTENDBIGDEC}          {
                                          yyextra->current->initializer+=yytext;
                                          if (yyextra->current->type.find("BigDecimal")==-1)
                                          {
                                            if(qstrcmp(yyextra->XtendInferredType, "noType") == 0)
                                              yyextra->XtendInferredType =" BigDecimal";
                                            else if(yyextra->XtendInferredType.find("char")!=-1)
                                              yyextra->XtendInferredType = " String";
                                          }
                                          BEGIN(XtendInitializerEnd);
                                        }
<ReadInitializer>{XTENDFLOAT}           {
                                          yyextra->current->initializer+=yytext;
                                          if (yyextra->current->type.find("float")==-1 && yyextra->current->type.find("double")==-1)
                                          {
                                            if(qstrcmp(yyextra->XtendInferredType, "noType") == 0 || yyextra->XtendInferredType.find("int")!=-1 || yyextra->XtendInferredType.find("long")!=-1 || 
                                                yyextra->XtendInferredType.find("BigInteger")!=-1)
                                              yyextra->XtendInferredType =" float";
                                            else if(yyextra->XtendInferredType.find("char")!=-1)
                                              yyextra->XtendInferredType = " String";
                                          }
                                          BEGIN(XtendInitializerEnd);
                                        }
<ReadInitializer>{XTENDDOUBLEDEF}       |
<ReadInitializer>{XTENDDOUBLE}          {
                                          yyextra->current->initializer+=yytext;
                                          if (yyextra->current->type.find("double")==-1)
                                          {
                                            if(qstrcmp(yyextra->XtendInferredType, "noType") == 0 || yyextra->XtendInferredType.find("float")!=-1 || yyextra->XtendInferredType.find("int")!=-1 ||
                                                yyextra->XtendInferredType.find("long")!=-1 || yyextra->XtendInferredType.find("BigInteger")!=-1)
                                              yyextra->XtendInferredType =" double";
                                            else if(yyextra->XtendInferredType.find("char")!=-1)
                                              yyextra->XtendInferredType = " String";
                                          }
                                          BEGIN(XtendInitializerEnd);
                                        }
<ReadInitializer>{XTENDINTHEX}          |
<ReadInitializer>{XTENDINT}             {
                                          yyextra->current->initializer+=yytext;
                                          if (yyextra->current->type.find("int")==-1 && yyextra->current->type.find("long")==-1 && yyextra->current->type.find("float")==-1 && yyextra->current->type.find("double")==-1)
                                          {
                                            if(qstrcmp(yyextra->XtendInferredType, "noType") == 0)
                                              yyextra->XtendInferredType =" int";
                                            else if(yyextra->XtendInferredType.find("char")!=-1)
                                              yyextra->XtendInferredType = " String";
                                          }
                                          BEGIN(XtendInitializerEnd);
                                        }
<ReadInitializer>"throw"{BN}+"new"{BN}+{ID}{BN}*"<" |
<ReadInitializer>"new"{BN}+{ID}{BN}*"<" {
                                          lineCount(yyscanner);
                                          yyextra->XtendBlockInferredType = TRUE;
                                          yyextra->current->initializer+=yytext;
                                          yyextra->XtendSharpCount = 1;
                                          BEGIN(XtendInitSkipSharp);
                                        }
<ReadInitializer>"throw"{BN}+"new"{BN}+{ID}{BN}*"(" |
<ReadInitializer>"new"{BN}+{ID}{BN}*"(" {
                                          lineCount(yyscanner);
                                          yyextra->XtendBlockInferredType = TRUE;
                                          yyextra->current->initializer+=yytext;
                                          yyextra->XtendRoundCount = 1;
                                          BEGIN(XtendInitSkipRound);
                                        }
<ReadInitializer>"throw"{BN}+"new"{BN}+{ID} |
<ReadInitializer>"new"{BN}+{ID}         {
                                          lineCount(yyscanner);
                                          yyextra->XtendBlockInferredType = TRUE;
                                          yyextra->current->initializer+=yytext;
                                          BEGIN(XtendInitializerEnd);
                                        }
<ReadInitializer>"#"{BN}*[\[\{]         { // array
                                          lineCount(yyscanner);
                                          yyextra->XtendBlockInferredType = TRUE;
                                          yyextra->current->initializer+=yytext;
                                          if (yytext[yyleng-1]=='[')
                                          {
                                            yyextra->XtendSquareCount = 1;
                                            BEGIN(XtendInitSkipSquare);
                                          }
                                          else
                                          {
                                            yyextra->XtendCurlyCount = 1;
                                            BEGIN(XtendInitSkipCurly);
                                          }
                                        }
<ReadInitializer>"try"{BN}*"("          | /* try with resources expression */
<ReadInitializer>"if"{BN}*"("           { // if expression
                                          lineCount(yyscanner);
                                          yyextra->XtendBlockInferredType = TRUE;
                                          yyextra->current->initializer+=yytext;
                                          yyextra->XtendRoundCount = 1;
                                          BEGIN(XtendInitSkipRoundExpr);
                                        }
<ReadInitializer>"try"                  | /* try catch finally expression */
<ReadInitializer>"return"               | /* return expression */
<ReadInitializer>"switch"               { // switch expression
                                          yyextra->XtendBlockInferredType = TRUE;
                                          yyextra->current->initializer+=yytext;
                                        }
<ReadInitializer>{ID}({BN}*("."|"::"|"?."|"$"){BN}*{ID})*{BN}*[\(\[] {
                                          lineCount(yyscanner);
                                          yyextra->XtendBlockInferredType = TRUE;
                                          QCString tmp=yytext;
                                          if (qstrcmp(removeRedundantWhiteSpace(tmp), "true")==0 || qstrcmp(removeRedundantWhiteSpace(tmp), "false")==0)
                                            REJECT; // treat booleans with a special rule
                                          yyextra->current->initializer+=yytext;
                                          if (yytext[yyleng-1]=='(')
                                          {
                                            yyextra->XtendRoundCount = 1;
                                            BEGIN(XtendInitSkipRound);
                                          }
                                          else
                                          {
                                            yyextra->XtendSquareCount = 1;
                                            BEGIN(XtendInitSkipSquare);
                                          }
                                        }
<ReadInitializer>{ID}({BN}*("."|"::"|"?."|"$"){BN}*{ID})* {
                                          lineCount(yyscanner);
                                          yyextra->XtendBlockInferredType = TRUE;
                                          QCString tmp=yytext;
                                          if (qstrcmp(removeRedundantWhiteSpace(tmp), "true")==0 || qstrcmp(removeRedundantWhiteSpace(tmp), "false")==0)
                                            REJECT; // treat booleans with a special rule
                                          yyextra->current->initializer+=yytext;
                                          BEGIN(XtendInitializerEnd);
                                        }
<ReadInitializer>"("                    { // generic round bracket
                                          yyextra->XtendBlockInferredType = TRUE;
                                          yyextra->current->initializer+=*yytext;
                                          yyextra->XtendRoundCount = 1;
                                          BEGIN(XtendInitSkipRound);
                                        }
<ReadInitializer>"["                    { // lambda
                                          yyextra->XtendBlockInferredType = TRUE;
                                          yyextra->current->initializer+=*yytext;
                                          yyextra->XtendSquareCount = 1;
                                          BEGIN(XtendInitSkipSquare);
                                        }
<ReadInitializer>"{"                    { // block
                                          yyextra->XtendBlockInferredType = TRUE;
                                          yyextra->current->initializer+=*yytext;
                                          yyextra->XtendCurlyCount = 1;
                                          BEGIN(XtendInitSkipCurly);
                                        }
<ReadInitializer>\'\'\'                 {
                                          yyextra->current->initializer+=yytext;
                                          yyextra->XtendBlockInferredType = TRUE;
                                          BEGIN(XtendInitSkipTemplate);
                                        }
<ReadInitializer>{BN}+                  {
                                          yyextra->current->initializer+=yytext;
                                          lineCount(yyscanner);
                                        }
<ReadInitializer><<EOF>>                |
<ReadInitializer>.                      {
                                          unput(*yytext);
                                          BEGIN(XtendInitializerEnd);
                                        }
  /* Xtend: states for skipping expressions inside brackets */
<XtendInitSkipSharp>"<"                 {
                                          yyextra->current->initializer+=yytext;
                                          yyextra->XtendSharpCount++;
                                        }
<XtendInitSkipSharp>">"                 {
                                          yyextra->current->initializer+=yytext;
                                          if(--yyextra->XtendSharpCount == 0)
                                            BEGIN(XtendInitializerEnd);
                                        }
<XtendInitSkipSharp>{BN}+               {
                                          yyextra->current->initializer+=yytext;
                                          lineCount(yyscanner);
                                        }
<XtendInitSkipSharp>.                   {
                                          yyextra->current->initializer+=yytext;
                                        }
<XtendInitSkipRound,XtendInitSkipRoundExpr>"(" {
                                          yyextra->current->initializer+=yytext;
                                          yyextra->XtendRoundCount++;
                                        }
<XtendInitSkipRound,XtendInitSkipRoundExpr>")" {
                                          yyextra->current->initializer+=yytext;
                                          if(--yyextra->XtendRoundCount == 0)
                                          {
                                            if (YY_START == XtendInitSkipRound)
                                              BEGIN(XtendInitializerEnd);
                                            else
                                              BEGIN(ReadInitializer);
                                          }
                                        }
<XtendInitSkipRound,XtendInitSkipRoundExpr>{BN}+ {
                                          yyextra->current->initializer+=yytext;
                                          lineCount(yyscanner);
                                        }
<XtendInitSkipRound,XtendInitSkipRoundExpr>. {
                                          yyextra->current->initializer+=yytext;
                                        }
<XtendInitSkipSquare>"["                {
                                          yyextra->current->initializer+=yytext;
                                          yyextra->XtendSquareCount++;
                                        }
<XtendInitSkipSquare>"]"                {
                                          yyextra->current->initializer+=yytext;
                                          if(--yyextra->XtendSquareCount == 0)
                                            BEGIN(XtendInitializerEnd);
                                        }
<XtendInitSkipSquare>{BN}+              {
                                          yyextra->current->initializer+=yytext;
                                          lineCount(yyscanner);
                                        }
<XtendInitSkipSquare>.                  {
                                          yyextra->current->initializer+=yytext;
                                        }
<XtendInitSkipCurly>"{"                 {
                                          yyextra->current->initializer+=yytext;
                                          yyextra->XtendCurlyCount++;
                                        }
<XtendInitSkipCurly>"}"                 {
                                          yyextra->current->initializer+=yytext;
                                          if(--yyextra->XtendCurlyCount == 0)
                                            BEGIN(XtendInitializerEnd);
                                        }
<XtendInitSkipCurly>{BN}+               {
                                          yyextra->current->initializer+=yytext;
                                          lineCount(yyscanner);
                                        }
<XtendInitSkipCurly>.                   {
                                          yyextra->current->initializer+=yytext;
                                        }
<XtendInitSkipTemplate>{BN}+            {
                                          yyextra->current->initializer+=yytext;
                                          lineCount(yyscanner);
                                        }
<XtendInitSkipTemplate>\'\'\'           {
                                          yyextra->current->initializer+=yytext;
                                          BEGIN(XtendInitializerEnd);
                                        }
<XtendInitSkipTemplate>.                {
                                          yyextra->current->initializer+=yytext;
                                        }


  /* generic quoted string copy rules */
<CopyString>\\.                         {
                                          *yyextra->pCopyQuotedString+=yytext;
                                        }
<CopyString>\'                          |
<CopyString>\"                          {
                                          *yyextra->pCopyQuotedString+=*yytext;
                                          if (yyextra->beginningChar == *yytext) // Xtend: string must end with the same type of quote with which it started
                                            BEGIN( yyextra->lastStringContext );
                                        }
<CopyString>"/*"|"*/"|"//"              {
                                          *yyextra->pCopyQuotedString+=yytext;
                                        }
<CopyString>\n                          {
                                          *yyextra->pCopyQuotedString+=*yytext;
                                          lineCount(yyscanner);
                                        }
<CopyString>.                           {
                                          *yyextra->pCopyQuotedString+=*yytext;
                                        }

  /* generic quoted growable string copy rules */
<CopyGString>\\.                        {
                                          *yyextra->pCopyQuotedGString+=yytext;
                                        }
<CopyGString>\'                         |
<CopyGString>\"                         {
                                          *yyextra->pCopyQuotedGString+=*yytext;
                                          if (yyextra->beginningChar == *yytext) // Xtend: string must end with the same type of quote with which it started
                                            BEGIN( yyextra->lastStringContext );
                                        }
<CopyGString>"<?php"                    { // we had an odd number of quotes.
                                          *yyextra->pCopyQuotedGString += yytext;
                                          BEGIN( yyextra->lastStringContext );
                                        }
<CopyGString>"/*"|"*/"|"//" {
                                          *yyextra->pCopyQuotedGString+=yytext;
                                        }
<CopyGString>\n                         {
                                          lineCount(yyscanner);
                                          *yyextra->pCopyQuotedGString+=*yytext;
                                        }
<CopyGString>.                          {
                                          *yyextra->pCopyQuotedGString+=*yytext;
                                        }

  /* template copy rules */
<CopyTemplate>\\«                       |
<CopyTemplate>\\»                       {
                                          *yyextra->pCopyQuotedGString+=yytext;
                                        }
<CopyTemplate>\\.                       {
                                          *yyextra->pCopyQuotedGString+=yytext;
                                        }
<CopyTemplate>\'\'\'                    {
                                          *yyextra->pCopyQuotedGString+=yytext;
                                          BEGIN( yyextra->lastStringContext );
                                        }
<CopyTemplate>««                        {
                                          *yyextra->pCopyQuotedGString+="««";
                                          yyextra->lastTemplateContext = YY_START;
                                          BEGIN( CopyTemplateComment );
                                        }
<CopyTemplate>«                         {
                                          *yyextra->pCopyQuotedGString+="«";
                                          BEGIN( CopyInnerTemplate );
                                        }
<CopyTemplate>\n                        {
                                          lineCount(yyscanner);
                                          *yyextra->pCopyQuotedGString+=*yytext;
                                        }
<CopyTemplate>"/*"|"*/"|"//"            {
                                          *yyextra->pCopyQuotedGString+=yytext;
                                        }
<CopyTemplate>»                         {
                                          *yyextra->pCopyQuotedGString+=yytext;
                                        }
<CopyTemplate>[^«]                      {
                                          *yyextra->pCopyQuotedGString+=*yytext;
                                        }

  /* template comment copy rules */
<CopyTemplateComment>"/*"|"*/"|"//"     |
<CopyTemplateComment>\\.                {
                                          *yyextra->pCopyQuotedGString+=yytext;
                                        }
<CopyTemplateComment>\n                 {
                                          lineCount(yyscanner);
                                          *yyextra->pCopyQuotedGString+=*yytext;
                                          BEGIN(yyextra->lastTemplateContext);
                                        }
<CopyTemplateComment>.                  {
                                          *yyextra->pCopyQuotedGString+=*yytext;
                                        }

  /* inner template copy rules */
<CopyInnerTemplate>\"|\'                {
                                          if (yyextra->InnerTemplateString)
                                          {
                                            if(yyextra->beginningChar == *yytext)
                                            {
                                              yyextra->InnerTemplateString = FALSE;
                                            }
                                          }
                                          else
                                          {
                                            yyextra->InnerTemplateString = TRUE;
                                            yyextra->beginningChar = *yytext;
                                          }
                                          *yyextra->pCopyQuotedGString += *yytext;
                                        }
<CopyInnerTemplate>\n                   {
                                          lineCount(yyscanner);
                                          *yyextra->pCopyQuotedGString += *yytext;
                                        }
<CopyInnerTemplate>"//"                 {
                                          *yyextra->pCopyQuotedGString += yytext;
                                          if (!yyextra->InnerTemplateString)
                                          {
                                            yyextra->innerTemplatelastCommentContext = YY_START ;
                                            BEGIN(CopyLineComment);
                                          }
                                        }
<CopyInnerTemplate>"/*"{B}*             { 
                                          *yyextra->pCopyQuotedGString += yytext;
                                          if (!yyextra->InnerTemplateString)
                                          {
                                            yyextra->innerTemplatelastCommentContext = YY_START ;
                                            BEGIN( CopyComment ) ;
                                          }
                                        }
<CopyInnerTemplate>"/*"{BL}             { 
                                          lineCount(yyscanner);
                                          *yyextra->pCopyQuotedGString += yytext;
                                          if (!yyextra->InnerTemplateString)
                                          {
                                            yyextra->innerTemplatelastCommentContext = YY_START ;
                                            BEGIN( CopyComment ) ;
                                          }
                                        }
<CopyInnerTemplate>\'\'\'               {
                                          *yyextra->pCopyQuotedGString += yytext;
                                          if (!yyextra->InnerTemplateString)
                                          {
                                            if (yyextra->templateCount == 0)
                                            {
                                              yyextra->templateCount = 1;
                                            }
                                            else
                                            {
                                              yyextra->templateCount = 0;
                                            }
                                          }
                                        }
<CopyInnerTemplate>»                    {
                                          *yyextra->pCopyQuotedGString+="»";
                                          if (!yyextra->InnerTemplateString)
                                          {
                                            if (yyextra->templateCount == 0)
                                            {
                                              BEGIN(CopyTemplate);
                                            }
                                          }
                                        }
<CopyInnerTemplate>««                   {
                                          *yyextra->pCopyQuotedGString+="««";
                                          if (!yyextra->InnerTemplateString)
                                          {
                                            yyextra->lastTemplateContext = YY_START;
                                            BEGIN( CopyTemplateComment );
                                          }
                                        }
<CopyInnerTemplate>\\.                  {
                                          *yyextra->pCopyQuotedGString+=yytext;
                                        }
<CopyInnerTemplate>[^»]                 {
                                          *yyextra->pCopyQuotedGString+=*yytext;
                                        }
 /* Comments inside an inner template */
<CopyLineComment>\n                     {
                                          lineCount(yyscanner);
                                          *yyextra->pCopyQuotedGString+=*yytext;
                                          BEGIN(yyextra->innerTemplatelastCommentContext);
                                        }
<CopyLineComment>[^\n]                  {
                                          *yyextra->pCopyQuotedGString+=*yytext;
                                        }
<CopyComment>\n                         {
                                          lineCount(yyscanner);
                                          *yyextra->pCopyQuotedGString+=*yytext;
                                        }
<CopyComment>"*/"                       {
                                          *yyextra->pCopyQuotedGString+=yytext;
                                          BEGIN(yyextra->innerTemplatelastCommentContext);
                                        }
<CopyComment>[^\n]                      {
                                          *yyextra->pCopyQuotedGString+=*yytext;
                                        }


  /* generic round bracket list copy rules */
<CopyRound>\'                           |
<CopyRound>\"                           {
                                          *yyextra->pCopyRoundString+=*yytext;
                                          yyextra->pCopyQuotedString=yyextra->pCopyRoundString;
                                          yyextra->lastStringContext=YY_START;
                                          yyextra->beginningChar = *yytext;
                                          BEGIN(CopyString);
                                        }
<CopyRound>"("                          {
                                          *yyextra->pCopyRoundString+=*yytext;
                                          yyextra->roundCount++;
                                        }
<CopyRound>")"                          {
                                          *yyextra->pCopyRoundString+=*yytext;
                                          if (--yyextra->roundCount<0)
                                            BEGIN(yyextra->lastRoundContext);
                                        }
<CopyRound>\n                           {
                                          lineCount(yyscanner);
                                          *yyextra->pCopyRoundString+="\\n";
                                        }
<CopyRound>{CHARLIT}                    {
                                          *yyextra->pCopyRoundString+=yytext;
                                        }
<CopyRound>[^"'()\n,]+                  {
                                          *yyextra->pCopyRoundString+=yytext;
                                        }
<CopyRound>.                            {
                                          *yyextra->pCopyRoundString+=*yytext;
                                        }

  /* generic round bracket list copy rules for growable strings */
<GCopyRound>\'                          |
<GCopyRound>\"                          {
                                          *yyextra->pCopyRoundGString+=*yytext;
                                          yyextra->pCopyQuotedGString=yyextra->pCopyRoundGString;
                                          yyextra->lastStringContext=YY_START;
                                          yyextra->beginningChar = *yytext;
                                          BEGIN(CopyGString);
                                        }
<GCopyRound>"("                         {
                                          *yyextra->pCopyRoundGString+=*yytext;
                                          yyextra->roundCount++;
                                        }
<GCopyRound>")"                         {
                                          *yyextra->pCopyRoundGString+=*yytext;
                                          if (--yyextra->roundCount<0)
                                            BEGIN(yyextra->lastRoundContext);
                                        }
<GCopyRound>\n                          {
                                          lineCount(yyscanner);
                                          *yyextra->pCopyRoundGString+=*yytext;
                                        }
<GCopyRound>{CHARLIT}                   {
                                          *yyextra->pCopyRoundGString+=yytext;
                                        }
<GCopyRound>[^"'()\n\/,]+               {
                                          *yyextra->pCopyRoundGString+=yytext;
                                        }
<GCopyRound>.                           {
                                          *yyextra->pCopyRoundGString+=*yytext;
                                        }

  /* generic square bracket list copy rules for growable strings, we should only enter here in case of php, left the test part as in GCopyRound to keep it compatible with the round bracket version */
<GCopySquare>\'                         |
<GCopySquare>\"                         {
                                          *yyextra->pCopySquareGString+=*yytext;
                                            yyextra->pCopyQuotedGString=yyextra->pCopySquareGString;
                                          yyextra->lastStringContext=YY_START;
                                          yyextra->beginningChar = *yytext;
                                          BEGIN(CopyGString);
                                        }
<GCopySquare>"["                        {
                                          *yyextra->pCopySquareGString+=*yytext;
                                          yyextra->squareCount++;
                                        }
<GCopySquare>"]"                        {
                                          *yyextra->pCopySquareGString+=*yytext;
                                          if (--yyextra->squareCount<0)
                                          BEGIN(yyextra->lastSquareContext);
                                        }
<GCopySquare>\n                         {
                                          lineCount(yyscanner);
                                          *yyextra->pCopySquareGString+=*yytext;
                                        }
<GCopySquare>{CHARLIT}                  {
                                          *yyextra->pCopySquareGString+=yytext;
                                        }
<GCopySquare>[^"\[\]\n\/,]+              {
                                          *yyextra->pCopySquareGString+=yytext;
                                        }
<GCopySquare>.                          {
                                          *yyextra->pCopySquareGString+=*yytext;
                                        }

  /* generic curly bracket list copy rules */
<CopyCurly>\'                           |
<CopyCurly>\"                           {
                                          *yyextra->pCopyCurlyString+=*yytext;
                                          yyextra->pCopyQuotedString=yyextra->pCopyCurlyString;
                                          yyextra->lastStringContext=YY_START;
                                          yyextra->beginningChar=*yytext;
                                          BEGIN(CopyString);
                                        }
<CopyCurly>"{"                          {
                                          *yyextra->pCopyCurlyString+=*yytext;
                                          yyextra->curlyCount++;
                                        }
<CopyCurly>"}"                          {
                                          *yyextra->pCopyCurlyString+=*yytext;
                                          if (--yyextra->curlyCount<0)
                                            BEGIN(yyextra->lastCurlyContext);
                                        }
<CopyCurly>{CHARLIT}                    {
                                          *yyextra->pCopyCurlyString+=yytext;
                                        }
<CopyCurly>[^"'{}\/\n,]+                {
                                          *yyextra->pCopyCurlyString+=yytext;
                                        }
<CopyCurly>"/"                          { *yyextra->pCopyCurlyString+=yytext; }
<CopyCurly>\n                           {
                                          lineCount(yyscanner);
                                          *yyextra->pCopyCurlyString+=*yytext;
                                        }
<CopyCurly>.                            {
                                          *yyextra->pCopyCurlyString+=*yytext;
                                        }

  /* generic curly bracket list copy rules for growable strings */
<GCopyCurly>^"#"{B}+[0-9]+{B}+"\""[^\"\n]+"\""{B}+"1"{B}*\n? { // start of included file marker
                                        }
<GCopyCurly>^"#"{B}+[0-9]+{B}+"\""[^\"\n]+"\""{B}+"2"{B}*\n? { // end of included file marker
                                          QCString line = QCString(yytext);
                                          int s = line.find(' ');
                                          int e = line.find('"',s);
                                          yyextra->yyLineNr = line.mid(s,e-s).toInt();
                                          if (yytext[yyleng-1]=='\n')
                                          {
                                            lineCount(yyscanner);
                                            yyextra->column=0;
                                          }
                                        }
<GCopyCurly>\'                          |
<GCopyCurly>\"                          {
                                          *yyextra->pCopyCurlyGString+=*yytext;
                                          yyextra->pCopyQuotedGString=yyextra->pCopyCurlyGString;
                                          yyextra->lastStringContext=YY_START;
                                          yyextra->beginningChar=*yytext;
                                          BEGIN(CopyGString);
                                        }
<GCopyCurly>"{"                         {
                                          *yyextra->pCopyCurlyGString+=*yytext;
                                          yyextra->curlyCount++;
                                        }
<GCopyCurly>"}"                         {
                                          *yyextra->pCopyCurlyGString+=*yytext;
                                          if (--yyextra->curlyCount<0)
                                            BEGIN(yyextra->lastCurlyContext);
                                        }
<GCopyCurly>{CHARLIT}                    {
                                          *yyextra->pCopyCurlyGString+=yytext;
                                        }
<GCopyCurly>[^"'{}\/\n,]+               {
                                          *yyextra->pCopyCurlyGString+=yytext;
                                        }
<GCopyCurly>[,]+                        {
                                          *yyextra->pCopyCurlyGString+=yytext;
                                        }
<GCopyCurly>"/"                         { *yyextra->pCopyCurlyGString+=yytext; }
<GCopyCurly>\n                          {
                                          lineCount(yyscanner);
                                          *yyextra->pCopyCurlyGString+=*yytext;
                                        }
<GCopyCurly>.                           {
                                          *yyextra->pCopyCurlyGString+=*yytext;
                                        }

  /* ---------------------- */


<EnumBaseType>.                         {
                                          yyextra->current->args+=*yytext;
                                        }
<EnumBaseType>\n                        {
                                          lineCount(yyscanner);
                                          yyextra->current->args+=' ';
                                        }
<XtendFindFieldInitOrEnd>{BN}*           {
                                          lineCount(yyscanner);
                                        }
<XtendFindFieldInitOrEnd>("//"{B}*)?"/**"/[^/*] |
<XtendFindFieldInitOrEnd>"///"/[^/]     |
<XtendFindFieldInitOrEnd>"//!"          |
<FindMembers,XtendFindFieldInitOrEnd>[;,] |
<XtendFindFieldInitOrEnd><<EOF>>        |
<XtendFindFieldInitOrEnd>.              {
                                          if (YY_START == XtendFindFieldInitOrEnd && yyleng==1 && *yytext != ';' && *yytext != ',')
                                            unput(*yytext);
                                          else if (yytext[0]=='/')
                                          {
                                            for(int i = 0; i < yyleng; i++)
                                              unput(yytext[yyleng - 1 -i]);
                                          }
                                          yyextra->XtendFieldNames = 0; // Xtend: reset variable
                                          //printf("DEBUG:<FindMembers>%s\n", yytext);
                                          if (!yyextra->accessModified && !(yyextra->current_root->spec & Entry::Interface)) // Xtend: Fields are private per default
                                            yyextra->current->protection = Private;
                                          else
                                            yyextra->accessModified = false;

                                          QCString oldType = yyextra->current->type;
                                          if (yyextra->current->bodyLine==-1)
                                          {
                                            yyextra->current->bodyLine = yyextra->yyLineNr;
                                            yyextra->current->bodyColumn = yyextra->yyColNr;
                                          }
                                          bool stat = yyextra->current->stat;
                                          if (!yyextra->current->name.isEmpty() && yyextra->current->section!=Entry::ENUM_SEC)
                                          {
                                            yyextra->current->type=yyextra->current->type.simplifyWhiteSpace();
                                            yyextra->current->args=removeRedundantWhiteSpace(yyextra->current->args);
                                            yyextra->current->name=yyextra->current->name.stripWhiteSpace();
                                            if (yyextra->current->section==Entry::CLASS_SEC) // remove spec for "struct Bla bla;"
                                            {
                                                yyextra->current->spec = 0;
                                            }
                                            yyextra->current->section = Entry::VARIABLE_SEC ;
                                            yyextra->current->fileName = yyextra->yyFileName;
                                            yyextra->current->startLine = yyextra->yyBegLineNr;
                                            yyextra->current->startColumn = yyextra->yyBegColNr;
                                            yyextra->current_root->moveToSubEntryAndRefresh( yyextra->current ) ;
                                            initEntry(yyscanner);
                                          }
                                          if ( *yytext == ',')
                                          {
                                            yyextra->current->stat = stat; // the static attribute holds for all variables
                                            yyextra->current->name.resize(0);
                                            yyextra->current->args.resize(0);
                                            yyextra->current->brief.resize(0);
                                            yyextra->current->doc.resize(0);
                                            yyextra->current->initializer.resize(0);
                                            yyextra->current->bitfields.resize(0);
                                            int i=oldType.length();
                                            while (i>0 && (oldType[i-1]=='*' || oldType[i-1]=='&' || oldType[i-1]==' ')) i--;
                                            yyextra->current->type = oldType.left(i);
                                          }
                                          else
                                          {
                                            yyextra->mtype = Method;
                                            yyextra->virt = Normal;
                                            yyextra->current->groups.clear();
                                            initEntry(yyscanner);
                                          }
                                          BEGIN(FindMembers); // Xtend: proceed with regular java member parsing
                                        }
<FindMembers,XtendMethod,XtendFindFieldInitOrEnd>"[" {
                                          if (yyextra->current->name.isEmpty()) // IDL function property
                                          {
                                            yyextra->squareCount=1;
                                            yyextra->lastSquareContext = YY_START;
                                            yyextra->idlAttr.resize(0);
                                            yyextra->idlProp.resize(0);
                                            yyextra->current->mtype = yyextra->mtype;

                                            if (Config_getBool(IDL_PROPERTY_SUPPORT) &&
                                                 yyextra->current->mtype == Property)
                                            { // we are yyextra->inside the properties section of a dispinterface
                                              yyextra->odlProp = true;
                                              yyextra->current->spec |= Entry::Gettable;
                                              yyextra->current->spec |= Entry::Settable;
                                            }

                                            BEGIN( IDLAttribute );
                                          }
                                          else
                                          {
                                            yyextra->current->args += yytext ;
                                            yyextra->squareCount=1;
                                            BEGIN( Array ) ;
                                          }
                                        }
<Array>"]"                              { yyextra->current->args += *yytext ;
                                          if (--yyextra->squareCount<=0)
                                             BEGIN( FindMembers ) ;
                                        }
<FuncFuncArray>"]"                      { yyextra->current->args += *yytext ;
                                          if (--yyextra->squareCount<=0)
                                             BEGIN( Function ) ;
                                        }
<Array,FuncFuncArray>"["                { yyextra->current->args += *yytext ;
                                          yyextra->squareCount++;
                                        }
<Array,FuncFuncArray>.                  { yyextra->current->args += *yytext ; }
<SkipSquare>"["                         { yyextra->squareCount++; }
<SkipSquare>"]"                         {
                                          if (--yyextra->squareCount<=0)
                                            BEGIN( yyextra->lastSquareContext );
                                        }
<SkipSquare>\'                          |
<SkipSquare>\"                          {
                                          yyextra->lastStringContext=YY_START;
                                          yyextra->beginningChar=yytext[0];
                                          BEGIN( SkipString );
                                        }
<SkipSquare>[^\n\[\]\"]+
<FindMembers,XtendMethod>"=>"           {
                                          yyextra->current->type += yytext;
                                        }
<FindMembers,XtendMethod>"<"            {
                                          BEGIN( SharpPre ) ;
                                        }
<FindMembers>"("                        {
                                          addType(yyscanner);
                                          if (yyextra->current->type.isEmpty())
                                          {
                                            yyextra->current->type = yytext;
                                            yyextra->current->type = " ";
                                          }
                                          else
                                          {
                                            yyextra->current->type += ' ';
                                            yyextra->current->type += yytext;
                                          }
                                          yyextra->roundCount++;
                                          BEGIN( XtendRound ) ;
                                        }
<SharpPre>{BN}+                         {
                                          yyextra->current->type += ' ';
                                          lineCount(yyscanner);
                                        }
<SharpPre>"("                           {
                                          yyextra->yyBegColNr=yyextra->yyColNr;
                                          yyextra->yyBegLineNr=yyextra->yyLineNr;
                                          lineCount(yyscanner);
                                          addType(yyscanner);
                                          if (yyextra->current->name.isEmpty())
                                            yyextra->current->name = "<";
                                          else
                                            yyextra->current->name += "<";
                                          unput(*yytext);
                                          BEGIN(XtendMethod);
                                        }
<SharpPre>.                             {
                                          addType(yyscanner);
                                          yyextra->current->type += '<' ;
                                          yyextra->sharpCount++;
                                          unput(*yytext);
                                          BEGIN(Sharp);
                                        }
<Sharp>">"                              {
                                          yyextra->current->type += *yytext ;
                                          if (--yyextra->sharpCount<=0)
                                          {
                                            if (yyextra->insideXtendMethod)
                                              BEGIN(XtendMethod);
                                            else
                                              BEGIN(FindMembers) ;
                                          }
                                        }
<Sharp>"<"                              {
                                          yyextra->current->type += *yytext ;
                                          yyextra->sharpCount++;
                                        }
<Sharp>{BN}+                            {
                                          yyextra->current->type += ' ';
                                          lineCount(yyscanner);
                                        }
<Sharp>.                                {
                                          yyextra->current->type += *yytext ;
                                        }
<XtendRound>")"                         {
                                          yyextra->current->type += *yytext ;
                                          if (--yyextra->roundCount<=0)
                                          {
                                            BEGIN(FindMembers);
                                          }
                                        }
<XtendRound>"("                         {
                                          yyextra->current->type += *yytext ;
                                          yyextra->roundCount++;
                                        }
<XtendRound>{BN}+                       {
                                          yyextra->current->type += ' ';
                                          lineCount(yyscanner);
                                        }
<XtendRound>.                           {
                                          yyextra->current->type += *yytext;
                                        }
<FindFields>{ID}                        {
                                          yyextra->current->bodyLine = yyextra->yyLineNr;
                                          yyextra->current->bodyColumn = yyextra->yyColNr;
                                          yyextra->current->name = yytext;
                                        }
<FindFields>"("                         {
                                          // Java enum initializer
                                          unput('(');
                                          yyextra->lastInitializerContext = YY_START;
                                          yyextra->initBracketCount=0;
                                          yyextra->current->initializer = "=";
                                          BEGIN(ReadInitializer);
                                        }
<FindFields>"="                         {
                                          yyextra->lastInitializerContext = YY_START;
                                          yyextra->initBracketCount=0;
                                          yyextra->current->initializer = yytext;
                                          BEGIN(ReadInitializer);
                                        }
<FindFields>";"                         {
                                          //printf("DEBUG:<FindFields>%s\n", yytext);
                                          if (!yyextra->current->name.isEmpty())
                                          {
                                            yyextra->current->fileName   = yyextra->yyFileName;
                                            yyextra->current->startLine  = yyextra->yyLineNr;
                                            yyextra->current->startColumn = yyextra->yyColNr;
                                            if (!(yyextra->current_root->spec&Entry::Enum))
                                            {
                                              yyextra->current->type       = "@"; // enum marker
                                            }
                                            yyextra->current->args       = yyextra->current->args.simplifyWhiteSpace();
                                            yyextra->current->name       = yyextra->current->name.stripWhiteSpace();
                                            yyextra->current->section    = Entry::VARIABLE_SEC;
                                            yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
                                            initEntry(yyscanner);
                                          }

                                          BEGIN( FindMembers );
                                        }
<SkipRemainder>\n                       {
                                          lineCount(yyscanner);
                                        }
<SkipRemainder>[^\n]*
<FindFields>","                         {
                                          //printf("adding '%s' '%s' '%s' to enum '%s' (mGrpId=%d)\n",
                                          //     yyextra->current->type.data(), yyextra->current->name.data(),
                                          //     yyextra->current->args.data(), yyextra->current_root->name.data(),yyextra->current->mGrpId);
                                          if (!yyextra->current->name.isEmpty())
                                          {
                                            yyextra->current->fileName   = yyextra->yyFileName;
                                            yyextra->current->startLine  = yyextra->yyLineNr;
                                            yyextra->current->startColumn = yyextra->yyColNr;
                                            if (!(yyextra->current_root->spec&Entry::Enum))
                                            {
                                              yyextra->current->type       = "@"; // enum marker
                                            }
                                            yyextra->current->args       = yyextra->current->args.simplifyWhiteSpace();
                                            yyextra->current->name       = yyextra->current->name.stripWhiteSpace();
                                            yyextra->current->section    = Entry::VARIABLE_SEC;
                                            yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
                                            initEntry(yyscanner);
                                          }
                                          else // probably a redundant ,
                                          {
                                            yyextra->current->reset();
                                            initEntry(yyscanner);
                                          }
                                        }
<FindFields>"["                         { // attribute list in IDL
                                          yyextra->squareCount=1;
                                          yyextra->lastSquareContext = YY_START;
                                          BEGIN(SkipSquare);
                                        }
  /*
<FindFieldArg>","                       { unput(*yytext); BEGIN(FindFields); }
  */
<ReadBody,ReadNSBody,ReadBodyIntf>[^\r\n\#{}"@'/<]*     { yyextra->current->program += yytext ; }
<ReadBody,ReadNSBody,ReadBodyIntf>"//".*"\n"?           { lineCount(yyscanner); yyextra->current->program += yytext ; }
<ReadBody,ReadNSBody,ReadBodyIntf>@\"   { 
                                          yyextra->current->program += yytext ;
                                          yyextra->pSkipVerbString = &yyextra->current->program;
                                          yyextra->lastSkipVerbStringContext=YY_START;
                                          BEGIN( SkipVerbString );
                                        }
<ReadBody,ReadNSBody,ReadBodyIntf>\'\'\' {
                                          yyextra->current->program += yytext ;
                                          yyextra->pCopyQuotedGString = &yyextra->current->program;
                                          yyextra->lastStringContext=YY_START;
                                          BEGIN(CopyTemplate);
                                        }
<ReadBody,ReadNSBody,ReadBodyIntf>\'    |
<ReadBody,ReadNSBody,ReadBodyIntf>\"    {
                                          yyextra->current->program += yytext ;
                                          yyextra->pCopyQuotedGString = &yyextra->current->program;
                                          yyextra->lastStringContext=YY_START;
                                          yyextra->beginningChar = *yytext;
                                          BEGIN( CopyGString );
                                        }
<ReadBody,ReadNSBody,ReadBodyIntf>"/*"{B}* { 
                                          yyextra->current->program += yytext ;
                                          yyextra->lastContext = YY_START ;
                                          BEGIN( Comment ) ;
                                        }
<ReadBody,ReadNSBody,ReadBodyIntf>"/*"{BL} { 
                                          yyextra->current->program += yytext ;
                                          ++yyextra->yyLineNr ;
                                          yyextra->lastContext = YY_START ;
                                          BEGIN( Comment ) ;
                                        }
<ReadBody,ReadNSBody,ReadBodyIntf>"{"   { 
                                          yyextra->current->program += yytext ;
                                          ++yyextra->curlyCount ;
                                        }
<ReadBodyIntf>"}"                       {
                                          yyextra->current->program += yytext ;
                                          --yyextra->curlyCount ;
                                        }
<ReadBody,ReadNSBody>"}"                { //err("ReadBody count=%d\n",yyextra->curlyCount);
                                          //printf("DEBUG:<ReadBody,ReadNSBody>%s\n", yytext);
                                          //printf("DEBUG:CurlyCount:%d\n", yyextra->curlyCount);
                                          if ( yyextra->curlyCount>0)
                                          {
                                            yyextra->current->program += yytext ;
                                            --yyextra->curlyCount ;
                                          }
                                          else
                                          {
                                            yyextra->current->endBodyLine = yyextra->yyLineNr;
                                            std::shared_ptr<Entry> original_root = yyextra->current_root; // save root this namespace is in
                                            if (yyextra->current->section == Entry::NAMESPACE_SEC && yyextra->current->type == "namespace")
                                            {
                                              int split_point;
                                              // save documentation values
                                              QCString doc       = yyextra->current->doc;
                                              int      docLine   = yyextra->current->docLine;
                                              QCString docFile   = yyextra->current->docFile;
                                              QCString brief     = yyextra->current->brief;
                                              int      briefLine = yyextra->current->briefLine;
                                              QCString briefFile = yyextra->current->briefFile;
                                              // reset documentation values
                                              yyextra->current->doc       = "";
                                              yyextra->current->docLine   = 0;
                                              yyextra->current->docFile   = "";
                                              yyextra->current->brief     = "";
                                              yyextra->current->briefLine = 0;
                                              yyextra->current->briefFile = "";
                                              while ((split_point = yyextra->current->name.find("::")) != -1)
                                              {
                                                std::shared_ptr<Entry> new_current = std::make_shared<Entry>(*yyextra->current);
                                                yyextra->current->program = "";
                                                new_current->name  = yyextra->current->name.mid(split_point + 2);
                                                yyextra->current->name  = yyextra->current->name.left(split_point);
                                                if (!yyextra->current_root->name.isEmpty()) yyextra->current->name.prepend(yyextra->current_root->name+"::");

                                                yyextra->current_root->moveToSubEntryAndKeep(yyextra->current);
                                                yyextra->current_root = yyextra->current;
                                                yyextra->current = new_current;
                                              }
                                              // restore documentation values
                                              yyextra->current->doc       = doc;
                                              yyextra->current->docLine   = docLine;
                                              yyextra->current->docFile   = docFile;
                                              yyextra->current->brief     = brief;
                                              yyextra->current->briefLine = briefLine;
                                              yyextra->current->briefFile = briefFile;
                                            }
                                            QCString &cn = yyextra->current->name;
                                            QCString rn = yyextra->current_root->name.copy();
                                            //printf("cn='%s' rn='%s' yyextra->isTypedef=%d\n",cn.data(),rn.data(),yyextra->isTypedef);
                                            if (!cn.isEmpty() && !rn.isEmpty())
                                            {
                                              prependScope(yyscanner);
                                            }
                                            
                                            if ((yyextra->current->section == Entry::ENUM_SEC) || (yyextra->current->spec&Entry::Enum))
                                            {
                                              yyextra->current->program+=','; // add field terminator
                                            }
                                            // add compound definition to the tree
                                            yyextra->current->args=removeRedundantWhiteSpace(yyextra->current->args);
                                                      // was: yyextra->current->args.simplifyWhiteSpace();
                                            yyextra->current->type = yyextra->current->type.simplifyWhiteSpace();
                                            yyextra->current->name = yyextra->current->name.stripWhiteSpace();
                                            //printf("adding '%s' '%s' '%s' brief=%s %x\n",yyextra->current->type.data(),yyextra->current->name.data(),yyextra->current->args.data(),yyextra->current->brief.data(),yyextra->current->section);
                                            
                                            yyextra->memspecEntry = yyextra->current;
                                            yyextra->current_root->moveToSubEntryAndKeep( yyextra->current ) ;
                                            yyextra->current = std::make_shared<Entry>(*yyextra->current);
                                            // namespaces and interfaces and java classes ends with a closing bracket without semicolon
                                            yyextra->current->reset();
                                            yyextra->current_root = original_root; // restore scope from before namespace descent
                                            initEntry(yyscanner);
                                            yyextra->memspecEntry.reset();
                                            BEGIN( FindMembers ) ;
                                          }
                                        }
<ReadBody,ReadNSBody,ReadBodyIntf>{BN}{1,80} { yyextra->current->program += yytext ;
                                          lineCount(yyscanner) ;
                                        }
<ReadBody,ReadNSBody,ReadBodyIntf>.     { yyextra->current->program += yytext ; }

<FindMembers,XtendMethod,XtendFindFieldInitOrEnd>"("/{BN}*"::"*{BN}*({TSCOPE}{BN}*"::")*{TSCOPE}{BN}*")"{BN}*"(" | /* typedef void (A<int>::func_t)(args...) */
<FindMembers,XtendMethod,XtendFindFieldInitOrEnd>("("({BN}*"::"*{BN}*{TSCOPE}{BN}*"::")*({BN}*[*&\^]{BN}*)+)+ {   /* typedef void (A::*ptr_t)(args...) or int (*func(int))[], the ^ is for Obj-C blocks */
                                          yyextra->current->bodyLine = yyextra->yyLineNr;
                                          yyextra->current->bodyColumn = yyextra->yyColNr;
                                          lineCount(yyscanner);
                                          addType(yyscanner);
                                          yyextra->funcPtrType=yytext;
                                          yyextra->roundCount=0;
                                          //yyextra->current->type += yytext;
                                          BEGIN( FuncPtr );
                                        }
<FuncPtr>{SCOPENAME}                    {
                                          yyextra->current->name = yytext;
                                          if (yyextra->current->name=="const" || yyextra->current->name=="volatile")
                                          {
                                            yyextra->funcPtrType += yyextra->current->name;
                                          }
                                          else
                                          {
                                            BEGIN( EndFuncPtr );
                                          }
                                        }
<FuncPtr>.                              {
                                          //printf("error: FuncPtr '%c' unexpected at line %d of %s\n",*yytext,yyextra->yyLineNr,yyextra->yyFileName);
                                        }
<EndFuncPtr>")"{BN}*/";"                { // a variable with extra braces
                                          lineCount(yyscanner);
                                          yyextra->current->type+=yyextra->funcPtrType.data()+1;
                                          BEGIN(FindMembers);
                                        }
<EndFuncPtr>")"{BN}*/"("                { // a function pointer
                                          lineCount(yyscanner);
                                          yyextra->current->type+=yyextra->funcPtrType+")";
                                          BEGIN(FindMembers);
                                        }
<EndFuncPtr>")"{BN}*/"["                { // an array of variables
                                          lineCount(yyscanner);
                                          yyextra->current->type+=yyextra->funcPtrType.data();
                                          yyextra->current->args += ")";
                                          BEGIN(FindMembers);
                                        }
<EndFuncPtr>"("                         { // a function returning a function or
                                          // a function returning a pointer to an array
                                          yyextra->current->args += *yytext ;
                                          //yyextra->roundCount=0;
                                          //BEGIN( FuncFunc );
                                          yyextra->current->bodyLine = yyextra->yyLineNr;
                                          yyextra->current->bodyColumn = yyextra->yyColNr;
                                          yyextra->currentArgumentContext = FuncFuncEnd;
                                          yyextra->fullArgString=yyextra->current->args.copy();
                                          yyextra->copyArgString=&yyextra->current->args;
                                          BEGIN( ReadFuncArgType ) ;
                                        }
<EndFuncPtr>"["[^\n\]]*"]"              {
                                          yyextra->funcPtrType+=yytext;
                                        }
<EndFuncPtr>")"                         {
                                          BEGIN(FindMembers);
                                        }
<FuncFunc>"("                           {
                                          yyextra->current->args += *yytext ;
                                          ++yyextra->roundCount;
                                        }
<FuncFunc>")"                           {
                                          yyextra->current->args += *yytext ;
                                          if ( yyextra->roundCount )
                                            --yyextra->roundCount;
                                          else
                                          {
                                            BEGIN(FuncFuncEnd);
                                          }
                                        }
<FuncFuncEnd>")"{BN}*"("                {
                                          lineCount(yyscanner);
                                          yyextra->current->type+=yyextra->funcPtrType+")(";
                                          BEGIN(FuncFuncType);
                                        }
<FuncFuncEnd>")"{BN}*/[;{]              {
                                          lineCount(yyscanner);
                                          yyextra->current->type+=yyextra->funcPtrType.data()+1;
                                          BEGIN(Function);
                                        }
<FuncFuncEnd>")"{BN}*/"["               { // function returning a pointer to an array
                                          lineCount(yyscanner);
                                          yyextra->current->type+=yyextra->funcPtrType;
                                          yyextra->current->args+=")";
                                          BEGIN(FuncFuncArray);
                                        }
<FuncFuncEnd>.                          {
                                          yyextra->current->args += *yytext;
                                        }
<FuncFuncType>"("                       {
                                          yyextra->current->type += *yytext;
                                          yyextra->roundCount++;
                                        }
<FuncFuncType>")"                       {
                                          yyextra->current->type += *yytext;
                                          if (yyextra->roundCount)
                                            --yyextra->roundCount;
                                          else
                                            BEGIN(Function);
                                        }
<FuncFuncType>{BN}*","{BN}*             { lineCount(yyscanner) ; yyextra->current->type += ", " ; }
<FuncFuncType>{BN}+                     { lineCount(yyscanner) ; yyextra->current->type += ' ' ; }
<FuncFuncType>.                         {
                                          yyextra->current->type += *yytext;
                                        }
<FindMembers,XtendMethod>"("            {
                                          //printf("DEBUG:<FindMembers>%s\n", yytext);
                                          if (!yyextra->current->name.isEmpty())
                                          {
                                            yyextra->current->args = yytext;
                                            yyextra->current->bodyLine = yyextra->yyLineNr;
                                            yyextra->current->bodyColumn = yyextra->yyColNr;
                                            yyextra->currentArgumentContext = FuncQual;
                                            yyextra->fullArgString=yyextra->current->args.copy();
                                            yyextra->copyArgString=&yyextra->current->args;
                                            BEGIN( ReadFuncArgType ) ;
                                            //printf(">>> Read function arguments yyextra->current->argList.size()=%d\n",yyextra->current->argList.size());
                                          }
                                        }

  /*- Function argument reading rules ---------------------------------------*/

<ReadFuncArgType>[^ \/\r\t\n\)\(\"\'#]+ { *yyextra->copyArgString+=yytext;
                                          yyextra->fullArgString+=yytext;
                                        }
<CopyArgString>[^\n\\\"\']+             { *yyextra->copyArgString+=yytext;
                                          yyextra->fullArgString+=yytext;
                                        }
<CopyArgRound>[^\/\n\)\(\"\']+          {
                                          *yyextra->copyArgString+=yytext;
                                          yyextra->fullArgString+=yytext;
                                        }
<ReadFuncArgType,ReadTempArgs>{BN}*     {
                                          *yyextra->copyArgString+=" ";
                                          yyextra->fullArgString+=" ";
                                          lineCount(yyscanner);
                                        }
<ReadFuncArgType,CopyArgRound,CopyArgSharp,ReadTempArgs>{RAWBEGIN}      {
                                          yyextra->delimiter = yytext+2;
                                          yyextra->delimiter=yyextra->delimiter.left(yyextra->delimiter.length()-1);
                                          yyextra->lastRawStringContext = YY_START;
                                          yyextra->pCopyRawString = yyextra->copyArgString;
                                          *yyextra->pCopyRawString+=yytext;
                                          yyextra->fullArgString+=yytext;
                                          BEGIN(RawString);
                                        }
<ReadFuncArgType,CopyArgRound,CopyArgSharp,ReadTempArgs>\"      {
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                          yyextra->lastCopyArgStringContext = YY_START;
                                          BEGIN( CopyArgString );
                                        }
<ReadFuncArgType,ReadTempArgs>"("       {
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                          yyextra->argRoundCount=0;
                                          yyextra->lastCopyArgContext = YY_START;
                                          BEGIN( CopyArgRound );
                                        }
<ReadFuncArgType>")"                    {
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                          yyextra->current->argList = *stringToArgumentList(yyextra->language, yyextra->fullArgString);
                                          handleParametersCommentBlocks(yyscanner,yyextra->current->argList);

                                          /* remember the yyextra->current documentation block, since
                                             we could overwrite it with the documentation of
                                             a function argument, which we then have to correct later
                                             on
                                           */
                                          yyextra->docBackup = yyextra->current->doc;
                                          yyextra->briefBackup = yyextra->current->brief;

                                          BEGIN( yyextra->currentArgumentContext );
                                        }
        /* a special comment */
<ReadFuncArgType,ReadTempArgs>("/*"[*!]|"//"[/!])("<"?) {
                                          // for functions we interpret a comment
                                          // as documentation for the argument
                                          yyextra->fullArgString+=yytext;
                                          yyextra->lastCopyArgChar=0;
                                          yyextra->lastCommentInArgContext=YY_START;
                                          if (yytext[1]=='/')
                                            BEGIN( CopyArgCommentLine );
                                          else
                                            BEGIN( CopyArgComment );
                                        }
        /* a non-special comment */
<ReadFuncArgType,ReadTempArgs>"/**/"    { /* empty comment */ }
<ReadFuncArgType,ReadTempArgs>"/*"      {
                                          yyextra->lastCContext = YY_START;
                                          BEGIN( SkipComment );
                                        }
<ReadFuncArgType,ReadTempArgs>"//"      {
                                          yyextra->lastCContext = YY_START;
                                          BEGIN( SkipCxxComment );
                                        }
        /* ')' followed by a special comment */
<ReadFuncArgType>")"{BN}*("/*"[*!]|"//"[/!])"<" {
                                          lineCount(yyscanner);
                                          // for functions we interpret a comment
                                          // as documentation for the yyextra->last argument
                                          yyextra->lastCopyArgChar=*yytext;
                                          QCString text=&yytext[1];
                                          text=text.stripWhiteSpace();
                                          yyextra->lastCommentInArgContext=YY_START;
                                          yyextra->fullArgString+=text;
                                          if (text.find("//")!=-1)
                                            BEGIN( CopyArgCommentLine );
                                          else
                                            BEGIN( CopyArgComment );
                                        }
<CopyArgComment>^{B}*"*"+/{BN}+
<CopyArgComment>[^\n\\\@\*]+            { yyextra->fullArgString+=yytext; }
<CopyArgComment>"*/"                    { yyextra->fullArgString+=yytext;
                                          if (yyextra->lastCopyArgChar!=0)
                                            unput(yyextra->lastCopyArgChar);
                                          BEGIN( yyextra->lastCommentInArgContext );
                                        }
<CopyArgCommentLine>\n                  { yyextra->fullArgString+=yytext;
                                          lineCount(yyscanner);
                                          if (yyextra->lastCopyArgChar!=0)
                                            unput(yyextra->lastCopyArgChar);
                                          BEGIN( yyextra->lastCommentInArgContext );
                                        }
<CopyArgCommentLine>{CMD}("verbatim"|"latexonly"|"htmlonly"|"xmlonly"|"manonly"|"rtfonly"|"docbookonly"|"dot"|"code")/[^a-z_A-Z0-9\-]   { // verbatim command (which could contain nested comments!)
                                          yyextra->docBlockName=&yytext[1];
                                          yyextra->fullArgString+=yytext;
                                          BEGIN(CopyArgVerbatim);
                                        }
<CopyArgCommentLine>{CMD}("f$"|"f["|"f{")               {
                                          yyextra->docBlockName=&yytext[1];
                                          if (yyextra->docBlockName.at(1)=='[')
                                          {
                                            yyextra->docBlockName.at(1)='}';
                                          }
                                          if (yyextra->docBlockName.at(1)=='{')
                                          {
                                            yyextra->docBlockName.at(1)='}';
                                          }
                                          yyextra->fullArgString+=yytext;
                                          BEGIN(CopyArgVerbatim);
                                        }
<CopyArgVerbatim>[\\@]("endverbatim"|"endlatexonly"|"endhtmlonly"|"endxmlonly"|"enddocbookonly"|"endmanonly"|"endrtfonly"|"enddot"|"endcode"|"f$"|"f]"|"f}")/[^a-z_A-Z0-9\-] { // end of verbatim block
                                          yyextra->fullArgString+=yytext;
                                          if (yytext[1]=='f') // end of formula
                                          {
                                            BEGIN(CopyArgCommentLine);
                                          }
                                          if (&yytext[4]==yyextra->docBlockName)
                                          {
                                            BEGIN(CopyArgCommentLine);
                                          }
                                        }
<CopyArgCommentLine>[^\\\@\n]+          { yyextra->fullArgString+=yytext; }
<CopyArgCommentLine>.                   { yyextra->fullArgString+=*yytext; }
<CopyArgComment,CopyArgVerbatim>\n      { yyextra->fullArgString+=*yytext; lineCount(yyscanner); }
<CopyArgComment,CopyArgVerbatim>.       { yyextra->fullArgString+=*yytext; }
<CopyArgComment>{CMD}("brief"|"short"){B}+ {
                                          warn(yyextra->yyFileName,yyextra->yyLineNr,
                                              "Ignoring %cbrief command inside argument documentation",*yytext
                                             );
                                          yyextra->fullArgString+=' ';
                                        }
<ReadTempArgs>"<"                       {
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                          yyextra->argSharpCount=1;
                                          BEGIN( CopyArgSharp );
                                        }
<ReadTempArgs>">"                       {
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                          //printf("end template list '%s'\n",yyextra->copyArgString->data());
                                          *yyextra->currentArgumentList = *stringToArgumentList(yyextra->language, yyextra->fullArgString);
                                          BEGIN( yyextra->currentArgumentContext );
                                        }
<CopyArgRound>"("                       {
                                          yyextra->argRoundCount++;
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                        }
<CopyArgRound>")"                       {
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                          if (yyextra->argRoundCount>0)
                                            yyextra->argRoundCount--;
                                          else
                                            BEGIN( yyextra->lastCopyArgContext );
                                        }
<CopyArgSharp>"("                       {
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                          yyextra->argRoundCount=0;
                                          yyextra->lastCopyArgContext = YY_START;
                                          BEGIN( CopyArgRound );
                                        }
<CopyArgSharp>"<"                       {
                                          yyextra->argSharpCount++;
                                          //printf("yyextra->argSharpCount++=%d  copy\n",yyextra->argSharpCount);
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                        }
<CopyArgSharp>">"                       {
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                          yyextra->argSharpCount--;
                                          if (yyextra->argSharpCount>0)
                                          {
                                            //printf("yyextra->argSharpCount--=%d copy\n",yyextra->argSharpCount);
                                          }
                                          else
                                          {
                                            BEGIN( ReadTempArgs );
                                            //printf("end of yyextra->argSharpCount\n");
                                          }
                                        }
<CopyArgString>\\.                      {
                                          *yyextra->copyArgString+=yytext;
                                          yyextra->fullArgString+=yytext;
                                        }
<CopyArgString>\"                       {
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                          BEGIN( yyextra->lastCopyArgStringContext );
                                        }
<ReadFuncArgType,ReadTempArgs,CopyArgRound,CopyArgSharp>{CHARLIT}     {
                                          *yyextra->copyArgString+=yytext;
                                          yyextra->fullArgString+=yytext;
                                        }
<ReadFuncArgType,ReadTempArgs,CopyArgRound,CopyArgSharp>\'     {
                                          *yyextra->copyArgString+=yytext;
                                          yyextra->fullArgString+=yytext;
                                        }
<ReadFuncArgType,ReadTempArgs,CopyArgString,CopyArgRound,CopyArgSharp>\n {
                                          lineCount(yyscanner);
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                        }
<ReadFuncArgType,ReadTempArgs,CopyArgString,CopyArgRound,CopyArgSharp>. {
                                          *yyextra->copyArgString+=*yytext;
                                          yyextra->fullArgString+=*yytext;
                                        }



  /*------------------------------------------------------------------------*/


<FuncRound>"("                          { yyextra->current->args += *yytext ;
                                          ++yyextra->roundCount ;
                                        }
<FuncRound>")"                          { yyextra->current->args += *yytext ;
                                          if ( yyextra->roundCount )
                                            --yyextra->roundCount ;
                                          else
                                            BEGIN( FuncQual ) ;
                                        }
<FuncQual>"\'\'\'"                      { // Xtend: Template method
                                          unput(*yytext);
                                          unput(*yytext);
                                          unput(*yytext);
                                          BEGIN( Function );
                                        }
<FuncQual>("//"{B}*)?"/**"/[^/*]        |
<FuncQual>"///"/[^/]                    |
<FuncQual>"//!"                         {
                                          for(int i = 0; i < yyleng; i++)
                                            unput(yytext[yyleng - 1 -i]);
                                          BEGIN( Function );
                                        }
<FuncQual>[{:;,]                        {
                                          unput(*yytext); BEGIN( Function );
                                        }
<FuncQual>{BN}*"abstract"{BN}*          { // pure virtual member function
                                          lineCount(yyscanner) ;
                                          yyextra->current->virt = Pure;
                                          yyextra->current->args += " override ";
                                        }
<FuncQual,TrailingReturn>{BN}*"final"{BN}*             { // C++11 final method
                                          lineCount(yyscanner) ;
                                          yyextra->current->spec |= Entry::Final;
                                          yyextra->current->args += " final ";
                                          BEGIN(FuncQual);
                                        }
<FuncQual>{BN}*"new"{BN}*               { // new member function
                                          lineCount(yyscanner) ;
                                          yyextra->current->spec |= Entry::New;
                                          yyextra->current->args += " new ";
                                        }
<FuncQual>{BN}*"const"{BN}*             { // const member function
                                          lineCount(yyscanner) ;
                                          yyextra->current->args += " const ";
                                          yyextra->current->argList.setConstSpecifier(TRUE);
                                        }
<FuncQual>{BN}*"volatile"{BN}*          { // volatile member function
                                          lineCount(yyscanner) ;
                                          yyextra->current->args += " volatile ";
                                          yyextra->current->argList.setVolatileSpecifier(TRUE);
                                        }
<FuncQual>{BN}*"&"                      {
                                          yyextra->current->args += " &";
                                          yyextra->current->argList.setRefQualifier(RefQualifierLValue);
                                        }
<FuncQual>{BN}*"&&"                     {
                                          yyextra->current->args += " &&";
                                          yyextra->current->argList.setRefQualifier(RefQualifierRValue);
                                        }

<FuncQual,TrailingReturn>{BN}*"="{BN}*"0"{BN}*          { // pure virtual member function
                                          lineCount(yyscanner) ;
                                          yyextra->current->args += " = 0";
                                          yyextra->current->virt = Pure;
                                          yyextra->current->argList.setPureSpecifier(TRUE);
                                          BEGIN(FuncQual);
                                        }
<FuncQual>{BN}*"->"{BN}*                {
                                          lineCount(yyscanner);
                                          yyextra->current->argList.setTrailingReturnType(" -> ");
                                          yyextra->current->args += " -> ";
                                          yyextra->roundCount=0;
                                          BEGIN(TrailingReturn);
                                        }
<FuncQual>{BN}                          {

                                        }
<FuncQual><<EOF>>                       { // Xtend: in case method declaration is last member
                                          unput(*yytext); BEGIN( Function );
                                        }
<FuncQual>.                             { // Xtend: something unexpected, probably an Xtend method declaration without ';'
                                          unput(*yytext); BEGIN( Function );
                                        }
<TrailingReturn>[{;]                    {
                                          if (yyextra->roundCount>0) REJECT;
                                          unput(*yytext);
                                          BEGIN(FuncQual);
                                        }
<TrailingReturn>"("                     {
                                          yyextra->roundCount++;
                                          yyextra->current->argList.setTrailingReturnType(yyextra->current->argList.trailingReturnType()+yytext);
                                          yyextra->current->args+=yytext;
                                        }
<TrailingReturn>")"                     {
                                          if (yyextra->roundCount>0)
                                          {
                                            yyextra->roundCount--;
                                          }
                                          else
                                          {
                                            warn(yyextra->yyFileName,yyextra->yyLineNr,
                                                 "Found ')' without opening '(' for trailing return type '%s)...'",
                                                 yyextra->current->argList.trailingReturnType().data());
                                          }
                                          yyextra->current->argList.setTrailingReturnType(yyextra->current->argList.trailingReturnType()+yytext);
                                          yyextra->current->args+=yytext;
                                        }
<TrailingReturn>.                       {
                                          yyextra->current->argList.setTrailingReturnType(yyextra->current->argList.trailingReturnType()+yytext);
                                          yyextra->current->args+=yytext;
                                        }
<TrailingReturn>\n                      {
                                          lineCount(yyscanner);
                                          yyextra->current->argList.setTrailingReturnType(yyextra->current->argList.trailingReturnType()+yytext);
                                          yyextra->current->args+=' ';
                                        }
<FuncRound,FuncFunc>{BN}*","{BN}*       {
                                          lineCount(yyscanner) ;
                                          yyextra->current->args += ", " ;
                                        }
<FuncQual,FuncRound,FuncFunc>{BN}+      {
                                          lineCount(yyscanner) ;
                                          yyextra->current->args += ' ' ;
                                        }
<FuncQual>"="                           {
                                          // typically an initialized function pointer
                                          yyextra->lastInitializerContext=YY_START;
                                          yyextra->initBracketCount=0;
                                          yyextra->current->initializer = yytext;
                                          BEGIN(ReadInitializer);
                                        }
<FuncPtrInit>[{;]                       {
                                          unput(*yytext);
                                          BEGIN(FuncQual);
                                        }
<FuncPtrInit>\"                         {
                                          yyextra->current->args += *yytext;
                                          yyextra->pCopyQuotedString=&yyextra->current->args;
                                          yyextra->lastStringContext=FuncPtrInit;
                                          yyextra->beginningChar = *yytext;
                                          BEGIN(CopyString);
                                        }
<FuncPtrInit>\'                         {
                                          yyextra->current->args += *yytext;
                                        }
<FuncPtrInit>{CHARLIT}                  {
                                          yyextra->current->args += yytext;
                                        }
<FuncPtrInit>{ID}                       {
                                          yyextra->current->args += yytext;
                                        }
<FuncPtrInit>.                          {
                                          yyextra->current->args += *yytext;
                                        }
<FuncPtrInit>\n                         {
                                          yyextra->current->args += *yytext;
                                          lineCount(yyscanner);
                                        }
<FuncQual,FuncRound,FuncFunc>.          { yyextra->current->args += *yytext; }
<FuncQual>{BN}*"try:"                   |
<FuncQual>{BN}*"try"{BN}+               { /* try-function-block */
                                          yyextra->insideTryBlock=TRUE;
                                          lineCount(yyscanner);
                                          if (yytext[yyleng-1]==':')
                                          {
                                            unput(':');
                                            BEGIN( Function );
                                          }
                                        }
<FuncQual>{BN}*"throw"{BN}*"("          { // C++ style throw clause
                                          yyextra->current->exception = " throw (" ;
                                          yyextra->roundCount=0;
                                          lineCount(yyscanner) ;
                                          BEGIN( ExcpRound ) ;
                                        }
<FuncQual>{BN}*"throws"{BN}+            { // Java style throw clause
                                          yyextra->current->exception = " throws " ;
                                          lineCount(yyscanner) ;
                                          BEGIN( ExcpList );
                                        }
<ExcpRound>"("                          { yyextra->current->exception += *yytext ;
                                          ++yyextra->roundCount ;
                                        }
<ExcpRound>")"                          { yyextra->current->exception += *yytext ;
                                          if ( yyextra->roundCount )
                                            --yyextra->roundCount ;
                                          else
                                            BEGIN( FuncQual ) ;
                                        }
<ExcpRound>.                            {
                                          yyextra->current->exception += *yytext;
                                        }
<ExcpList>"{"                           {
                                          unput('{'); BEGIN( FuncQual );
                                        }
<ExcpList>";"                           {
                                          unput(';'); BEGIN( FuncQual );
                                        }
<ExcpList>"\n"                          {
                                          yyextra->current->exception += ' ';
                                          lineCount(yyscanner);
                                        }
<ExcpList>.                             {
                                          yyextra->current->exception += *yytext;
                                        }
<Function>"("                           { yyextra->current->type += yyextra->current->name ;
                                          yyextra->current->name  = yyextra->current->args ;
                                          yyextra->current->args  = yytext ;
                                          yyextra->roundCount=0;
                                          BEGIN( FuncRound ) ;
                                        }
<Function>":"                           {
                                          BEGIN(SkipInits);
                                        }
<Function>{BN}                          {

                                        }
<Function>("//"{B}*)?"/**"/[^/*]        |
<Function>"///"/[^/]                    |
<Function>"//!"                         |
<Function>"\'\'\'"                      |
<Function>[;{,]                         |
<Function>.                             { // Xtend: optional ';
                                          yyextra->XtendFieldNames = 0; // Xtend: reset variable
                                          yyextra->insideXtendMethod = false; // Xtend:reset variable

                                          yyextra->current->name=yyextra->current->name.simplifyWhiteSpace();
                                          yyextra->current->type=yyextra->current->type.simplifyWhiteSpace();
                                          yyextra->current->args=removeRedundantWhiteSpace(yyextra->current->args);
                                                        // was: yyextra->current->args.simplifyWhiteSpace();
                                          yyextra->current->fileName = yyextra->yyFileName;
                                          yyextra->current->startLine = yyextra->yyBegLineNr;
                                          yyextra->current->startColumn = yyextra->yyBegColNr;
                                          static QRegExp re("([^)]*[*&][^)]*)"); // (...*...)
                                          int ts=yyextra->current->type.find('<');
                                          int te=yyextra->current->type.findRev('>');
                                          int ti=yyextra->current->type.find(re,0);

                                          // bug677315: A<int(void *, char *)> get(); is not a function pointer
                                          bool isFunction = ti==-1 || // not a (...*...) pattern
                                                            (ts!=-1 && ts<te && ts<ti && ti<te); // (...*...) is part of a template argument list
                                          bool isVariable = (!yyextra->current->type.isEmpty() &&
                                                (!isFunction || yyextra->current->type.left(8)=="typedef "));

                                          //printf("type=%s ts=%d te=%d ti=%d isFunction=%d\n",
                                          //    yyextra->current->type.data(),ts,te,ti,isFunction);

                                          if (*yytext!=';' || (yyextra->current_root->section&Entry::COMPOUND_MASK) ) // Xtend: optional ';'
                                          {
                                            int tempArg=yyextra->current->name.find('<');
                                            QCString tempName;
                                            if (tempArg==-1) tempName=yyextra->current->name; else tempName=yyextra->current->name.left(tempArg);
                                            if (isVariable)
                                            {
                                              yyextra->current->section = Entry::VARIABLE_SEC ;
                                            }
                                            else
                                            {
                                              //printf("Scanner.l: found in class function: '%s' '%s' '%s'\n", yyextra->current->type.data(),yyextra->current->name.data(),yyextra->current->args.data());
                                              yyextra->current->section = Entry::FUNCTION_SEC ;
                                              yyextra->current->proto = (*yytext==';');
                                            }
                                          }
                                          else // a global function prototype or function variable
                                          {
                                            //printf("Scanner.l: prototype? type='%s' name='%s' args='%s'\n",yyextra->current->type.data(),yyextra->current->name.data(),yyextra->current->args.data());
                                            if (isVariable)
                                            {
                                              //printf("Scanner.l: found function variable!\n");
                                              yyextra->current->section = Entry::VARIABLE_SEC;
                                            }
                                            else
                                            {
                                              //printf("Scanner.l: found prototype\n");
                                              yyextra->current->section = Entry::FUNCTION_SEC;
                                              yyextra->current->proto = TRUE;
                                            }
                                          }
                                          yyextra->previous = yyextra->current;
                                          yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
                                          initEntry(yyscanner);
                                          yyextra->lastCurlyContext = FindMembers;
                                          if ( *yytext == ',' )
                                          {
                                            yyextra->current->type = yyextra->previous->type;
                                            // we need to strip any trailing * and & (see bugs 623023 and 649103 for test cases)
                                            int i=yyextra->current->type.length();
                                            while (i>0 && (yyextra->current->type[i-1]=='*' || yyextra->current->type[i-1]=='&' || yyextra->current->type[i-1]==' ')) i--;
                                            yyextra->current->type = yyextra->current->type.left(i);
                                          }
                                          if ( *yytext == '{' )
                                          {
                                            if (yyextra->current_root->section & Entry::COMPOUND_MASK)
                                            {
                                              yyextra->previous->spec |= Entry::Inline;
                                            }
                                            //addToBody(yytext);
                                            yyextra->curlyCount=0;
                                            yyextra->isTemplateMethod=FALSE;
                                            BEGIN( SkipCurly ) ;
                                          }
                                          else if (qstrcmp(yytext,"\'\'\'")==0)
                                          {
                                            if (yyextra->current_root->section & Entry::COMPOUND_MASK)
                                            {
                                              yyextra->previous->spec |= Entry::Inline;
                                            }
                                            //addToBody(yytext);
                                            yyextra->curlyCount=0;
                                            yyextra->isTemplateMethod=TRUE;
                                            BEGIN( SkipCurly ) ;
                                          }
                                          else if (*yytext == ';')
                                          {
                                            if (yyextra->previous->section!=Entry::VARIABLE_SEC)
                                              yyextra->previous->bodyLine=-1; // a function/member declaration
                                            BEGIN( FindMembers ) ;
                                          }
                                          else // Xtend: start of another member -> no ';' was used at the end of declaration
                                          {
                                            if (yytext[0] == '/')
                                            {
                                              for (int i = 0; i < yyleng; i++)
                                                unput(yytext[yyleng - 1 - i]);
                                            }
                                            else
                                              unput(*yytext);
                                            if (yyextra->previous->section!=Entry::VARIABLE_SEC)
                                              yyextra->previous->bodyLine=-1; // a function/member declaration
                                            BEGIN( FindMembers ) ;
                                          }
                                        }
<SkipInits>">"{BN}*"{"                  { // C++11 style initializer (see bug 790788)
                                          lineCount(yyscanner);
                                          yyextra->curlyCount=1;
                                          BEGIN(SkipC11Inits);
                                        }
<SkipInits>{ID}{BN}*"{"                 { // C++11 style initializer (see bug 688647)
                                          lineCount(yyscanner);
                                          yyextra->curlyCount=1;
                                          BEGIN(SkipC11Inits);
                                        }
<SkipC11Inits>"{"                       {
                                          ++yyextra->curlyCount;
                                        }
<SkipC11Inits>"}"                       {
                                          if ( --yyextra->curlyCount<=0 )
                                          {
                                            BEGIN(SkipInits);
                                          }
                                        }
<SkipInits>"{"                          { // C++11 style initializer
                                          unput('{');
                                          BEGIN( Function );
                                        }
<SkipCurly>"{"                          {
                                          //addToBody(yytext);
                                          ++yyextra->curlyCount ;
                                        }
<SkipCurly>"}"/{BN}*("/*!"|"/**"|"//!"|"///")"<!--" | /* see bug710917 */
<SkipCurly>"}"                          {
                                          //addToBody(yytext);
                                          if (!yyextra->isTemplateMethod)
                                          {
                                            if(yyextra->curlyCount)
                                            {
                                              --yyextra->curlyCount ;
                                            }
                                            else
                                            {
                                              if (!yyextra->current->sli.empty() && yyextra->previous) // copy special list items
                                              {
                                                yyextra->previous->sli = yyextra->current->sli;
                                                yyextra->current->sli.clear();
                                              }
                                              if (yyextra->previous) yyextra->previous->endBodyLine=yyextra->yyLineNr;
                                              BEGIN( yyextra->lastCurlyContext ) ;
                                            }
                                          }
                                        }
<SkipCurly>"\'\'\'"/{BN}*("/*!"|"/**"|"//!"|"///")"<!--" |
<SkipCurly>"\'\'\'"                     { /* Xtend: Template method end */
                                          //addToBody(yytext);
                                          if (yyextra->isTemplateMethod)
                                          {
                                            if (!yyextra->current->sli.empty() && yyextra->previous) // copy special list items
                                            {
                                              yyextra->previous->sli = yyextra->current->sli;
                                              yyextra->current->sli.clear();
                                            }
                                            if (yyextra->previous) yyextra->previous->endBodyLine=yyextra->yyLineNr;
                                            yyextra->isTemplateMethod = FALSE;
                                            BEGIN( yyextra->lastCurlyContext ) ;
                                          }
                                          else
                                          {
                                            yyextra->lastStringContext = YY_START;
                                            BEGIN(SkipTemplate);
                                          }
                                        }
<SkipCurly>"\'\'\'"{BN}*("/*!"|"/**"|"//!"|"///")"<" |
<SkipCurly>"}"{BN}*("/*!"|"/**"|"//!"|"///")"<" {
                                          lineCount(yyscanner);
                                          if ( yyextra->curlyCount && !yyextra->isTemplateMethod && yytext[0]=='}')
                                          {
                                            //addToBody(yytext);
                                            --yyextra->curlyCount ;
                                          }
                                          else
                                          {
                                            yyextra->current->endBodyLine=yyextra->yyLineNr;
                                            yyextra->tempEntry = yyextra->current; // temporarily switch to the previous entry
                                            yyextra->current   = yyextra->previous;

                                            yyextra->docBlockContext   = SkipCurlyEndDoc;
                                            yyextra->docBlockInBody    = FALSE;
                                            yyextra->docBlockAutoBrief = ( yytext[yyleng-2]=='*' && Config_getBool(JAVADOC_AUTOBRIEF) ) ||
                                                                ( yytext[yyleng-2]=='!' && Config_getBool(QT_AUTOBRIEF) );
                                            yyextra->docBlock.resize(0);
                                            yyextra->docBlockTerm = '}';
                                            if (yytext[yyleng-3]=='/')
                                            {
                                              startCommentBlock(yyscanner,TRUE);
                                              BEGIN( DocLine );
                                            }
                                            else
                                            {
                                              startCommentBlock(yyscanner,FALSE);
                                              BEGIN( DocBlock );
                                            }
                                          }
                                        }
<SkipCurlyEndDoc>"\'\'\'"{BN}*("/*!"|"/**"|"//!"|"///")"<" |
<SkipCurlyEndDoc>"}"{BN}*("/*!"|"/**"|"//!"|"///")"<" { // desc is followed by another one
                                          yyextra->docBlockContext   = SkipCurlyEndDoc;
                                          yyextra->docBlockInBody    = FALSE;
                                          yyextra->docBlockAutoBrief = ( yytext[yyleng-2]=='*' && Config_getBool(JAVADOC_AUTOBRIEF) ) ||
                                                              ( yytext[yyleng-2]=='!' && Config_getBool(QT_AUTOBRIEF) );
                                          yyextra->docBlock.resize(0);
                                          yyextra->docBlockTerm = '}';
                                          if (yytext[yyleng-3]=='/')
                                          {
                                            startCommentBlock(yyscanner,TRUE);
                                            BEGIN( DocLine );
                                          }
                                          else
                                          {
                                            startCommentBlock(yyscanner,FALSE);
                                            BEGIN( DocBlock );
                                          }
                                        }
<SkipCurlyEndDoc>"}"                    {
                                          //addToBody("}");
                                          if (yyextra->tempEntry) // we can only switch back to yyextra->current if no new item was created
                                          {
                                            yyextra->current = yyextra->tempEntry;
                                            yyextra->tempEntry.reset();
                                          }
                                          BEGIN( yyextra->lastCurlyContext );
                                        }
<SkipCurly>\'                           |
<SkipCurly>\"                           {
                                          //addToBody(yytext);
                                          if (!yyextra->isTemplateMethod)
                                          {
                                            yyextra->beginningChar = *yytext;
                                            yyextra->lastStringContext=SkipCurly;
                                            BEGIN( SkipString );
                                          }
                                        }
<SkipCurly>«                            {
                                          //addToBody(yytext);
                                          if (yyextra->isTemplateMethod)
                                          {
                                            yyextra->lastInnerTemplateContext=SkipCurly;
                                            BEGIN( SkipInnerTemplate );
                                          }
                                        }
<SkipCurly>««                           {
                                          //addToBody(yytext);
                                          if (yyextra->isTemplateMethod)
                                          {
                                            yyextra->lastTemplateContext=SkipCurly;
                                            BEGIN( SkipTemplateComment );
                                          }
                                        }
<SkipCurly,SkipC11Inits,SkipInits>\n    {
                                          lineCount(yyscanner);
                                          //addToBody(yytext);
                                        }
<SkipCurly>{B}*{RAWBEGIN}               {
                                          if (!yyextra->isTemplateMethod)
                                          {
                                            QCString raw=QCString(yytext).stripWhiteSpace();
                                            yyextra->delimiter = raw.data()+2;
                                            yyextra->delimiter=yyextra->delimiter.left(yyextra->delimiter.length()-1);
                                            yyextra->lastRawStringContext = YY_START;
                                            yyextra->dummyRawString.resize(0);
                                            yyextra->pCopyRawString = &yyextra->dummyRawString;
                                            *yyextra->pCopyRawString+=yytext;
                                            BEGIN(RawString);
                                          }
                                        }
<SkipCurly>[^\n#"'@\\/{}<«]+            {
                                          lineCount(yyscanner);  // for yyextra->column updates
                                          //addToBody(yytext);
                                        }
<SkipInits,SkipC11Inits,SkipCurly>"/*"  {
                                          if (!yyextra->isTemplateMethod)
                                          {
                                            //addToBody(yytext);
                                            yyextra->lastCContext = YY_START;
                                            BEGIN(SkipComment);
                                          }
                                        }
<SkipInits,SkipC11Inits,SkipCurly>"//"  {
                                          if (!yyextra->isTemplateMethod)
                                          {
                                            //addToBody(yytext);
                                            yyextra->lastCContext = YY_START;
                                            BEGIN(SkipCxxComment);
                                          }
                                        }
<SkipInits,SkipC11Inits>"("             {
                                          if (!yyextra->isTemplateMethod)
                                          {
                                            yyextra->roundCount=0;
                                            yyextra->lastSkipRoundContext=YY_START;
                                            BEGIN(SkipRound);
                                          }
                                        }
<SkipInits,SkipC11Inits>\"              {
                                          if (!yyextra->isTemplateMethod)
                                          {
                                            yyextra->lastStringContext=YY_START;
                                            yyextra->beginningChar = *yytext;
                                            BEGIN( SkipString );
                                          }
                                        }
<SkipInits>;                            {
                                          warn(yyextra->yyFileName,yyextra->yyLineNr,
                                              "Found ';' while parsing initializer list! "
                                              "(doxygen could be confused by a macro call without semicolon)"
                                             );
                                          BEGIN( FindMembers );
                                        }
<SkipInits,SkipC11Inits,SkipCurly>.     { }
<SkipString,SkipPHPString>\\.           { }
<SkipString>\'                          |
<SkipString>\"                          {
                                          if (yyextra->beginningChar == yytext[0])
                                            BEGIN( yyextra->lastStringContext );
                                        }
<SkipPHPString>\'                       {
                                          BEGIN( yyextra->lastStringContext );
                                        }
<SkipString,SkipPHPString>"/*"|"*/"|"//" { }
<SkipString,SkipPHPString>\n            {
                                          lineCount(yyscanner);
                                        }
<SkipString,SkipPHPString>[«»]          |
<SkipString,SkipPHPString>.             { }
<SkipTemplate>\'\'\'                    {
                                          BEGIN(yyextra->lastStringContext);
                                        }
<SkipTemplate>\n                        {
                                          lineCount(yyscanner);
                                        }
<SkipTemplate>««                        {
                                          yyextra->lastTemplateContext = YY_START;
                                          BEGIN( SkipTemplateComment );
                                        }
<SkipTemplate>«                         {
                                          yyextra->lastInnerTemplateContext = YY_START;
                                          BEGIN( SkipInnerTemplate );
                                        }
<SkipTemplate>"//"|"/*"|"*/"            |
<SkipTemplate>\\.                       |
<SkipTemplate>[^«]                      { }
<SkipTemplateComment>\n                 {
                                          lineCount(yyscanner);
                                          BEGIN(yyextra->lastTemplateContext);
                                        }
<SkipTemplateComment>"//"|"/*"|"*/"     |
<SkipTemplateComment>\\.                |
<SkipTemplateComment>[«»]               |
<SkipTemplateComment>.                  { }

  /* inner template copy rules */
<SkipInnerTemplate>\"|\'                {
                                          if (yyextra->InnerTemplateString)
                                          {
                                            if(yyextra->beginningChar == *yytext)
                                            {
                                              yyextra->InnerTemplateString = FALSE;
                                            }
                                          }
                                          else
                                          {
                                            yyextra->InnerTemplateString = TRUE;
                                            yyextra->beginningChar = *yytext;
                                          }
                                        }
<SkipInnerTemplate>\n                   {
                                          lineCount(yyscanner);
                                        }
<SkipInnerTemplate>"/*"                 {
                                          if (!yyextra->InnerTemplateString)
                                          {
                                            yyextra->lastCContext = YY_START;
                                            BEGIN(SkipComment);
                                          }
                                        }
<SkipInnerTemplate>"//"                 {
                                          if (!yyextra->InnerTemplateString)
                                          {
                                            yyextra->lastCContext = YY_START;
                                            BEGIN(SkipCxxComment);
                                          }
                                        }
<SkipInnerTemplate>\'\'\'               {
                                          if (!yyextra->InnerTemplateString)
                                          {
                                            if (yyextra->templateCount == 0)
                                            {
                                              yyextra->templateCount = 1;
                                            }
                                            else
                                            {
                                              yyextra->templateCount = 0;
                                            }
                                          }
                                        }
<SkipInnerTemplate>««                   {
                                          if (!yyextra->InnerTemplateString)
                                          {
                                            yyextra->lastTemplateContext = YY_START;
                                            BEGIN( SkipTemplateComment );
                                          }
                                        }
<SkipInnerTemplate>»                    {
                                          if (!yyextra->InnerTemplateString)
                                          {
                                            if (yyextra->templateCount == 0)
                                            {
                                              BEGIN(yyextra->lastInnerTemplateContext);
                                            }
                                          }
                                        }
<SkipInnerTemplate>\\.                  |
<SkipInnerTemplate>[^»]                 { }
<CompoundName>":"                       { // for "class : public base {} var;" construct, see bug 608359
                                          //printf("DEBUG:<CompoundName>%s\n", yytext);
                                          unput(':');
                                          BEGIN(ClassVar);
                                        }
<CompoundName>";"                       {
                                          yyextra->current->section = Entry::EMPTY_SEC ;
                                          yyextra->current->type.resize(0) ;
                                          yyextra->current->name.resize(0) ;
                                          yyextra->current->args.resize(0) ;
                                          yyextra->current->argList.clear();
                                          BEGIN( FindMembers ) ;
                                        }
<Bases>";"                              {
                                          yyextra->current->section = Entry::EMPTY_SEC ;
                                          yyextra->current->type.resize(0) ;
                                          yyextra->current->name.resize(0) ;
                                          yyextra->current->args.resize(0) ;
                                          yyextra->current->argList.clear();
                                          BEGIN( FindMembers ) ;
                                        }
<CompoundName>{SCOPENAME}/{BN}*"<"      {
                                          yyextra->sharpCount = 0;
                                          yyextra->current->name = yytext ;
                                          lineCount(yyscanner);
                                          yyextra->lastClassTemplSpecContext = ClassVar;
                                          
                                          yyextra->roundCount=0;
                                          BEGIN( ClassTemplSpec );
                                        }
<CSGeneric>"<"                          {
                                          ArgumentList al;
                                          // check bug 612858 before enabling the next line
                                          //yyextra->current->spec |= Entry::Template;
                                          yyextra->current->tArgLists.push_back(al);
                                          yyextra->currentArgumentList = &yyextra->current->tArgLists.back();
                                          yyextra->templateStr="<";
                                          yyextra->current->name += "<";
                                          yyextra->fullArgString = yyextra->templateStr;
                                          yyextra->copyArgString = &yyextra->current->name;
                                          //yyextra->copyArgString = &yyextra->templateStr;
                                          yyextra->currentArgumentContext = ClassVar;
                                          BEGIN( ReadTempArgs );
                                        }
<ClassTemplSpec>">"({BN}*"::"{BN}*{SCOPENAME})? {
                                          yyextra->current->name += yytext;
                                          lineCount(yyscanner);
                                          if (yyextra->roundCount==0 && --yyextra->sharpCount<=0)
                                          {
                                            yyextra->current->name = removeRedundantWhiteSpace(yyextra->current->name);
                                            BEGIN( yyextra->lastClassTemplSpecContext );
                                          }
                                        }
<ClassTemplSpec>"<"                     {
                                          yyextra->current->name += yytext;
                                          if (yyextra->roundCount==0) yyextra->sharpCount++;
                                        }
<ClassTemplSpec>.                       {
                                          yyextra->current->name += yytext;
                                        }
<CompoundName>{SCOPENAME}{BN}*";"       { // forward declaration
                                          if (!yyextra->current->tArgLists.empty())
                                          {
                                            // found a forward template declaration, this has
                                            // a purpose of its own
                                            yyextra->current->name = yytext;
                                            yyextra->current->name=yyextra->current->name.left(yyextra->current->name.length()-1).stripWhiteSpace();
                                            //printf("template class declaration for %s!\n",yyextra->current->name.data());
                                            QCString rn = yyextra->current_root->name.copy();
                                            //printf("cn='%s' rn='%s' yyextra->isTypedef=%d\n",cn.data(),rn.data(),yyextra->isTypedef);
                                            if (!yyextra->current->name.isEmpty() && !rn.isEmpty())
                                            {
                                              prependScope(yyscanner);
                                            }
                                            yyextra->current->spec|=Entry::ForwardDecl;
                                            yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
                                          }

                                          unput(';');
                                          yyextra->current->reset();
                                          initEntry(yyscanner);
                                          BEGIN( FindMembers );
                                        }
<CompoundName>{SCOPENAME}/{BN}*"("      {
                                          yyextra->current->name = yytext ;
                                          lineCount(yyscanner);
                                          BEGIN( ClassVar );
                                        }
<CompoundName>{SCOPENAME}/{BN}*","      {  // multiple forward declarations on one line
                                           // e.g. @protocol A,B;
                                          yyextra->current->reset();
                                          initEntry(yyscanner);
                                        }
<CompoundName>{SCOPENAME}               {
                                          //printf("DEBUG:<CompoundName>%s\n", yytext);
                                          yyextra->current->name = yytext ;
                                          lineCount(yyscanner);
                                          BEGIN( ClassVar );
                                        }
<ClassVar>{SCOPENAME}{BN}*/"("          {
                                          addType(yyscanner);
                                          yyextra->current->name = yytext;
                                          yyextra->current->name = yyextra->current->name.stripWhiteSpace();
                                          lineCount(yyscanner);
                                          BEGIN( FindMembers );
                                        }
<ClassVar>","                           { }
<ClassVar>{ID}                          {
                                          //printf("DEBUG:<ClassVar>%s\n", yytext);
                                          if (qstrcmp(yytext,"implements")==0 || qstrcmp(yytext,"extends")==0)
                                          {
                                            yyextra->current->type.resize(0);
                                            yyextra->baseProt=Public;
                                            yyextra->baseVirt=Normal;
                                            yyextra->baseName.resize(0);
                                            BEGIN( BasesProt ) ;
                                          }
                                          else if (qstrcmp(yytext,"final")==0)
                                          {
                                            yyextra->current->spec|=Entry::Final;
                                          }
                                          else
                                          {
                                            if (yyextra->current->section == Entry::ENUM_SEC)
                                            { // found "enum a b" -> variable
                                              yyextra->current->section = Entry::VARIABLE_SEC ;
                                            }
                                            yyextra->current->type += ' ' ;
                                            yyextra->current->type += yyextra->current->name ;
                                            yyextra->current->name = yytext ;
                                          }
                                        }
<ClassVar>[(\[]                         {
                                          // probably a function anyway
                                          unput(*yytext);
                                          BEGIN( FindMembers );
                                        }
<ClassCategory>{ID}                     {
                                          yyextra->current->name+=yytext;
                                        }
<ClassCategory>")"/{BN}*"{"             {
                                          yyextra->current->name+=')';
                                          BEGIN( ClassVar );
                                        }
<ClassCategory>")"                      {
                                          yyextra->current->name+=')';
                                          unput('}');
                                          unput('{');
                                          BEGIN( ClassVar );
                                        }
<ClassVar>":"                           {
                                          //printf("DEBUG:<ClassVar>:%s\n", yytext);
                                          yyextra->current->type.resize(0);
                                          if ((yyextra->current->spec & Entry::Interface) ||
                                              (yyextra->current->spec & Entry::Struct)    ||
                                              (yyextra->current->spec & Entry::Ref)       ||
                                              (yyextra->current->spec & Entry::Value)
                                              )
                                            yyextra->baseProt=Public;
                                          else
                                            yyextra->baseProt=Private;
                                          yyextra->baseVirt=Normal;
                                          yyextra->baseName.resize(0);
                                          BEGIN( BasesProt ) ;
                                        }
<ClassVar>[;=*&]                        {
                                          unput(*yytext);
                                          if ((yytext[0]=='*' || yytext[0]=='&') &&
                                              yyextra->current->section == Entry::ENUM_SEC)
                                          { // found "enum a *b" -> variable
                                            yyextra->current->section = Entry::VARIABLE_SEC ;
                                          }
                                          BEGIN( FindMembers );
                                        }
<CompoundName,ClassVar>{B}*"{"{B}*      {
                                          //printf("DEBUG:<CompoundName,ClassVar>%s\n", yytext);
                                          yyextra->current->program.resize(0);
                                          yyextra->current->fileName = yyextra->yyFileName ;
                                          yyextra->current->bodyLine  = yyextra->yyLineNr;
                                          yyextra->current->bodyColumn = yyextra->yyColNr;
                                          yyextra->current->name = removeRedundantWhiteSpace(yyextra->current->name);
                                          if (yyextra->current->name.isEmpty() && !yyextra->isTypedef) // anonymous compound
                                          {
                                            if (yyextra->current->section==Entry::NAMESPACE_SEC) // allow reopening of anonymous namespaces
                                            {
                                              if (Config_getBool(EXTRACT_ANON_NSPACES)) // use visible name
                                              {
                                                yyextra->current->name="anonymous_namespace{"+stripPath(yyextra->current->fileName)+"}";
                                              }
                                              else // use invisible name
                                              {
                                                // yyextra->current->name.sprintf("@%d",anonNSCount.load()); TODO
                                              }
                                            }
                                            else
                                            {
                                              // yyextra->current->name.sprintf("@%d",anonCount++); TODO
                                            }
                                          }
                                          yyextra->curlyCount=0;
                                          if (yyextra->current->section==Entry::NAMESPACE_SEC)
                                          { // namespace body
                                            BEGIN( ReadNSBody );
                                          }
                                          else
                                          { // class body
                                            BEGIN( ReadBody ) ;
                                          }
                                        }
<CompoundName,ClassVar>{B}*"\'\'\'"{B}* {
                                          //printf("DEBUG:<CompoundName,ClassVar>%s\n", yytext);
                                          yyextra->current->program.resize(0);
                                          yyextra->current->fileName = yyextra->yyFileName ;
                                          yyextra->current->bodyLine  = yyextra->yyLineNr;
                                          yyextra->current->bodyColumn = yyextra->yyColNr;
                                          yyextra->current->name = removeRedundantWhiteSpace(yyextra->current->name);
                                          if (yyextra->current->name.isEmpty() && !yyextra->isTypedef) // anonymous compound
                                          {
                                            if (yyextra->current->section==Entry::NAMESPACE_SEC) // allow reopening of anonymous namespaces
                                            {
                                              if (Config_getBool(EXTRACT_ANON_NSPACES)) // use visible name
                                              {
                                                yyextra->current->name="anonymous_namespace{"+stripPath(yyextra->current->fileName)+"}";
                                              }
                                              else // use invisible name
                                              {
                                                // yyextra->current->name.sprintf("@%d",anonNSCount.load()); TODO
                                              }
                                            }
                                            else
                                            {
                                              // yyextra->current->name.sprintf("@%d",anonCount++); TODO
                                            }
                                          }
                                          yyextra->curlyCount=0;
                                          if (yyextra->current->section==Entry::NAMESPACE_SEC)
                                          { // namespace body
                                            BEGIN( ReadNSBody );
                                          }
                                          else
                                          { // class body
                                            BEGIN( ReadBody ) ;
                                          }
                                        }
<BasesProt>"public"{BN}+                { lineCount(yyscanner); yyextra->baseProt = Public; }
<BasesProt>"protected"{BN}+             { lineCount(yyscanner); yyextra->baseProt = Protected; }
<BasesProt>"private"{BN}+               { lineCount(yyscanner); yyextra->baseProt = Private; }
<BasesProt>{BN}                         { lineCount(yyscanner); }
<BasesProt>.                            { unput(*yytext); BEGIN(Bases); }
<Bases>("::")?{BN}*({ID}{BN}*"::"{BN}*)*{ID}    {
                                          lineCount(yyscanner);
                                          QCString baseScope = yytext;
                                          yyextra->baseName+=yytext;
                                          yyextra->current->args += ' ';
                                          yyextra->current->args += yytext;
                                        }
<Bases>{BN}*{ID}("."{ID})*              { // Java style class
                                          //printf("DEBUG:<Bases>:%s\n", yytext);
                                          QCString name = substitute(yytext,".","::");
                                          yyextra->baseName += name;
                                          yyextra->current->args += ' ';
                                          yyextra->current->args += name;
                                        }
<ClassVar>"<"                           { yyextra->current->name += *yytext;
                                          yyextra->sharpCount=1;
                                          yyextra->roundCount=0;
                                          yyextra->lastSkipSharpContext = YY_START;
                                          yyextra->specName = &yyextra->current->name;
                                          BEGIN ( Specialization );
                                        }
<Bases>{BN}*"<"                         {
                                          lineCount(yyscanner);
                                          yyextra->sharpCount=1;
                                          yyextra->roundCount=0;
                                          yyextra->lastSkipSharpContext = YY_START;
                                          
                                          yyextra->templateStr = yytext;
                                          yyextra->specName = &yyextra->templateStr;
                                          BEGIN ( Specialization );
                                        }
<Specialization>"<"                     { *yyextra->specName += *yytext;
                                          if (yyextra->roundCount==0) yyextra->sharpCount++;
                                        }
<Specialization>">"                     {
                                          *yyextra->specName += *yytext;
                                          if (yyextra->roundCount==0 && --yyextra->sharpCount<=0)
                                          {
                                            yyextra->baseName+=removeRedundantWhiteSpace(*yyextra->specName);
                                            BEGIN(yyextra->lastSkipSharpContext);
                                          }
                                        }
<Specialization>{BN}+                   { lineCount(yyscanner); *yyextra->specName +=' '; }
<Specialization>"<<"                    { *yyextra->specName += yytext; }
<Specialization>">>"/{B}*"::"           { // M$ C++ extension to allow >> to close a template...
                                          unput('>');
                                          unput(' ');
                                          unput('>');
                                        }
<Specialization>">>"                    {
                                          if (yyextra->roundCount>0)
                                          {
                                            *yyextra->specName += yytext;
                                          }
                                          else
                                          {
                                            unput('>');
                                            unput(' ');
                                            unput('>');
                                          }
                                        }
<Specialization>"("                     { *yyextra->specName += *yytext; yyextra->roundCount++; }
<Specialization>")"                     { *yyextra->specName += *yytext; yyextra->roundCount--; }

<Specialization>"\\\\"                  { *yyextra->specName += *yytext;}
<Specialization>"\\'"                   { *yyextra->specName += *yytext;}
<Specialization>"\\\""                  { *yyextra->specName += *yytext;}
<Specialization>"'"                     { *yyextra->specName += *yytext;BEGIN(SpecializationSingleQuote);}
<Specialization>"\""                    { *yyextra->specName += *yytext;BEGIN(SpecializationDoubleQuote);}
<SpecializationSingleQuote,SpecializationDoubleQuote>"\\\\"       { *yyextra->specName += *yytext;}
<SpecializationSingleQuote>"\\'"        { *yyextra->specName += *yytext;}
<SpecializationSingleQuote>"'"          { *yyextra->specName += *yytext; BEGIN(Specialization);}
<SpecializationDoubleQuote>"\\\""       { *yyextra->specName += *yytext;}
<SpecializationDoubleQuote>"\""         { *yyextra->specName += *yytext; BEGIN(Specialization);}
<SpecializationSingleQuote,SpecializationDoubleQuote>.          { *yyextra->specName += *yytext;}

<Specialization>.                       {
                                          *yyextra->specName += *yytext;
                                        }
<SkipRound>"("                          { ++yyextra->roundCount; }
<SkipRound>")"                          { if (--yyextra->roundCount<0)
                                            BEGIN ( yyextra->lastSkipRoundContext );
                                        }
<SkipRound>\'                           |
<SkipRound>\"                           {
                                          yyextra->lastStringContext=SkipRound;
                                          yyextra->beginningChar = yytext[0];
                                          BEGIN(SkipString);
                                        }
<SkipRound>[\r\n]+                      {
                                          lineCount(yyscanner);
                                        }
<SkipRound>.                            {} // probably an active annotation
<Bases>","|(">"({BN}*"{")?)|({BN}+"implements"{BN}*)    { lineCount(yyscanner);
                                          yyextra->current->args += ',' ;
                                          yyextra->current->name = removeRedundantWhiteSpace(yyextra->current->name);
                                          if (!yyextra->baseName.isEmpty())
                                          {
                                            yyextra->current->extends.push_back(
                                              BaseInfo(yyextra->baseName,yyextra->baseProt,yyextra->baseVirt)
                                            );
                                          }
                                          yyextra->baseProt=Public;
                                          yyextra->baseVirt=Normal;
                                          yyextra->baseName.resize(0);
                                          if (*yytext=='>')
                                          { // end of a ObjC protocol list
                                            yyextra->insideProtocolList=FALSE;
                                            if (yyleng==1)
                                            {
                                              unput('{'); // dummy start body
                                            }
                                            else
                                            {
                                              yyless(1);
                                            }
                                          }
                                          else
                                          {
                                            BEGIN(BasesProt);
                                          }
                                        }
<Bases>{B}*"{"{B}*                      {
                                          //printf("DEBUG:<Bases>%s\n", yytext);
                                          yyextra->current->program.resize(0);
                                          yyextra->current->fileName = yyextra->yyFileName ;
                                          yyextra->current->bodyLine  = yyextra->yyLineNr;
                                          yyextra->current->bodyColumn = yyextra->yyColNr;
                                          yyextra->current->name = removeRedundantWhiteSpace(yyextra->current->name);
                                          if (!yyextra->baseName.isEmpty())
                                            yyextra->current->extends.push_back(
                                               BaseInfo(yyextra->baseName,yyextra->baseProt,yyextra->baseVirt)
                                            );
                                          yyextra->curlyCount=0;
                                          BEGIN( ReadBody ) ;
                                        }
<Comment>{BN}+                          { yyextra->current->program += yytext ;
                                          lineCount(yyscanner) ;
                                        }
<Comment>"/*"                           { yyextra->current->program += yytext ; }
<Comment>"//"                           { yyextra->current->program += yytext ; }
<Comment>{CMD}("code"|"verbatim")       {
                                          yyextra->insideCode=TRUE;
                                          yyextra->current->program += yytext ;
                                        }
<Comment>{CMD}("endcode"|"endverbatim") {
                                          yyextra->insideCode=FALSE;
                                          yyextra->current->program += yytext ;
                                        }
<Comment>[^ \.\t\r\n\/\*]+              { yyextra->current->program += yytext ; }
<Comment>"*/"                           { yyextra->current->program += yytext ;
                                          if (!yyextra->insideCode) BEGIN( yyextra->lastContext ) ;
                                        }
<Comment>.                              { yyextra->current->program += *yytext ; }

<FindMembers,FindFields,SkipCurly,Operator,ClassVar,SkipInits,SkipC11Inits,Bases,XtendMethod>("//"{B}*)?"/*!" {
                                          //printf("Start doc block at %d\n",yyextra->yyLineNr);
                                          if (!yyextra->current->doc.isEmpty())
                                          {
                                            yyextra->current->doc+="\n\n";
                                          }
                                          else
                                          {
                                            yyextra->current->docLine = yyextra->yyLineNr;
                                            yyextra->current->docFile = yyextra->yyFileName;
                                          }

                                          yyextra->lastDocContext = YY_START;
                                          if (yyextra->current_root->section & Entry::SCOPE_MASK)
                                          {
                                            yyextra->current->inside = yyextra->current_root->name+"::";
                                          }
                                          yyextra->docBlockContext   = YY_START;
                                          yyextra->docBlockInBody    = YY_START==SkipCurly;
                                          yyextra->docBlockAutoBrief = Config_getBool(QT_AUTOBRIEF);

                                          QCString indent;
                                          indent.fill(' ',computeIndent(yytext,yyextra->column));
                                          yyextra->docBlock=indent;

                                          if (yyextra->docBlockAutoBrief)
                                          {
                                            yyextra->current->briefLine = yyextra->yyLineNr;
                                            yyextra->current->briefFile = yyextra->yyFileName;
                                          }
                                          startCommentBlock(yyscanner,FALSE);
                                          BEGIN( DocBlock );
                                        }
<FindMembers,FindFields,SkipCurly,Operator,ClassVar,SkipInits,Bases,XtendMethod>"/**"[*]+{BL} {
                                          bool javadocBanner = Config_getBool(JAVADOC_BANNER);
                                          lineCount(yyscanner);

                                          if( javadocBanner )
                                          {
                                            yyextra->lastDocContext = YY_START;

                                            //printf("Found comment banner at %s:%d\n",yyextra->yyFileName,yyextra->yyLineNr);
                                            if (yyextra->current_root->section & Entry::SCOPE_MASK)
                                            {
                                              yyextra->current->inside = yyextra->current_root->name+"::";
                                            }
                                            yyextra->current->docLine = yyextra->yyLineNr;
                                            yyextra->current->docFile = yyextra->yyFileName;
                                            yyextra->docBlockContext = YY_START;
                                            yyextra->docBlockInBody  = YY_START==SkipCurly;
                                            bool javadocAutoBrief = Config_getBool(JAVADOC_AUTOBRIEF);
                                            yyextra->docBlockAutoBrief = javadocAutoBrief;

                                            QCString indent;
                                            indent.fill(' ',computeIndent(yytext,yyextra->column));
                                            yyextra->docBlock=indent;

                                            if (yyextra->docBlockAutoBrief)
                                            {
                                              yyextra->current->briefLine = yyextra->yyLineNr;
                                              yyextra->current->briefFile = yyextra->yyFileName;
                                            }
                                            startCommentBlock(yyscanner,FALSE);
                                            BEGIN( DocBlock );
                                          }
                                          else
                                          {
                                            yyextra->current->program += yytext ;
                                            yyextra->lastContext = YY_START ;
                                            BEGIN( Comment ) ;
                                          }
                                        }
<FindMembers,FindFields,SkipCurly,Operator,ClassVar,SkipInits,Bases,XtendMethod>("//"{B}*)?"/**"/[^/*] {
                                          yyextra->lastDocContext = YY_START;

                                          //printf("Found comment block at %s:%d\n",yyextra->yyFileName,yyextra->yyLineNr);
                                          if (yyextra->current_root->section & Entry::SCOPE_MASK)
                                          {
                                            yyextra->current->inside = yyextra->current_root->name+"::";
                                          }
                                          yyextra->current->docLine = yyextra->yyLineNr;
                                          yyextra->current->docFile = yyextra->yyFileName;
                                          yyextra->docBlockContext = YY_START;
                                          yyextra->docBlockInBody  = YY_START==SkipCurly;
                                          bool javadocAutoBrief = Config_getBool(JAVADOC_AUTOBRIEF);
                                          yyextra->docBlockAutoBrief = javadocAutoBrief;

                                          QCString indent;
                                          indent.fill(' ',computeIndent(yytext,yyextra->column));
                                          yyextra->docBlock=indent;

                                          if (yyextra->docBlockAutoBrief)
                                          {
                                            yyextra->current->briefLine = yyextra->yyLineNr;
                                            yyextra->current->briefFile = yyextra->yyFileName;
                                          }
                                          startCommentBlock(yyscanner,FALSE);
                                          BEGIN( DocBlock );
                                        }
<FindMembers,FindFields,SkipCurly,Operator,ClassVar,Bases,XtendMethod>"//!" {
                                          yyextra->lastDocContext = YY_START;
                                          if (yyextra->current_root->section & Entry::SCOPE_MASK)
                                          {
                                            yyextra->current->inside = yyextra->current_root->name+"::";
                                          }
                                          yyextra->docBlockContext   = YY_START;
                                          yyextra->docBlockInBody    = YY_START==SkipCurly;
                                          yyextra->docBlockAutoBrief = FALSE;

                                          QCString indent;
                                          indent.fill(' ',computeIndent(yytext,yyextra->column));
                                          yyextra->docBlock=indent;

                                          startCommentBlock(yyscanner,yyextra->current->brief.isEmpty());
                                          BEGIN( DocLine );
                                        }
<FindMembers,FindFields,SkipCurly,Operator,ClassVar,Bases,XtendMethod>"///"/[^/] {
                                          yyextra->lastDocContext = YY_START;
                                          if (yyextra->current_root->section & Entry::SCOPE_MASK)
                                          {
                                            yyextra->current->inside = yyextra->current_root->name+"::";
                                          }
                                          yyextra->docBlockContext   = YY_START;
                                          yyextra->docBlockInBody    = YY_START==SkipCurly;
                                          yyextra->docBlockAutoBrief = FALSE;
                                          QCString indent;
                                          indent.fill(' ',computeIndent(yytext,yyextra->column));
                                          yyextra->docBlock=indent;
                                          startCommentBlock(yyscanner,yyextra->current->brief.isEmpty());
                                          BEGIN( DocLine );
                                        }
<FindMembers,XtendMethod>"{"            {
                                          if (yyextra->current->name.isEmpty())
                                          {
                                            // static Java initializer
                                            yyextra->needsSemi = FALSE;
                                            if (yyextra->current->stat)
                                            {
                                              yyextra->current->name="[static initializer]";
                                              yyextra->current->type.resize(0);
                                            }
                                            else
                                            {
                                              yyextra->current->name="[instance initializer]";
                                            }
                                            unput(*yytext);
                                            BEGIN( Function );
                                          }
                                          else
                                          {
                                            // pre C++11 code -> ignore the initializer
                                            //yyextra->needsSemi = TRUE;
                                            //yyextra->current->type.resize(0);
                                            //yyextra->current->name.resize(0);
                                            //yyextra->current->args.resize(0);
                                            //yyextra->current->argList.clear();
                                            //yyextra->curlyCount=0;
                                            //BEGIN( SkipCurlyBlock );

                                            // C++11 style initializer list
                                            yyextra->current->bodyLine = yyextra->yyLineNr;
                                            yyextra->current->bodyColumn = yyextra->yyColNr;
                                            yyextra->current->initializer = yytext;
                                            yyextra->lastInitializerContext = YY_START;
                                            yyextra->initBracketCount=1;
                                            BEGIN(ReadInitializer);
                                          }
                                        }


 /**********************************************************************************/
 /******************** Documentation block related rules ***************************/
 /**********************************************************************************/

 /* ---- Single line comments ------ */
<DocLine>[^\n]*"\n"[ \t]*"//"[/!][<]?   { // continuation of multiline C++-style comment
                                           yyextra->docBlock+=yytext;
                                           int markerLen = yytext[yyleng-1]=='<' ? 4 : 3;
                                           yyextra->docBlock.resize(yyextra->docBlock.length() - markerLen);
                                           lineCount(yyscanner);
                                        }
<DocLine>{B}*"///"[/]+{B}*/"\n"         { // ignore marker line (see bug700345)
                                          handleCommentBlock(yyscanner,yyextra->docBlock.data(),yyextra->current->brief.isEmpty());
                                          BEGIN( yyextra->docBlockContext );
                                        }
<DocLine>[^\n]*/"\n"{B}*"//"[!/]{B}*{CMD}"}"    { // next line is an end group marker, see bug 752712
                                          yyextra->docBlock+=yytext;
                                          handleCommentBlock(yyscanner,yyextra->docBlock.data(),yyextra->current->brief.isEmpty());
                                          BEGIN( yyextra->docBlockContext );
                                        }
<DocLine>[^\n]*/"\n"                    { // whole line
                                          yyextra->docBlock+=yytext;
                                          handleCommentBlock(yyscanner,yyextra->docBlock.data(),yyextra->current->brief.isEmpty());
                                          BEGIN( yyextra->docBlockContext );
                                        }

 /* ---- Comments blocks ------ */

<DocBlock>"*"*"*/"                      { // end of comment block
                                          handleCommentBlock(yyscanner,yyextra->docBlock.data(),FALSE);
                                          BEGIN(yyextra->docBlockContext);
                                        }
<DocBlock>^{B}*"*"+/[^/]                {

                                          QCString indent;
                                          indent.fill(' ',computeIndent(yytext,yyextra->column));
                                          yyextra->docBlock+=indent;
                                        }
<DocBlock>^{B}*("//")?{B}*"*"+/[^//a-z_A-Z0-9*] { // start of a comment line
                                          QCString indent;
                                          indent.fill(' ',computeIndent(yytext,yyextra->column));
                                          yyextra->docBlock+=indent;
                                        }
<DocBlock>^{B}*("//"){B}*               { // strip embedded C++ comments if at the start of a line
                                        }
<DocBlock>"//"                          { // slashes in the middle of a comment block
                                          yyextra->docBlock+=yytext;
                                        }
<DocBlock>"/*"                          { // start of a new comment in the
                                          // middle of a comment block
                                          yyextra->docBlock+=yytext;
                                        }
<DocBlock>({CMD}{CMD}){ID}/[^a-z_A-Z0-9] { // escaped command
                                          yyextra->docBlock+=yytext;
                                        }
<DocBlock>{CMD}("f$"|"f["|"f{")         {
                                          yyextra->docBlock+=yytext;
                                          yyextra->docBlockName=&yytext[1];
                                          if (yyextra->docBlockName.at(1)=='{')
                                          {
                                            yyextra->docBlockName.at(1)='}';
                                          }
                                          yyextra->fencedSize=0;
                                          yyextra->nestedComment=FALSE;
                                          BEGIN(DocCopyBlock);
                                        }
<DocBlock>{B}*"<"{PRE}">"               {
                                          yyextra->docBlock+=yytext;
                                          yyextra->docBlockName="<pre>";
                                          yyextra->fencedSize=0;
                                          yyextra->nestedComment=FALSE;
                                          BEGIN(DocCopyBlock);
                                        }
<DocBlock>{CMD}("verbatim"|"latexonly"|"htmlonly"|"xmlonly"|"manonly"|"rtfonly"|"docbookonly"|"dot"|"code")/[^a-z_A-Z0-9\-]     { // verbatim command (which could contain nested comments!)
                                          yyextra->docBlock+=yytext;
                                          yyextra->docBlockName=&yytext[1];
                                          yyextra->fencedSize=0;
                                          yyextra->nestedComment=FALSE;
                                          BEGIN(DocCopyBlock);
                                        }
<DocBlock>^({B}*"*"+)?{B}{0,3}"~~~"[~]* {

                                          QCString pat = substitute(yytext,"*"," ");
                                          yyextra->docBlock+=pat;
                                          yyextra->docBlockName="~~~";
                                          yyextra->fencedSize=pat.stripWhiteSpace().length();
                                          yyextra->nestedComment=FALSE;
                                          BEGIN(DocCopyBlock);
                                        }
<DocBlock>^({B}*"*"+)?{B}{0,3}"```"[`]* {
                                          QCString pat = substitute(yytext,"*"," ");
                                          yyextra->docBlock+=pat;
                                          yyextra->docBlockName="```";
                                          yyextra->fencedSize=pat.stripWhiteSpace().length();
                                          yyextra->nestedComment=FALSE;
                                          BEGIN(DocCopyBlock);
                                        }
<DocBlock>[^@*~\/\\\n]+                 { // any character that isn't special
                                          yyextra->docBlock+=yytext;
                                        }
<DocBlock>\n                            { // newline
                                          lineCount(yyscanner);
                                          yyextra->docBlock+=*yytext;
                                        }
<DocBlock>.                             { // command block
                                          yyextra->docBlock+=*yytext;
                                        }

 /* ---- Copy verbatim sections ------ */

<DocCopyBlock>"</"{PRE}">"              { // end of a <pre> block
                                          yyextra->docBlock+=yytext;
                                          if (yyextra->docBlockName=="<pre>")
                                          {
                                            BEGIN(DocBlock);
                                          }
                                        }
<DocCopyBlock>"</"{CODE}">"             { // end of a <code> block
                                          yyextra->docBlock+=yytext;
                                          if (yyextra->docBlockName=="<code>")
                                          {
                                            BEGIN(DocBlock);
                                          }
                                        }
<DocCopyBlock>[\\@]("f$"|"f]"|"f}")     {
                                          yyextra->docBlock+=yytext;
                                          BEGIN(DocBlock);
                                        }
<DocCopyBlock>[\\@]("endverbatim"|"endlatexonly"|"endhtmlonly"|"endxmlonly"|"enddocbookonly"|"endmanonly"|"endrtfonly"|"enddot"|"endcode")/[^a-z_A-Z0-9] { // end of verbatim block
                                          yyextra->docBlock+=yytext;
                                          if (&yytext[4]==yyextra->docBlockName)
                                          {
                                            BEGIN(DocBlock);
                                          }
                                        }
<DocCopyBlock>^{B}*"*"+/{BN}+           { // start of a comment line
                                          if (yyextra->docBlockName=="verbatim")
                                          {
                                            REJECT;
                                          }
                                          else if (yyextra->docBlockName=="code")
                                          {
                                            REJECT;
                                          }
                                          else
                                          {
                                            QCString indent;
                                            indent.fill(' ',computeIndent(yytext,0));
                                            yyextra->docBlock+=indent;
                                          }
                                        }
<DocCopyBlock>^{B}*"*"+/{B}+"*"{BN}*    { // start of a comment line with two *'s
                                          if (yyextra->docBlockName=="code")
                                          {
                                            QCString indent;
                                            indent.fill(' ',computeIndent(yytext,0));
                                            yyextra->docBlock+=indent;
                                          }
                                          else
                                          {
                                            REJECT;
                                          }
                                        }
<DocCopyBlock>^{B}*"*"+/({ID}|"(")      { // Assume *var or *(... is part of source code (see bug723516)
                                          if (yyextra->docBlockName=="code")
                                          {
                                            QCString indent;
                                            indent.fill(' ',computeIndent(yytext,-1));
                                            yyextra->docBlock+=indent+"*";
                                          }
                                          else
                                          {
                                            REJECT;
                                          }
                                        }
<DocCopyBlock>^{B}*"*"+/{BN}*           { // start of a comment line with one *
                                          if (yyextra->docBlockName=="code")
                                          {
                                            QCString indent;
                                            if (yyextra->nestedComment) // keep * it is part of the code
                                            {
                                              indent.fill(' ',computeIndent(yytext,-1));
                                              yyextra->docBlock+=indent+"*";
                                            }
                                            else // remove * it is part of the comment block
                                            {
                                              indent.fill(' ',computeIndent(yytext,0));
                                              yyextra->docBlock+=indent;
                                            }
                                          }
                                          else
                                          {
                                            REJECT;
                                          }
                                        }
<DocCopyBlock>^({B}*"*"+)?{B}{0,3}"~~~"[~]* {
                                          QCString pat = substitute(yytext,"*"," ");
                                          yyextra->docBlock+=pat;
                                          if (yyextra->fencedSize==pat.stripWhiteSpace().length())
                                          {
                                            BEGIN(DocBlock);
                                          }
                                        }
<DocCopyBlock>^({B}*"*"+)?{B}{0,3}"```"[`]*                 {
                                          QCString pat = substitute(yytext,"*"," ");
                                          yyextra->docBlock+=pat;
                                          if (yyextra->fencedSize==pat.stripWhiteSpace().length())
                                          {
                                            BEGIN(DocBlock);
                                          }
                                        }
<DocCopyBlock>[^\<@/*\]~\$\\\n]+        { // any character that is not special
                                          yyextra->docBlock+=yytext;
                                        }
<DocCopyBlock>"/*"|"*/"|"//"            {
                                          if (yytext[1]=='*')
                                          {
                                            yyextra->nestedComment=TRUE;
                                          }
                                          else if (yytext[0]=='*')
                                          {
                                            yyextra->nestedComment=FALSE;
                                          }
                                          yyextra->docBlock+=yytext;
                                        }
<DocCopyBlock>\n                        { // newline
                                          yyextra->docBlock+=*yytext;
                                          lineCount(yyscanner);
                                        }
<DocCopyBlock>.                         { // any other character
                                          yyextra->docBlock+=*yytext;
                                        }
<DocCopyBlock><<EOF>>                   {
                                          warn(yyextra->yyFileName,yyextra->yyLineNr,
                                              "reached end of file while inside a '%s' block!\n"
                                              "The command that should end the block seems to be missing!\n",
                                              yyextra->docBlockName.data());
                                          yyterminate();
                                        }


<SkipCxxComment>[«»]                    {}
<SkipCxxComment>.*"\\\n"                {  // line continuation
                                          lineCount(yyscanner);
                                        }
<SkipCxxComment>.*/\n                   {
                                          BEGIN( yyextra->lastCContext ) ;
                                        }
<SkipComment>[«»]                       |
<SkipComment>\*                         |
<SkipComment>[^\*\n]+

  /* ------------ Generic rules -------------- */

<*>[\r\n]                               { lineCount(yyscanner); }
<*>\x00                                 |
<*>[ \t]+                               {
                                        }
<*>.                                    {
                                          printf("-> Xtend: Language Parser Error: Unexpected char: '%c' (0x%02X) inside file '%s'\n", *yytext, *yytext, yyextra->yyFileName.data());
  
                                        }
<SkipComment>"//"|"/*"
<*>"/*"                                 { yyextra->lastCContext = YY_START ;
                                          BEGIN( SkipComment ) ;
                                        }
<SkipComment>{B}*"*/"                   { BEGIN( yyextra->lastCContext ) ; }
<*>"//"                                 {
                                          yyextra->lastCContext = YY_START ;
                                          BEGIN( SkipCxxComment ) ;
                                        }
%%


// Xtend:
static char* extractClassName(char* fullName)
{
  char* result = fullName;
  char* p = fullName;
  while (*p != '\0')
  {
    if (*p == ':' && *(p+1) != '\0')
    {
      result = p+1;
    }
    p++;
  }
  return result;
}

//-----------------------------------------------------------------------------

static void initParser(yyscan_t yyscanner)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  yyextra->outerScopeEntries.clear();
  yyextra->baseName.resize(0);
  yyextra->protection = Public;
  yyextra->baseProt = Public;
  yyextra->sharpCount = 0;
  yyextra->roundCount = 0;
  yyextra->curlyCount = 0;
  yyextra->mtype = Method;
  yyextra->stat = FALSE;
  yyextra->virt = Normal;
  yyextra->baseVirt = Normal;
  yyextra->isTypedef = FALSE;
  yyextra->insideTryBlock = FALSE;
  yyextra->insideFormula = FALSE;
  yyextra->insideCode=FALSE;
  yyextra->previous = 0;
  yyextra->firstTypedefEntry.reset();
  yyextra->memspecEntry.reset();
}

static void initEntry(yyscan_t yyscanner)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  yyextra->protection = (yyextra->current_root->spec & (Entry::Interface|Entry::Enum)) ?  Public : Package;
  yyextra->current->protection = yyextra->protection ;
  yyextra->current->mtype      = yyextra->mtype;
  yyextra->current->virt       = yyextra->virt;
  yyextra->current->stat       = yyextra->stat;
  yyextra->current->lang       = yyextra->language;
  //printf("*** initEntry(yyscanner) yyextra->language=%d\n",yyextra->language);
  yyextra->commentScanner.initGroupInfo(yyextra->current.get());
  yyextra->isTypedef=FALSE;
}


//-----------------------------------------------------------------------------

static yy_size_t yyread(yyscan_t yyscanner,char *buf,yy_size_t max_size)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  yy_size_t c=0;
  while( c < max_size && yyextra->inputString[yyextra->inputPosition] )
  {
    *buf = yyextra->inputString[yyextra->inputPosition++] ;
    c++; buf++;
  }
  return c;
}

static void lineCount(yyscan_t yyscanner)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  int tabSize = Config_getInt(TAB_SIZE);
  const char *p;
  for (p = yytext ; *p ; ++p )
  {
    if (*p=='\n')
    {
      yyextra->yyLineNr++,yyextra->column=0,yyextra->yyColNr=1;
    }
    else if (*p=='\t')
    {
      yyextra->column+=tabSize - (yyextra->column%tabSize);
    }
    else
    {
      yyextra->column++,yyextra->yyColNr++;
    }
  }
  //printf("lineCount()=%d\n",yyextra->column);
}

static inline int computeIndent(const char *s,int startIndent)
{
  int col=startIndent;
  int tabSize=Config_getInt(TAB_SIZE);
  const char *p=s;
  char c;
  while ((c=*p++))
  {
    if (c=='\t') col+=tabSize-(col%tabSize);
    else if (c=='\n') col=0;
    else col++;
  }
  return col;
}

static void startCommentBlock(yyscan_t yyscanner,bool brief)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  if (brief)
  {
    yyextra->current->briefFile = yyextra->yyFileName;
    yyextra->current->briefLine = yyextra->yyLineNr;
  }
  else
  {
    yyextra->current->docFile = yyextra->yyFileName;
    yyextra->current->docLine = yyextra->yyLineNr;
  }
}

static void newEntry(yyscan_t yyscanner)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  if (yyextra->tempEntry==0) // if temp entry is not 0, it holds yyextra->current,
                    // and yyextra->current is actually replaced by yyextra->previous which was
                    // already added to yyextra->current_root, so we should not add it again
                    // (see bug723314)
  {
    yyextra->previous = yyextra->current;
    yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
  }
  else
  {
    yyextra->previous = yyextra->current;
    yyextra->current  = yyextra->tempEntry;
    yyextra->tempEntry.reset();
  }
  initEntry(yyscanner);
}

static void handleCommentBlock(yyscan_t yyscanner,const QCString &doc,bool brief)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  bool hideInBodyDocs = Config_getBool(HIDE_IN_BODY_DOCS);
  if (yyextra->docBlockInBody && hideInBodyDocs) return;
  //printf("parseCommentBlock [%s] brief=%d\n",doc.data(),brief);
  int lineNr = brief ? yyextra->current->briefLine : yyextra->current->docLine;   // line of block start

  // fill in inbodyFile && inbodyLine the first time, see bug 633891
  std::shared_ptr<Entry> docEntry = yyextra->docBlockInBody && yyextra->previous ? yyextra->previous : yyextra->current;
  if (yyextra->docBlockInBody && docEntry && docEntry->inbodyLine==-1)
  {
    docEntry->inbodyFile = yyextra->yyFileName;
    docEntry->inbodyLine = lineNr;
  }

  int position=0;
  bool needsEntry=FALSE;
  Markdown markdown(yyextra->yyFileName,lineNr);
  QCString strippedDoc = stripIndentation(doc);
  QCString processedDoc = Config_getBool(MARKDOWN_SUPPORT) ? markdown.process(strippedDoc,lineNr) : strippedDoc;
  while (yyextra->commentScanner.parseCommentBlock(
        yyextra->thisParser,
        yyextra->docBlockInBody && yyextra->previous ? yyextra->previous.get() : yyextra->current.get(),
        processedDoc, // text
        yyextra->yyFileName,   // file
        lineNr,       // line of block start
        yyextra->docBlockInBody ? FALSE : brief,               // isBrief
        yyextra->docBlockInBody ? FALSE : yyextra->docBlockAutoBrief,   // isJavaDocStyle
        yyextra->docBlockInBody,                               // isInBody
        yyextra->protection,
        position,
        needsEntry,
        Config_getBool(MARKDOWN_SUPPORT)
        )
     )
  {
    //printf("parseCommentBlock position=%d [%s]\n",position,doc.data()+position);
    if (needsEntry)
    {
      QCString docFile = yyextra->current->docFile;
      newEntry(yyscanner);
      yyextra->current->docFile = docFile;
      yyextra->current->docLine = lineNr;
    }
  }
  if (needsEntry)
  {
    newEntry(yyscanner);
  }

  if (yyextra->docBlockTerm)
  {
    unput(yyextra->docBlockTerm);
    yyextra->docBlockTerm=0;
  }
}

static void handleParametersCommentBlocks(yyscan_t yyscanner,ArgumentList &al)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  //printf(">>>>>>> handleParametersCommentBlocks()\n");
  for (Argument &a : al)
  {
    //printf("    Param %s docs=%s\n",a->name.data(),a->docs.data());
    if (!a.docs.isEmpty())
    {
      if  (!a.name && a.type == "...") a.name= "...";
      int position=0;
      bool needsEntry;

      // save context
      QCString orgDoc   = yyextra->current->doc;
      QCString orgBrief = yyextra->current->brief;
      int orgDocLine    = yyextra->current->docLine;
      int orgBriefLine  = yyextra->current->briefLine;

      yyextra->current->doc.resize(0);
      yyextra->current->brief.resize(0);

      //printf("handleParametersCommentBlock [%s]\n",doc.data());
      while (yyextra->commentScanner.parseCommentBlock(
             yyextra->thisParser,
             yyextra->current.get(),
             a.docs,             // text
             yyextra->yyFileName,         // file
             yyextra->current->docLine,   // line of block start
             FALSE,
             FALSE,
             FALSE,
             yyextra->protection,
             position,
             needsEntry,
             Config_getBool(MARKDOWN_SUPPORT)
            )
          )
      {
        //printf("handleParametersCommentBlock position=%d [%s]\n",position,doc.data()+position);
        if (needsEntry) newEntry(yyscanner);
      }
      if (needsEntry)
      {
        newEntry(yyscanner);
      }
      a.docs = yyextra->current->doc;

      // restore context
      yyextra->current->doc       = orgDoc;
      yyextra->current->brief     = orgBrief;
      yyextra->current->docLine   = orgDocLine;
      yyextra->current->briefLine = orgBriefLine;
    }
  }
}

static void parseCompounds(yyscan_t yyscanner,const std::shared_ptr<Entry> &rt)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  //printf("parseCompounds(%s)\n",rt->name.data());
  for (const auto &ce : rt->children())
  {
    if (!ce->program.isEmpty())
    {
      //printf("-- %s ---------\n%s\n---------------\n",
      //  ce->name.data(),ce->program.data());
      // init scanner state
      yyextra->padCount=0;
      //depthIf = 0;
      yyextra->column=0;
      yyextra->inputString = ce->program;
      yyextra->inputPosition = 0;
      if (ce->section==Entry::ENUM_SEC || (ce->spec&Entry::Enum))
        BEGIN( FindFields ) ;
      else
        BEGIN( FindMembers ) ;
      yyextra->current_root = ce;
      yyextra->yyFileName = ce->fileName;
      yyextra->yyLineNr = ce->bodyLine;
      yyextra->yyColNr = ce->bodyColumn;
      //printf("---> Inner block starts at line %d objC=%d\n",yyextra->yyLineNr,yyextra->insideObjC);
      yyextra->current = std::make_shared<Entry>();
      yyextra->stat = FALSE;
      initEntry(yyscanner);

      // deep copy group list from parent (see bug 727732)
      bool autoGroupNested = Config_getBool(GROUP_NESTED_COMPOUNDS);
      if (autoGroupNested && ce->section!=Entry::ENUM_SEC && !(ce->spec&Entry::Enum))
      {
        ce->groups = rt->groups;
      }

      int ni=ce->name.findRev("::"); if (ni==-1) ni=0; else ni+=2;
      // set default protection based on the compound type
      if( ce->section==Entry::CLASS_SEC ) // class
      {
        yyextra->current->protection = yyextra->protection = (ce->spec & (Entry::Interface|Entry::Enum)) ?  Public : Package;
      }
      else if (ce->section == Entry::ENUM_SEC ) // enum
      {
        yyextra->current->protection = yyextra->protection = ce->protection;
      }
      else if (!ce->name.isEmpty() && ce->name.at(ni)=='@') // unnamed union or namespace
      {
        if (ce->section == Entry::NAMESPACE_SEC ) // unnamed namespace
        {
          yyextra->current->stat = yyextra->stat = TRUE;
        }
        yyextra->current->protection = yyextra->protection = ce->protection;
      }
      else // named struct, union, protocol, category
      {
        yyextra->current->protection = yyextra->protection = Public ;
      }
      yyextra->mtype = Method;
      yyextra->virt = Normal;
      //printf("name=%s yyextra->current->stat=%d yyextra->stat=%d\n",ce->name.data(),yyextra->current->stat,yyextra->stat);

      //memberGroupId = DOX_NOGROUP;
      //memberGroupRelates.resize(0);
      //memberGroupInside.resize(0);
      QCString name = ce->name;
      yyextra->commentScanner.enterCompound(yyextra->yyFileName,yyextra->yyLineNr,name);

      xtendscannerYYlex(yyscanner);
      yyextra->lexInit=TRUE;
      //forceEndGroup();

      yyextra->commentScanner.leaveCompound(yyextra->yyFileName,yyextra->yyLineNr,name);

      ce->program.resize(0);


      //if (depthIf>0)
      //{
      //        warn(yyextra->yyFileName,yyextra->yyLineNr,"Documentation block ended in the middle of a conditional section!");
      //}
    }
    parseCompounds(yyscanner,ce);
  }
}

static void prependScope(yyscan_t yyscanner)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  if (yyextra->current_root->section & Entry::SCOPE_MASK)
  {
    //printf("--- prependScope %s to %s\n",yyextra->current_root->name.data(),yyextra->current->name.data());
    yyextra->current->name.prepend(yyextra->current_root->name+"::");
    //printf("prependScope #=%d #yyextra->current=%d\n",yyextra->current_root->tArgLists->count(),yyextra->current->tArgLists->count());
    for (const ArgumentList &srcAl : yyextra->current_root->tArgLists)
    {
      yyextra->current->tArgLists.insert(yyextra->current->tArgLists.begin(),srcAl);
    }
  }
}

static void addType(yyscan_t yyscanner)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  uint tl=yyextra->current->type.length();
  if( tl>0 && !yyextra->current->name.isEmpty() && yyextra->current->type.at(tl-1)!='.')
  {
    yyextra->current->type += ' ' ;
  }
  yyextra->current->type += yyextra->current->name ;
  yyextra->current->name.resize(0) ;
  tl=yyextra->current->type.length();
  if( tl>0 && !yyextra->current->args.isEmpty() && yyextra->current->type.at(tl-1)!='.')
  {
    yyextra->current->type += ' ' ;
  }
  yyextra->current->type += yyextra->current->args ;
  yyextra->current->args.resize(0) ;
  yyextra->current->argList.clear();
}


static QCString stripQuotes(const char *s)
{
  QCString name;
  if (s==0 || *s==0) return name;
  name=s;
  if (name.at(0)=='"' && name.at(name.length()-1)=='"')
  {
    name=name.mid(1,name.length()-2);
  }
  return name;
}

//-----------------------------------------------------------------------------

static void parseMain(yyscan_t yyscanner,
                      const char *fileName,
                      const char *fileBuf,
                      const std::shared_ptr<Entry> &rt)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  initParser(yyscanner);

  yyextra->inputString = fileBuf;
  yyextra->inputPosition = 0;
  yyextra->column = 0;
  xtendscannerYYrestart(0,yyscanner);

  //depthIf       = 0;
  yyextra->protection    = Public;
  yyextra->mtype         = Method;
  yyextra->stat          = FALSE;
  yyextra->virt          = Normal;
  yyextra->current_root  = rt;
  yyextra->yyLineNr      = 1 ;
  yyextra->yyBegLineNr   = 1;
  yyextra->yyBegColNr    = 0;
  yyextra->yyFileName = fileName;
  yyextra->language    = getLanguageFromFileName(yyextra->yyFileName);
  rt->lang = yyextra->language;
  msg("Parsing file %s...\n",yyextra->yyFileName.data());

  yyextra->current_root  = rt;
  initParser(yyscanner);
  yyextra->commentScanner.enterFile(yyextra->yyFileName,yyextra->yyLineNr);
  yyextra->current = std::make_shared<Entry>();
  //printf("yyextra->current=%p yyextra->current_root=%p\n",yyextra->current,yyextra->current_root);
  int sec=guessSection(yyextra->yyFileName);
  if (sec)
  {
    yyextra->current->name    = yyextra->yyFileName;
    yyextra->current->section = sec;
    yyextra->current_root->moveToSubEntryAndRefresh(yyextra->current);
  }
  yyextra->current->reset();
  initEntry(yyscanner);
  BEGIN( FindMembers );

  xtendscannerYYlex(yyscanner);
  yyextra->lexInit=TRUE;

  if (YY_START==Comment)
  {
    warn(yyextra->yyFileName,yyextra->yyLineNr,"File ended in the middle of a comment block! Perhaps a missing \\endcode?");
  }

  //forceEndGroup();
  yyextra->commentScanner.leaveFile(yyextra->yyFileName,yyextra->yyLineNr);

  rt->program.resize(0);

  parseCompounds(yyscanner,rt);

  //anonNSCount++;

  // add additional entries that were created during processing
  for (auto &kv: yyextra->outerScopeEntries)
  {
    //printf(">>> adding '%s' to scope '%s'\n",kv.second->name.data(),kv.first->name.data());
    kv.first->moveToSubEntryAndKeep(kv.second);
  }
  yyextra->outerScopeEntries.clear();

}

//----------------------------------------------------------------------------

struct XtendOutlineParser::Private
{
  yyscan_t yyscanner;
  xtendscannerYY_state state;
};

XtendOutlineParser::XtendOutlineParser() : p(std::make_unique<XtendOutlineParser::Private>())
{
  xtendscannerYYlex_init_extra(&p->state,&p->yyscanner);
#ifdef FLEX_DEBUG
  xtendscannerYYset_debug(1,p->yyscanner);
#endif
}

XtendOutlineParser::~XtendOutlineParser()
{
  xtendscannerYYlex_destroy(p->yyscanner);
}

void XtendOutlineParser::parseInput(const char *fileName,
                                  const char *fileBuf,
                                  const std::shared_ptr<Entry> &root,
                                  ClangTUParser *)
{
  struct yyguts_t *yyg = (struct yyguts_t*)p->yyscanner;
  yyextra->thisParser = this;

  printlex(yy_flex_debug, TRUE, __FILE__, fileName);

  ::parseMain(p->yyscanner,fileName,fileBuf,root);

  printlex(yy_flex_debug, FALSE, __FILE__, fileName);
}


//----------------------------------------------------------------------------

#if USE_STATE2STRING
#include "xtendscanner.l.h"
#endif