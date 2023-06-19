/******************************************************************************
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
%option prefix="xtendcodeYY"
%option reentrant bison-bridge
%option extra-type="struct xtendcodeYY_state *"

%top{
#include <stdint.h>
}

%{

#include <stdio.h>

#include "config.h"
#include "doxygen.h"
#include "outputgen.h"
#include "code.h"
#include "xtendcode_p.h"
#include "xtendcode.h"
#include "xtendcode_parse.h" // generated header file
#include "filedef.h"
#include "message.h"

#define YY_NO_INPUT 1
#define YY_NO_UNISTD_H 1
#define USE_STATE2STRING 0

#define DELETE_MINIMAL_INDENTATION 1

#if USE_STATE2STRING
static const char *stateToString(int state);
#endif

static yy_size_t yyread(yyscan_t yyscanner,char *buf,yy_size_t max_size);
static void nextCodeLine(yyscan_t yyscanner);
static int countLines(yyscan_t yyscanner);
static void startCodeLine(yyscan_t yyscanner);
static void startFontClass(yyscan_t yyscanner, const char *s);
static void endFontClass(yyscan_t yyscanner);

#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) result=yyread(yyscanner,buf,max_size);

%}

BN             [ \t\n\r]
ID             "\^"?"$"?[a-z_A-Z\x80-\xAA\xAC-\xBA\xBC-\xC1\xC3-\xFF][a-z_A-Z0-9\x80-\xAA\xAC-\xBA\xBC-\xC1\xC3-\xFF]*
SCOPENAME      "$"?(({ID}?{BN}*("::"|"."){BN}*)*)(((~|!){BN}*)?{ID})
GUILLEMETOPEN   \xC2\xAB
GUILLEMETCLOSED \xC2\xBB

XTENDINT       [0-9\_]+("e"[0-9\_]+)?
XTENDINTHEX    "0x"[a-fA-F0-9\_]+
XTENDLONG      [0-9\_]+(\.[0-9\_]+)?("e"[0-9\_]+)?[lL]
XTENDLONGHEX   "0x"[a-fA-F0-9\_]+"#"[lL]
XTENDBIGINT    [0-9\_]+(\.[0-9\_]+)?("e"[0-9\_]+)?[bB][iI]
XTENDBIGINTHEX "0x"[a-fA-F0-9\_]+"#"[bB][iI]

XTENDDOUBLEDEF [0-9\_]+\.[0-9\_]+("e"[0-9\_]+)?
XTENDDOUBLE    [0-9\_]+("."[0-9\_]+)?("e"[0-9\_]+)?[dD]
XTENDFLOAT     [0-9\_]+("."[0-9\_]+)?("e"[0-9\_]+)?[fF]
XTENDBIGDEC    [0-9\_]+("."[0-9\_]+)?("e"[0-9\_]+)?[bB][dD]

NumberLiteral   ({XTENDINT}|{XTENDINTHEX}|{XTENDLONG}|{XTENDLONGHEX}|{XTENDBIGINT}|{XTENDBIGINTHEX}|{XTENDDOUBLEDEF}|{XTENDDOUBLE}|{XTENDFLOAT}|{XTENDBIGDEC})

/* "<", ">" and "*" have an additional meaning and are therefore treated specially */
OPERATOR "+="|"-="|"*="|"/="|"%="|"||"|"&&"|"=="|"!="|"==="|"!=="|"<="|">="|"->"|".."|">.."|"..<"|"<>"|"?:"|"<=>"|"/"|"%"|"**"|"+"|"-"
OPERATORPOSTFIX "++"|"--"
/* "+" and "-" are prefix and infix operators */
OPERATORPREFIX "!"

%s      Top
%x      SkipString
%x      SkipCommentString
%x      SkipLineComment
%x      SkipBlockComment
%x      SkipTemp
%x      SkipTempComment
%x      SkipTempGuillemet
%x      SkipRoundAnnotation

%%

<SkipString>\\.           { yyextra->strToken += yytext; }
<SkipString>\'            |
<SkipString>\"            {
                            yyextra->strToken += yytext;
                            if(yytext[0] == yyextra->quoteType)
                            {
                              BEGIN(Top);
                              return TOK_STRING;
                            }
                          }
<SkipString>[\r\n]+       {
                            yyextra->strToken += yytext; yyextra->skippedIndentCount = 0;
                          }
<SkipString>[ \t]+        {
                            int tabEquivalent = Config_getBool(TAB_SIZE);
                            for (int i=0; i < yyleng; i++)
                            {
                              if (yyextra->skippedIndentCount >= yyextra->minimalIndentation)
                              {
                                yyextra->strToken += yytext[i];
                              }
                              else
                              {
                                if (yytext[i] == ' ') yyextra->skippedIndentCount++;
                                else yyextra->skippedIndentCount+=tabEquivalent;
                              }
                            }
                          }
<SkipString>»             |
<SkipString>«             |
<SkipString>[^ \t\r\n\\\"\']+ {
                            yyextra->strToken += yytext;
                          }
<SkipCommentString>\\.    { yyextra->comment += yytext; }
<SkipCommentString>\'     |
<SkipCommentString>\"     {
                            yyextra->comment += yytext;
                            if(yytext[0] == yyextra->quoteType)
                            {
                              BEGIN(SkipRoundAnnotation);
                            }
                          }
<SkipCommentString>[\r\n]+ {
                            yyextra->comment += yytext; yyextra->skippedIndentCount = 0;
                          }
<SkipCommentString>[ \t]+ {
                            int tabEquivalent = Config_getBool(TAB_SIZE);
                            for (int i=0; i < yyleng; i++)
                            {
                              if (yyextra->skippedIndentCount >= yyextra->minimalIndentation)
                              {
                                yyextra->comment += yytext[i];
                              }
                              else
                              {
                                if (yytext[i] == ' ') yyextra->skippedIndentCount++;
                                else yyextra->skippedIndentCount+=tabEquivalent;
                              }
                            }
                          }
<SkipCommentString>»      |
<SkipCommentString>«      |
<SkipCommentString>[^ \t\r\n\\\"\']+ {
                            yyextra->comment += yytext;
                          }

<SkipLineComment>[^\n\r]+   {
                            yyextra->comment += yytext;
                          }
<SkipLineComment>[\n\r]+  {
                            yyextra->skippedIndentCount = 0;
                            yyextra->comment += yytext;
                            BEGIN(yyextra->lastCommentContext);
                          }
<SkipBlockComment>"*"+/[^/] |
<SkipBlockComment>»       |
<SkipBlockComment>«       |
<SkipBlockComment>[^* \t\r\n]+ {
                            yyextra->comment += yytext;
                          }
<SkipBlockComment>"*/"    {
                            yyextra->comment += yytext;
                            BEGIN(yyextra->lastCommentContext);
                          }
<SkipBlockComment>[\r\n]+ {
                            yyextra->skippedIndentCount = 0;
                            yyextra->comment += yytext;
                          }
<SkipBlockComment>[ \t]+  {
                            int tabEquivalent = Config_getBool(TAB_SIZE);
                            for (int i=0; i < yyleng; i++)
                            {
                              if (yyextra->skippedIndentCount >= yyextra->minimalIndentation)
                              {
                                yyextra->comment += yytext[i];
                              }
                              else
                              {
                                if (yytext[i] == ' ') yyextra->skippedIndentCount++;
                                else yyextra->skippedIndentCount+=tabEquivalent;
                              }
                            }
                          }
<SkipTemp>\'\'\'          {
                            yyextra->strToken += yytext;
                            BEGIN(Top);
                            return TOK_TEMPEND;
                          }
<SkipTemp>««              {
                            yyextra->buffer += yytext;
                            BEGIN(SkipTempComment);
                            return TOK_TEMPPART;
                          }
<SkipTemp>«               {
                            yyextra->strToken += yytext;
                            BEGIN(SkipTempGuillemet);
                            return TOK_TEMPPART;
                          }
<SkipTemp>»               |
<SkipTemp>[^« \t\r\n]     {
                            yyextra->strToken += yytext;
                          }
<SkipTemp>[\r\n]+         {
                            yyextra->skippedIndentCount = 0;
                            yyextra->strToken += yytext;
                          }
<SkipTemp>[ \t]+          {
                            int tabEquivalent = Config_getBool(TAB_SIZE);
                            for (int i=0; i < yyleng; i++)
                            {
                              if (yyextra->skippedIndentCount >= yyextra->minimalIndentation)
                              {
                                yyextra->strToken += yytext[i];
                              }
                              else
                              {
                                if (yytext[i] == ' ') yyextra->skippedIndentCount++;
                                else yyextra->skippedIndentCount+=tabEquivalent;
                              }
                            }
                          }
<SkipTempGuillemet>««     {
                            BEGIN(SkipTempComment);
                            yyextra->buffer += yytext;
                          }
<SkipTempGuillemet>[^ \t\n\r] {
                            unput(*yytext);
                            BEGIN(Top);
                          }
<SkipTempGuillemet>[ \t]  {
                            int tabEquivalent = Config_getBool(TAB_SIZE);
                            if (yyextra->skippedIndentCount >= yyextra->minimalIndentation)
                            {
                              yyextra->comment += yytext[0];
                            }
                            else
                            {
                              if (yytext[0] == ' ') yyextra->skippedIndentCount++;
                              else yyextra->skippedIndentCount+=tabEquivalent;
                            }
                          }
<SkipTempGuillemet>[\n\r]+ {
                            yyextra->skippedIndentCount = 0;
                            yyextra->comment += yytext;
                          }
<SkipTempComment>[\n\r]   {
                            yyextra->skippedIndentCount = 0;
                            yyextra->comment += yytext;
                            BEGIN(SkipTemp);
                          }
<SkipTempComment>[^ \t\n\r]+ {
                            if (!yyextra->buffer.isEmpty())
                            {
                              yyextra->comment += yyextra->buffer;
                              yyextra->buffer = "";
                            }
                            yyextra->comment += yytext;
                          }
<SkipTempComment>[ \t]+   {
                            int tabEquivalent = Config_getBool(TAB_SIZE);
                            for (int i=0; i < yyleng; i++)
                            {
                              if (yyextra->skippedIndentCount >= yyextra->minimalIndentation)
                              {
                                yyextra->comment += yytext[i];
                              }
                              else
                              {
                                if (yytext[i] == ' ') yyextra->skippedIndentCount++;
                                else yyextra->skippedIndentCount+=tabEquivalent;
                              }
                            }
                          }
<SkipRoundAnnotation>"//" {
                            yyextra->lastCommentContext = YY_START; yyextra->comment += yytext; BEGIN(SkipLineComment);
                          }
<SkipRoundAnnotation>"/*" {
                            yyextra->lastCommentContext = YY_START; yyextra->comment += yytext; BEGIN(SkipBlockComment);
                          }
<SkipRoundAnnotation>\'   |
<SkipRoundAnnotation>\"   {
                            yyextra->comment += yytext;
                            yyextra->quoteType = yytext[0];
                            BEGIN(SkipCommentString);
                          }
<SkipRoundAnnotation>[\n\r]+ {
                            yyextra->skippedIndentCount = 0;
                            yyextra->comment += yytext;
                          }
<SkipRoundAnnotation>[ \t]+ {
                            int tabEquivalent = Config_getBool(TAB_SIZE);
                            for (int i=0; i < yyleng; i++)
                            {
                              if (yyextra->skippedIndentCount >= yyextra->minimalIndentation)
                              {
                                yyextra->comment += yytext[i];
                              }
                              else
                              {
                                if (yytext[i] == ' ') yyextra->skippedIndentCount++;
                                else yyextra->skippedIndentCount+=tabEquivalent;
                              }
                            }
                          }
<SkipRoundAnnotation>\)   {
                            yyextra->comment += yytext; BEGIN(Top);
                          }
<SkipRoundAnnotation>[^ \t\n\r\'\"\(] {
                            yyextra->comment += yytext;
                          }

 /* Brackets */
<Top>\{                   { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_CURLYOPEN; }
<Top>\}                   { yyextra->strToken += yytext; return TOK_CURLYCLOSED; }
<Top>\[                   { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_SQUAREOPEN; }
<Top>\]                   { yyextra->strToken += yytext; return TOK_SQUARECLOSED; }
<Top>\(                   {
                            if (yyextra->tryRes)
                            {
                              yyextra->blockSemicolon = FALSE;
                              yyextra->tryRes = FALSE;
                            }
                            if (!yyextra->blockSemicolon) yyextra->roundCount++;
                            yyextra->strToken += yytext;
                            return TOK_ROUNDOPEN; }
<Top>\)                   {
                            if (!yyextra->blockSemicolon)
                            {
                              if (yyextra->roundCount <= 1)
                              {
                                yyextra->blockSemicolon = TRUE;
                                yyextra->roundCount = 0;
                              }
                              else
                              {
                                yyextra->roundCount--;
                              }
                            }
                            yyextra->strToken += yytext;
                            return TOK_ROUNDCLOSED; }
<Top>\#                   { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_HASH; }

 /* Keywords */
<Top>"this"               { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_THIS; }
<Top>"it"                 { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_IT; }
<Top>"self"               { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_SELF; }
<Top>"super"              { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_SUPER; }
<Top>"class"              { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_CLASS; }
<Top>"extension"          { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_EXTENSION; }
<Top>"new"                { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_NEW; }
<Top>"if"                 { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_IF; }
<Top>"else"               { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_ELSE; }
<Top>"switch"             { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_SWITCH; }
<Top>"case"               { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_CASE; }
<Top>"default"            { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_DEFAULT; }
<Top>"for"                { yyextra->tryRes = FALSE; yyextra->blockSemicolon = FALSE; yyextra->strToken += yytext; return TOK_FOR; }
<Top>"while"              { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_WHILE; }
<Top>"do"                 { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_DO; }
<Top>"return"             { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_RETURN; }
<Top>"throw"              { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_THROW; }
<Top>"try"                { yyextra->tryRes = TRUE; yyextra->strToken += yytext; return TOK_TRY; }
<Top>"catch"              { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_CATCH; }
<Top>"finally"            { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_FINALLY; }
<Top>"throws"             { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_THROWS; }
<Top>"as"                 { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_AS; }
<Top>"instanceof"         { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_INSTANCEOF; }

 /* Member keywords */
<Top>"annotation"         { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_ANNOTATION; }
<Top>"enum"               { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_ENUM; }
<Top>"extends"            { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_EXTENDS; }
<Top>"implements"         { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_IMPLEMENTS; }
<Top>"interface"          { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_INTERFACE; }
<Top>"create"             { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_CREATE; }

 /* Common modifiers */
<Top>"public"             { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_PUBLIC; }
<Top>"private"            { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_PRIVATE; }
<Top>"protected"          { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_PROTECTED; }
<Top>"package"            { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_PACKAGE; }
<Top>"abstract"           { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_ABSTRACT; }
<Top>"static"             { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_STATIC; }
<Top>"dispatch"           { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_DISPATCH; }
<Top>"final"              { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_FINAL; }
<Top>"strictfp"           { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_STRICTFP; }
<Top>"native"             { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_NATIVE; }
<Top>"volatile"           { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_VOLATILE; }
<Top>"synchronized"       { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_SYNCHRONIZED; }
<Top>"transient"          { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_TRANSIENT; }

 /* Method modifiers */
<Top>"def"                { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_DEF; }
<Top>"override"           { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_OVERRIDE; }

 /* Field modifiers */
<Top>"val"                { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_VAL; }
<Top>"var"                { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_VAR; }

 /* Template */
 /* <Top>"ABSTRACT"           { yyextra->strToken += yytext; return TOK_TEMPABSTRACT; } */
<Top>"IF"                 { yyextra->strToken += yytext; return TOK_TEMPIF; }
<Top>"ELSEIF"             { yyextra->strToken += yytext; return TOK_TEMPELSEIF; }
<Top>"ELSE"               { yyextra->strToken += yytext; return TOK_TEMPELSE; }
<Top>"ENDIF"              { yyextra->strToken += yytext; return TOK_TEMPENDIF; }
<Top>"FOR"                { yyextra->strToken += yytext; return TOK_TEMPFOR; }
<Top>"BEFORE"             { yyextra->strToken += yytext; return TOK_TEMPBEFORE; }
<Top>"SEPARATOR"          { yyextra->strToken += yytext; return TOK_TEMPSEPARATOR; }
<Top>"AFTER"              { yyextra->strToken += yytext; return TOK_TEMPAFTER; }
<Top>"ENDFOR"             { yyextra->strToken += yytext; return TOK_TEMPENDFOR; }
<Top>\'\'\'               { yyextra->tryRes = FALSE; yyextra->strToken += yytext; BEGIN(SkipTemp); return TOK_TEMPBEGIN; }
<Top>»                    { yyextra->strToken += yytext; BEGIN(SkipTemp); }

 /* Operators */
<Top>"<"                  { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_LESSTHAN; }
<Top>">"                  { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_GREATERTHAN; }
<Top>"*"                  { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_ASTERISK; }
<Top>"&"                  { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_AND; }
<Top>"."                  { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_DOT; }
<Top>","                  { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_COMMA; }
<Top>"|"                  { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_BAR; }
<Top>"?"                  { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_QUESTIONMARK; }
<Top>"?."                 { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_NULLSAFE; }
<Top>"..."                { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_THREEDOTS; }
<Top>":"                  { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_COLON; }
<Top>"::"                 { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_DOUBLECOLON; }
<Top>"="                  { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_ASSIGN; }
<Top>{OPERATOR}           { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_OPINFIX; }
<Top>{OPERATORPREFIX}     { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_OPPREFIX; }
<Top>{OPERATORPOSTFIX}    { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_OPPOSTFIX; }
<Top>"=>"                 { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_DOUBLEARROW; }

 /* Annotation */

<Top>"@"{BN}*{SCOPENAME}  { yyextra->comment += yytext; }
<Top>"@"{BN}*{SCOPENAME}{BN}*\( { BEGIN(SkipRoundAnnotation); yyextra->comment += yytext; }

 /* Whitespace and indentation skipping */

<Top>[\r\n]+              { yyextra->comment += yytext; yyextra->skippedIndentCount = 0;}
<Top>[ \t]+               {
                            int tabEquivalent = Config_getBool(TAB_SIZE);
                            for (int i=0; i < yyleng; i++)
                            {
                              if (yyextra->skippedIndentCount >= yyextra->minimalIndentation)
                              {
                                yyextra->comment += yytext[i];
                              }
                              else
                              {
                                if (yytext[i] == ' ') yyextra->skippedIndentCount++;
                                else yyextra->skippedIndentCount+=tabEquivalent;
                              }
                            }
                          }
<Top>";"                  {
                            if (yyextra->blockSemicolon) 
                            {
                              yyextra->comment += yytext;
                            }
                            else
                            {
                              yyextra->strToken += yytext;
                              return TOK_SEMICOLON;
                            }
                          }

 /* Comments */

<Top>"//"                 { yyextra->lastCommentContext = YY_START; yyextra->comment += yytext; BEGIN(SkipLineComment); }
<Top>"/*"                 { yyextra->lastCommentContext = YY_START; yyextra->comment += yytext; BEGIN(SkipBlockComment); }

 /* Literals */
<Top>\"[^\\]\"            |
<Top>\'[^\\]\'            { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_CHAR; }
<Top>\'                   |
<Top>\"                   {
                            yyextra->tryRes = FALSE;
                            BEGIN(SkipString);
                            yyextra->strToken += yytext;
                            yyextra->quoteType = yytext[0];
                          }
<Top>"true"|"false"       { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_BOOLEANLIT; }
<Top>"null"               { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_NULLLIT; }
<Top>"typeof"             { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_TYPEOF; }
<Top>{NumberLiteral}      { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_NUMLIT; }
<Top>{ID}                 { yyextra->tryRes = FALSE; yyextra->strToken += yytext; return TOK_ID; }

<*>.                      {
                            if (yyextra->currentMemberDef && yyextra->sourceFileDef)
                            {
                              printf("-> Xtend: Source Lexer Error: '%c' inside member '%s' inside file '%s'.\n", yytext[0], yyextra->currentMemberDef->name().data(), yyextra->sourceFileDef->name().data());
                            }
                            else
                            {
                              if(yyextra->sourceFileDef)
                                printf("-> Xtend: Source Lexer Error: '%c' inside unknown member (probably a field initialization) inside file '%s'.\n", yytext[0], yyextra->sourceFileDef->name().data());
                            
                              else
                                printf("-> Xtend: Source Lexer Error: '%c' inside unknown member (probably a field initialization) inside unknown file.\n", yytext[0]);
                            }
                            return -1;
                          }
%%

static yy_size_t yyread(yyscan_t yyscanner,char *buf,yy_size_t max_size)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  yy_size_t inputPosition = yyextra->inputPosition;
  const char *s = yyextra->inputString + inputPosition;
  yy_size_t c=0;
  while( c < max_size && *s )
  {
    *buf++ = *s++;
    c++;
  }
  yyextra->inputPosition += c;
  return c;
}

static int calcMinimalIndentation(yyscan_t yyscanner)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  const char *p=yyextra->inputString;
  char c;
  int localMinimum=0;
  int globalMinimum=-1;
  bool blockTracking=FALSE;
  int tabEquivalent = Config_getBool(TAB_SIZE);
  while ((c=*p))
  {
    p++ ;
    if (c=='\n' || c=='\r')
    {
      if (blockTracking)
      {
        if (globalMinimum == -1) globalMinimum = localMinimum;
        else if (localMinimum < globalMinimum) globalMinimum = localMinimum;
        localMinimum = 0;
        blockTracking = FALSE;
      }
    }
    else if (c=='\t')
    {
      if (!blockTracking) localMinimum+=tabEquivalent;
    }
    else if (c==' ')
    {
      if (!blockTracking) localMinimum++;
    }
    else
    {
      blockTracking = TRUE;
    }
  }
  return globalMinimum == -1 ? 0 : globalMinimum;
}

static void codifyRemainingInput(yyscan_t yyscanner, const char *text, int offset)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  const char *p=text;
  unsigned char c;
  int colCounter = 0;
  int offsetCounter = 0;
  bool done = FALSE;
  bool blockCodifying = FALSE;
  int tabEquivalent = Config_getBool(TAB_SIZE);
  if (text == 0) return;

  while(!done)
  {
    while((c=*p++) && c!='\n')
    {
      if(DELETE_MINIMAL_INDENTATION)
      {
        if (c=='\t') colCounter+=tabEquivalent;
        else if (c==' ') colCounter++;
        else colCounter = yyextra->minimalIndentation + 1;
        
        if (colCounter > yyextra->minimalIndentation)
        {
          offsetCounter++;
          blockCodifying = FALSE;
        }
        else
        {
          blockCodifying = TRUE;
        }
      }
      else
      {
        offsetCounter++;
      }
      if ((offsetCounter > offset) && !blockCodifying)
      {
        if (c==0xC2)
        {
          offsetCounter++;
          c=*p++;
          if (c==0xBB)
          {
            offsetCounter++;
            unsigned char *tmp = (unsigned char*)malloc(3);
            tmp[0] = 0xC2;
            tmp[1] = 0xBB;
            tmp[2] ='\0';
            yyextra->code->codify((char *)tmp);
            free(tmp);
          }
          else if (c==0xAB)
          {
            offsetCounter++;
            unsigned char *tmp = (unsigned char*)malloc(3);
            tmp[0] = 0xC2;
            tmp[1] = 0xAB;
            tmp[2] ='\0';
            yyextra->code->codify((char *)tmp);
            free(tmp);
          }
        }
        else
        {
          char *tmp = (char*)malloc(2);
          tmp[0] = c;
          tmp[1] ='\0';
          yyextra->code->codify(tmp);
          free(tmp);
        }
      }
    }
    if (c=='\n')
    {
      if (*(p++) == '\0')
        done = TRUE;
      else
      {
        colCounter = 0;
        offsetCounter++;
        if ((offsetCounter > offset) && !blockCodifying)
        {
          yyextra->yyLineNr++;
          endCodeLine(yyscanner);
          startCodeLine(yyscanner);
        }
      }
      p--;
    }
    else if (c=='\0')
    {
      done = TRUE;
    }
  }
  return;
}

static void nextCodeLine(yyscan_t yyscanner)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  const char * fc = yyextra->currentFontClass;
  endCodeLine(yyscanner);
  if (yyextra->yyLineNr<yyextra->inputLines)
  {
    yyextra->currentFontClass = fc;
    startCodeLine(yyscanner);
  }
}

/*! counts the number of lines in the input */
static int countLines(yyscan_t yyscanner)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  const char *p=yyextra->inputString;
  char c;
  int count=1;
  while ((c=*p))
  {
    p++ ;
    if (c=='\n') count++;
  }
  if (p>yyextra->inputString && *(p-1)!='\n')
  { // last line does not end with a \n, so we add an extra
    // line and explicitly terminate the line after parsing.
    count++,
    yyextra->needsTermination=TRUE;
  }
  return count;
}

static void endCodeLine(yyscan_t yyscanner)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  //DBG_CTX((stderr,"endCodeLine(%d)\n",yyextra->yyLineNr));
  endFontClass(yyscanner);
  yyextra->code->endCodeLine();
}

static void startCodeLine(yyscan_t yyscanner)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
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

static void startFontClass(yyscan_t yyscanner, const char *s)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  endFontClass(yyscanner);
  yyextra->code->startFontClass(s);
  yyextra->currentFontClass=s;
}

static void endFontClass(yyscan_t yyscanner)
{
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;
  if (yyextra->currentFontClass)
  {
    yyextra->code->endFontClass();
    yyextra->currentFontClass=0;
  }
}

// public interface -----------------------------------------------------------

struct XtendCodeParser::Private
{
  yyscan_t yyscanner;
  struct xtendcodeYY_state state;
};

XtendCodeParser::XtendCodeParser() : p(std::make_unique<Private>())
{
  xtendcodeYYlex_init_extra(&p->state, &p->yyscanner);
#ifdef FLEX_DEBUG
  xtendcodeYYset_debug(1,p->yyscanner);
#endif
  resetCodeParserState();
}

XtendCodeParser::~XtendCodeParser()
{
  xtendcodeYYlex_destroy(p->yyscanner);
}

void XtendCodeParser::resetCodeParserState()
{
  struct yyguts_t *yyg = (struct yyguts_t*)p->yyscanner;
  yyextra->currentDefinition = 0;
  yyextra->currentMemberDef = 0;
}

void XtendCodeParser::parseCode(CodeOutputInterface &codeOutIntf,
               const char *scopeName,
               const QCString &input,
               SrcLangExt,
               bool isExampleBlock,
               const char *exampleName,
               FileDef *fileDef,
               int startLine,
               int endLine,
               bool inlineFragment,
               const MemberDef *memberDef,
               bool showLineNumbers,
               const Definition *searchCtx,
               bool collectXRefs
              )
{
  // TODO: this is called when u use @include /tmp/proj/Xtend.xtend -> syntax highlight, references etc
  yyscan_t yyscanner = p->yyscanner;
  struct yyguts_t *yyg = (struct yyguts_t*)yyscanner;

  if (input.isEmpty()) return;

  printlex(yy_flex_debug, true, __FILE__, fileDef ? fileDef->fileName().data(): NULL);

  yyextra->code = &codeOutIntf;
  yyextra->inputString   = input;
  yyextra->inputPosition = 0;
  yyextra->currentFontClass = 0;
  yyextra->needsTermination = false;

  yyextra->classScope=scopeName;
  yyextra->currentMemberDef=memberDef;
  yyextra->searchCtx=searchCtx;
  yyextra->collectXRefs=collectXRefs;

  if (startLine!=-1)
    yyextra->yyLineNr    = startLine;
  else
    yyextra->yyLineNr    = 1;

  if (endLine!=-1)
    yyextra->inputLines  = endLine+1;
  else
    yyextra->inputLines  = yyextra->yyLineNr + countLines(yyscanner) - 1;
  
  yyextra->yyColNr = 0;

  yyextra->exampleBlock  = isExampleBlock;
  yyextra->exampleName   = exampleName;
  yyextra->sourceFileDef = fileDef;
  yyextra->lineNumbers   = fileDef!=0 && showLineNumbers;

  bool cleanupSourceDef = false;

  if (isExampleBlock && fileDef==0)
  {
    // create a dummy filedef for the example
    yyextra->sourceFileDef = createFileDef("",(exampleName?exampleName:"generated"));
    cleanupSourceDef = true;
  }

  yyextra->includeCodeFragment = inlineFragment;
  if (DELETE_MINIMAL_INDENTATION == 1)
  {
    yyextra->minimalIndentation = calcMinimalIndentation(yyscanner);
  }
  startCodeLine(yyscanner);

  xtendcodeYYrestart( 0, yyscanner );
  BEGIN(Top);
  //xtendcodeYYlex(yyscanner);
  if (xtendcodeYYparse(yyscanner) != 0)
  {
    codifyRemainingInput(yyscanner, yyextra->inputString, yyextra->codifiedPosition);
  }

  if (cleanupSourceDef)
  {
    // delete the temporary file definition used for this example
    delete yyextra->sourceFileDef;
    yyextra->sourceFileDef=0;
  }

  printlex(yy_flex_debug, false, __FILE__, fileDef ? fileDef->fileName().data(): NULL);
}

//---------------------------------------------------------------------------------

extern "C" {
  int xtendcodeYYwrap(yyscan_t yyscanner) { return 1; }
}

#if USE_STATE2STRING
#include "xtendcode.l.h"
#endif
