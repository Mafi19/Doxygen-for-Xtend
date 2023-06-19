/******************************************************************************
 *
 * Copyright (C) 1997-2019 by Dimitri van Heesch.
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

#ifndef _XTENDCODE_P_H
#define _XTENDCODE_P_H

#include <stdio.h>
#include <qcstring.h>

#include "config.h"
#include "doxygen.h"
#include "outputgen.h"
#include "code.h"
#include "filedef.h"
#include "message.h"
#include "cppvalue.h"


//! @file
//! @brief Private interface between Parser (xtendcode.y) and Lexer (xtendcode.l)

#include "cppvalue.h"
#define YYSTYPE CPPValue

typedef void* yyscan_t;
typedef size_t yy_size_t;

struct xtendcodeYY_state
{
     CodeOutputInterface * code;
     const char   *inputString;     //!< the code fragment as text
     yy_size_t     inputPosition;   //!< read offset during parsing
     int           inputLines = 0;      //!< number of line in the code fragment
     int           yyLineNr = 0;        //!< current line number
     yy_size_t     yyColNr = 0;         //!< current column number
     bool          needsTermination;
     
     int           lastCommentContext = 0;
     QCString      strToken;
     char          quoteType;
     QCString      comment;
     QCString      buffer;
     int           codifiedPosition = 0;

     bool          blockSemicolon = TRUE;
     bool          tryRes = FALSE;
     int           roundCount = 0;
     int           skippedIndentCount = 0;
     int           minimalIndentation = -1;

     bool          lineNumbers = FALSE;
     const Definition   *searchCtx;
     bool          collectXRefs = FALSE;

     int           lastContext = 0;

     bool          exampleBlock;
     QCString      exampleName;
     QCString      classScope;

     FileDef    *sourceFileDef;
     const Definition *currentDefinition;
     const MemberDef  *currentMemberDef;
     bool          includeCodeFragment;
     const char   *currentFontClass;
};
xtendcodeYY_state* xtendcodeYYget_extra(yyscan_t yyscanner);
static void endFontClass(yyscan_t yyscanner);
static void startFontClass(yyscan_t yyscanner, const char *s);
static void nextCodeLine(yyscan_t yyscanner);
static void codifyLines(yyscan_t yyscanner,const char *text);static void startCodeLine(yyscan_t yyscanner);
static void startCodeLine(yyscan_t yyscanner);
static void endCodeLine(yyscan_t yyscanner);
static void codify(yyscan_t yyscanner, const char *text, const char *style);
static void setCurrentDoc(yyscan_t yyscanner,const QCString &anchor);

extern int xtendcodeYYlex(YYSTYPE *lvalp, yyscan_t);
extern int xtendcodeYYparse(yyscan_t);

#endif