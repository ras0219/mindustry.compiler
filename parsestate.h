#pragma once

#include "fwd.h"

enum ParserState_t
{
    PARSER_START,
    // expr: id ` | id ` = expr | id ` plist1
    PARSER_EXPR_ID,
    // expr: id = ` expr
    PARSER_EXPR_ASSIGN2,
    // expr: id = expr `
    PARSER_EXPR_ASSIGN3,
    // plist1: ( ` plist | ( ` )
    PARSER_EXPR_PAREN2,
    // expr: id ( ) `
    PARSER_EXPR_PLIST_END,
    // plist: expr ` ) | expr ` , plist
    PARSER_EXPR_PLIST,
    // stmt: expr ` ;
    PARSER_EXPR_STMT,
    // stmt: expr ; `
    PARSER_EXPR_STMT_SEMI,
};
