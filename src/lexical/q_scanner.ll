%{
    #include <iostream>

    #include <src/lexical/lexer.hpp>
    #include <src/lexical/location.hpp>
    #include <src/lexical/token.hpp>
    #include <src/util/string.hpp>

    #define YY_DECL Token yylex(SpanLocation& loc)
    YY_DECL;
%}

%option noyywrap nounput noinput batch debug
%option yylineno

DIG [0-9]

%{
    #define YY_USER_ACTION loc.columns(yyleng);
%}

%%
%{
    loc.step();
%}

^[ \t]*\r?\n { loc.lines(); loc.step(); }

^[ \t]*"//".*\r?\n { loc.lines(); loc.step(); }
"//".*\r?\n { loc.lines(); loc.step(); }

"/*"[^("*/")]*"*/" { loc.lines(Util::Count(yytext, '\n')); loc.columns(strlen(yytext) - Util::FindLast(yytext, '\n')); loc.step(); }

\r?\n { loc.lines(); loc.step(); }

[ \t] { loc.step(); }

"0x"[0-9A-Fa-f]+ {
    return Lexer::MakeInteger(std::string{yytext+2}, 16, loc);
}

"0"[0-7]+ {
    return Lexer::MakeInteger(std::string{yytext+1}, 8, loc);
}

"0b"[01]+ {
    return Lexer::MakeInteger(std::string{yytext+2}, 2, loc);
}

{DIG}+ {
    return Lexer::MakeInteger(std::string{yytext}, 10, loc);
}

({DIG}+"."{DIG}*)|("."{DIG}+) {
    return Lexer::MakeFloating(std::string{yytext}, true, loc);
}

(({DIG}+"."{DIG}*)|("."{DIG}+))f {
    std::string s{yytext};
    s.erase(s.end() - 1);
    return Lexer::MakeFloating(std::move(s), false, loc);
}

("true")|("false") {
    return Lexer::MakeBoolean(strcmp(yytext, "false"), loc);
}

\"(\\.|[^"\\(\r?\n)])*\" {
    std::string s{yytext};
    s.erase(s.begin());
    s.erase(s.end() - 1);
    return Lexer::MakeStringLiteral(std::move(s), loc);
}

"if" {
    return Lexer::MakeIF(loc);
}

"elif" {
    return Lexer::MakeELIF(loc);
}

"else" {
    return Lexer::MakeELSE(loc);
}

"offer" {
    return Lexer::MakeOFFER(loc);
}

"exert" {
    return Lexer::MakeEXERT(loc);
}

"nix" {
    return Lexer::MakeNIX(loc);
}

"=="    { return Lexer::MakeDEQUAL(loc); }
"="     { return Lexer::MakeEQUAL(loc); }
"+"     { return Lexer::MakePLUS(loc); }
"-"     { return Lexer::MakeMINUS(loc); }
"*"     { return Lexer::MakeASTERISK(loc); }
"/"     { return Lexer::MakeBACKSLASH(loc); }
"%"     { return Lexer::MakePERCENT(loc); }


"("     { return Lexer::MakeLPAREN(loc); }
")"     { return Lexer::MakeRPAREN(loc); }
"{"     { return Lexer::MakeLCURLY(loc); }
"}"     { return Lexer::MakeRCURLY(loc); }
"["     { return Lexer::MakeLBRACE(loc); }
"]"     { return Lexer::MakeRBRACE(loc); }

","     { return Lexer::MakeCOMMA(loc); }
";"     { return Lexer::MakeSEMICOLON(loc); }


([A-Za-z_][A-Za-z_0-9]+"::")+ {
    const char* const delim = "::";
    return Lexer::MakeNamespace(Util::Split(yytext, delim), loc);
}

[A-Za-z_][A-Za-z_0-9]* {
    return Lexer::MakeIdentifier(std::string{yytext}, loc);
}

<<EOF>> {
    return Lexer::MakeEND(loc);
}

. {
    return Lexer::MakeError(std::string{yytext[0]}, loc);
}

%%

void Lexer::scan_begin(const std::string& filename) {
    yy_flex_debug = false;
    if (filename.empty() || filename == "-") {
        yyin = stdin;
    } else if (!(yyin = fopen(filename.c_str(), "r"))) {
        std::cerr << "Cannot open " << filename << strerror(errno) << "\n";
        exit(EXIT_FAILURE);
    }
}

void Lexer::scan_end() {
    fclose(yyin);
    yylex_destroy();
}