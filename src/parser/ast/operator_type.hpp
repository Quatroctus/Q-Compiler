#pragma once

#include <src/lexical/token.hpp>

#define AS(type, value) static_cast<type>(value)

enum struct OperatorType {
    ADD = AS(int, SingleType::PLUS), SUB = AS(int, SingleType::MINUS),
    MUL = AS(int, SingleType::ASTERISK), DIV = AS(int, SingleType::BACKSLASH),
    MOD = AS(int, SingleType::PERCENT), LPAREN = AS(int, SingleType::LPAREN),
    RPAREN = AS(int, SingleType::RPAREN),
    
    NEG
};
