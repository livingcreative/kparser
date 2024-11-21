/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017

    https://github.com/livingcreative/kparser

    kexpression.h
        classes for parsing expressions (like a = b + c * d)
*/

#pragma once

#include "khelpers.h"
#include <algorithm>


namespace k_parser
{

    // -------------------------------------------------------------------------
    //  Operand
    // -------------------------------------------------------------------------
    //
    //  example class for operand object
    //      this class has no implementation!
    //      shows interface that real Operand class should implement
    //  operand can hold a value to perform immediate expression computations
    //  (such as constants)
    //  or it can hold AST node which will be used to build AST while parsing
    //  expression

    class Operand
    {
    public:
        // construct empty (unassigned operand)
        //      real Operand class can implement other constructors in order to
        //      have specific values and data which will be assigned as a result
        //      of parsing
        Operand();

        // Operand must implement assignment operator
        Operand &operator=(const Operand &other);

        // Determines operand validity
        // invalid operands can serve as placeholders for "non stop" parsing
        constexpr operator bool() const noexcept;
    };


    // operator group defines operator kind, there are three groups
    //      Unary operators act on single operand
    //      Binary operators act on two operands
    //      Ternary operators act on 3 operands (e.g. ?: conditional operator)
    enum class OperatorGroup
    {
        Unary,
        Binary,
        Ternary
    };

    // -------------------------------------------------------------------------
    //  Operator
    // -------------------------------------------------------------------------
    //
    //  example class for operator object
    //      this class has no implementation!
    //      shows interface that real Operator class should implement
    //  operator can be applied in-place for immediate computation and return
    //  computed value or it can return AST node which will contain passed
    //  operands as child nodes
    //
    //  definitionT - implementation specific operator definition metadata

    template <typename definitionT>
    class Operator
    {
    public:
        // construct empty (invalid) operator
        Operator();
        // construct operator from given definition
        Operator(const definitionT &def);

        // Apply unary operation
        Operand operator()(const Operand &other) const;
        // Apply binary operation
        Operand operator()(const Operand &left, const Operand &right) const;
        // Apply ternary operation
        Operand operator()(const Operand &cond, const Operand &a, const Operand &b) const;

        // Determines operator validity, returns true if requested operator has
        // been correctly parsed. Expression parser must return empty invalid
        // operator in case of parser error
        constexpr operator bool() const noexcept;

        // Determines operator associativity, true for right to left operators
        constexpr bool rtl() const noexcept;
        // Determines operator precedence level, higher the number - higher
        // the precedence
        constexpr int level() const noexcept;
    };


    // -------------------------------------------------------------------------
    //  ExpressionTokenParser
    // -------------------------------------------------------------------------
    //
    //  example class of expression token parser interface
    //      this class has no implementation!
    //      it just shows what functions real ExpressionTokenParser class should
    //      implement

    template <typename operandT, typename definitionT>
    class ExpressionTokenParser
    {
    public:
        // parse and return (primary) operand object
        operandT Operand();

        // parse left bracket for grouping expressions
        // not mandatory, can safely return false if there's no bracket
        bool LeftBracket();
        // parse right bracket for grouping expressions
        // presense of right bracket is mandatory, therefore returning false indicates error
        bool RightBracket();

        // parse and return whether current token matches given operator definition
        const definitionT *Operator(int level, OperatorGroup group);

        // parse ternary operator "separator" part (e.g. : token in ?: conditional operator)
        // presense of separator is mandatory, therefore returning false indicates error
        bool TernarySeparator();
    };


    // -------------------------------------------------------------------------------
    //  ExpressionParser
    // -------------------------------------------------------------------------------
    //
    //  class for parsing (and optionally evaluating) expressions
    //  call Expression() function of class instance to parse expression with given
    //  token parser
    //      P  - expression token parser type
    //      F  - operator type
    //      Op - operand type
    //      D  - operator definition type

    template <
        typename parserT,
        typename operatorT, typename operandT
    >
    class ExpressionParser
    {
    public:
        // parse expression
        // returns parsed expression operand, which is depending on implementation
        // might be expression computed value or expression AST node
        static constexpr operandT Expression(parserT &parser) noexcept;

    private:
        // parse operand (value or node)
        static constexpr operandT Operand(parserT &parser) noexcept;
        // parse operator with requested level and group
        static constexpr operatorT Operator(parserT &parser, int level, OperatorGroup group) noexcept;
        // helper to get operand as possible subexpression result with higher operator precedence
        static constexpr operandT OperatorOperand(parserT &parser, int level) noexcept;
    };

    #define TT template <typename parserT, typename operatorT, typename operandT>
    #define TTP parserT, operatorT, operandT

    TT constexpr operandT
    ExpressionParser<TTP>::Expression(parserT &parser) noexcept
    {
        // take expression as operator operand with lowest precedence level
        return OperatorOperand(parser, 0);
    }


    TT constexpr operandT
    ExpressionParser<TTP>::Operand(parserT &parser) noexcept
    {
        // check if there's a subexpression in brackets
        auto sub = parser.LeftBracket();

        if (!sub) {
            // parse regular operand
            return parser.Operand();
        }

        // parse subexpression as independent expression
        auto op = Expression(parser);

        if (parser.RightBracket()) {
            return op;
        }

        // TODO: handle error - absense of mandatory right bracket
        return op;
    }

    TT constexpr operatorT
    ExpressionParser<TTP>::Operator(parserT &parser, int level, OperatorGroup group) noexcept
    {
        auto result = parser.Operator(level, group);

        if (result != nullptr) {
            return operatorT(*result);
        }

        // return empty error operator if required operator was not parsed
        return operatorT();
    }

    TT constexpr operandT
    ExpressionParser<TTP>::OperatorOperand(parserT &parser, int level) noexcept
    {
        operandT result;

        // unroll unary operators
        if (auto opf = Operator(parser, level, OperatorGroup::Unary)) {
            result = opf(OperatorOperand(parser, opf.level()));
        } else {
            result = Operand(parser);
        }

        // unroll binary operators
        while (true) {
            // try to get binary operator
            auto opf = Operator(parser, level, OperatorGroup::Binary);
            if (opf) {
                auto level = opf.level() + (opf.rtl() ? 0 : 1);
                // apply binary operator to left and right hand side operands
                // and replace left as a result
                result = opf(
                    result,
                    OperatorOperand(parser, level)
                );
            } else {
                // try to get ternary operator
                auto ternary = Operator(parser, level, OperatorGroup::Ternary);
                if (!ternary) {
                    // stop expression parsing in case there's no more operators
                    break;
                }

                // left part of ternary operator subexpression (right after operator itself)
                // parsed as independent subexpression
                auto a = Expression(parser);

                // parse ternary operator separator
                // TODO: handle error
                parser.TernarySeparator();

                // right part of ternary operator subexpression parsed as regular operator operand
                auto b = OperatorOperand(parser, ternary.level() + 1);

                // apply ternaryoperator to all three operands
                result = ternary(result, a, b);
            }
        }

        return result;
    }

    #undef TT
    #undef TTP

} // namespace k_scanner
