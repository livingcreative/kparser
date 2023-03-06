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

    // -------------------------------------------------------------------------------
    //  Operand
    // -------------------------------------------------------------------------------
    //
    //  example class for operand object
    //      this class has no implementation!
    //      shows interface that real Operand class should implement
    //  operand can hold a value to perform immediate expression computations (such as constants)
    //  or it can hold AST node which will be used to build AST while parsing expression

    class Operand
    {
    public:
        // construct empty (unassigned operand)
        //      real Operand class can implement other constructors in order to
        //      have specific values and data which will be assigned as a result of parsing
        Operand();

        // Operand must implement assignment operator
        Operand &operator=(const Operand &other);
    };


    // operator group defines operator kind, there are three groups
    //      Unary operators act on single operand
    //      Binary operators act on two operands
    //      Ternary operators act on three operands (e.g. ?: conditional operator)
    enum class OperatorGroup
    {
        Unary,
        Binary,
        Ternary
    };

    // -------------------------------------------------------------------------------
    //  Operator
    // -------------------------------------------------------------------------------
    //
    //  example class for operator object
    //      this class has no implementation!
    //      shows interface that real Operator class should implement
    //  operator can be applied in-place for immediate computation and return computed value
    //  or it can return AST node which will contain passed operands as children nodes

    template <typename D>
    class Operator
    {
    public:
        // construct empty (invalid) operator
        Operator();
        // construct operator from given definition
        Operator(const D &def);

        // Apply unary operation
        Operand operator()(const Operand &other) const;
        // Apply binary operation
        Operand operator()(const Operand &left, const Operand &right) const;
        // Apply ternary operation
        Operand operator()(const Operand &cond, const Operand &a, const Operand &b) const;

        // Determines operator validity, returns true if requested operator has been correctly parsed
        // expression parser must return empty invalid operator in case of parser error
        constexpr operator bool() const noexcept;

        // Determines operator associativity, true for right to left operators
        constexpr bool rtl() const noexcept;
        // Determines operator precedence level, higher the number - higher the precedence
        constexpr int level() const noexcept;
    };


    // -------------------------------------------------------------------------------
    //  ExpressionTokenParser
    // -------------------------------------------------------------------------------
    //
    //  example class of expression token parser interface
    //      this class has no implementation!
    //      it just shows what functions real ExpressionTokenParser class should implement

    template <typename Op, typename D>
    class ExpressionTokenParser
    {
    public:
        // parse and return operand object
        Op Operand();

        // parse left bracket for grouping expressions
        // not mandatory, can safely return false if there's no bracket
        bool LeftBracket();
        // parse right bracket for grouping expressions
        // presense of right bracket is mandatory, therefore returning false indicates error
        bool RightBracket();

        // parse and return whether current token matches given operator definition
        bool Operator(const D &def);

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

    template <typename P, typename F, typename Op, typename D>
    class ExpressionParser
    {
    public:
        // minimum required operator definition struct example
        /*
        struct OperatorDefinition
        {
            OperatorGroup group;
            int           level;
        };
        */

        // construct expression parser instance with given token parser implementation
        constexpr ExpressionParser(P &tokenparser, const FixedArray<D> &operators) noexcept;

        // parse expression
        // returns parsed expression operand, which is depending on implementation
        // might be expression computed value or expression AST node
        Op Expression();

    private:
        // parse operand (value or node)
        Op Operand();
        // parse operator with requested level and group
        F Operator(int level, OperatorGroup group);
        // helper to get operand as possible subexpression result with higher operator precedence
        Op OperatorOperand(int level);

    private:
        P                   &p_tokenparser;
        const FixedArray<D>  p_operators;
    };


    template <typename P, typename F, typename Op, typename D>
    constexpr ExpressionParser<P, F, Op, D>::ExpressionParser(P &tokenparser, const FixedArray<D> &operators) noexcept :
        p_tokenparser(tokenparser),
        p_operators(operators)
    {}

    template <typename P, typename F, typename Op, typename D>
    Op ExpressionParser<P, F, Op, D>::Expression()
    {
        // take expression as operator operand with lowest precedence level
        return OperatorOperand(0);
    }


    template <typename P, typename F, typename Op, typename D>
    Op ExpressionParser<P, F, Op, D>::Operand()
    {
        // check if there's a subexpression in brackets
        auto sub = p_tokenparser.LeftBracket();

        if (sub) {
            // parse subexpression as independent expression
            auto op = Expression();
            // TODO: handle error - upsense of mandatory right bracket
            p_tokenparser.RightBracket();
            return op;
        } else {
            // parse regular operand
            return p_tokenparser.Operand();
        }
    }

    template <typename P, typename F, typename Op, typename D>
    F ExpressionParser<P, F, Op, D>::Operator(int level, OperatorGroup group)
    {
        // parse operator
        auto op = std::find_if(
            p_operators.begin(), p_operators.end(),
            [level, group, this](auto &od) {
                // skip definitions with unwanted group/level
                if (od.group != group || level > od.level) {
                    return false;
                }
                // try to parse matching operator definition and return it on match
                return p_tokenparser.Operator(od);
            }
        );

        // return empty operator if nothing was parsed or found one otherwise
        return op == p_operators.end() ? F() : F(*op);
    }

    template <typename P, typename F, typename Op, typename D>
    Op ExpressionParser<P, F, Op, D>::OperatorOperand(int level)
    {
        Op result;

        // unroll unary operators
        if (auto opf = Operator(level, OperatorGroup::Unary)) {
            result = opf(OperatorOperand(opf.level()));
        } else {
            result = Operand();
        }

        // unroll binary operators
        while (true) {
            // try to get binary operator
            auto opf = Operator(level, OperatorGroup::Binary);
            if (opf) {
                // apply binary operator to left and right hand side operands and replace left as a result
                result = opf(result, OperatorOperand(opf.level() + (opf.rtl() ? 0 : 1)));
            } else {
                // try to get ternary operator
                auto ternary = Operator(level, OperatorGroup::Ternary);
                if (!ternary) {
                    // stop expression parsing in case there's no more operators
                    break;
                }

                // left part of ternary operator subexpression (right after operator itself)
                // parsed as independent subexpression
                auto a = Expression();

                // parse ternary operator separator
                // TODO: handle error
                p_tokenparser.TernarySeparator();

                // right part of ternary operator subexpression parsed as regular operator operand
                auto b = OperatorOperand(ternary.level() + 1);

                // apply ternaryoperator to all three operands
                result = ternary(result, a, b);
            }
        }

        return result;
    }

} // namespace k_scanner
