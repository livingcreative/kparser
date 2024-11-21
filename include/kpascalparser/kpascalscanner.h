/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017

    https://github.com/livingcreative/kparser

    kpascalscanner.h
        Example Pascal scanner implementation, general example on using
        Scanner class
*/

#pragma once

#include "kparser/kscanner.h"
#include "kparser/kscannercommon.h"


namespace k_pascalparser
{

    template <typename Tsource>
    class PascalScanner : public k_parser::Scanner<Tsource>, public k_parser::ScannerCommonDefs
    {
    public:
        PascalScanner(Tsource &source);

        // namespaces are shit
        using ScannerSourceIterator = k_parser::ScannerSourceIterator;
        using SourceLength = k_parser::SourceLength;
        using ScanResult = k_parser::ScanResult;

        enum class TokenType
        {
            None,         // token haven't been scanned yet
            Directive,    // comment with directive
            Identifier,   // any valid identifier
            Keyword,      // keyword (special identifiers)
            Number,       // any integer number or hexadecimal (might be incomplete)
            RealNumber,   // any real (float or double) number (might be incomplete)
            String,       // any string or character (might be incomplete or malformed)
            Comment,      // any comment (single- or multi-line)
            Symbol,       // any standalone character or compiund sequence
            Spacer,       // sequence of spaces/line breaks
            Invalid       // invalid token/character
        };

        using Token = k_parser::ScannerToken<TokenType>;

        enum class Mode
        {
            Source,
            Comment,
            Directive,
            OldStyleComment,
            OldStyleDirective,
            Assembler
        };

        Token ReadToken(Mode mode, bool includespacers = false);

    private:
        bool ScanIdent(ScannerSourceIterator &it) const;
        bool ScanComment(Token &token, ScannerSourceIterator &it) const;
        bool ScanDirective(Token &token, ScannerSourceIterator &it) const;
        k_parser::ScanResult ScanString(ScannerSourceIterator &it) const;
        k_parser::ScanResult ScanDQString(ScannerSourceIterator &it) const;
        bool ScanHexadecimal(ScannerSourceIterator &it) const;
        bool ScanDecimal(ScannerSourceIterator &it) const;
        bool ScanReal(ScannerSourceIterator &it) const;

    private:
        using TokenChar = k_parser::TokenT<char>;
        using CharRange = k_parser::CharRange;

        static const CharRange p_asmalpha[5];    // inline assembler alpha characters set (not exact, unicode range needs refinement)
        static const CharRange p_asmalphanum[6]; // inline assembler alpha + numeric characters set

        static const TokenChar p_compounds[7];   // all compound sequences

        static const TokenChar p_keywords[71];   // all Pascal keywords
    };


    template <typename Tsource>
    const typename PascalScanner<Tsource>::CharRange PascalScanner<Tsource>::p_asmalpha[] = {
        { '_', '_' },
        { 'A', 'Z' },
        { 'a', 'z' },
        { '@', '@' },
        // TODO: refine alpha range
        { L'\x0100', L'\xFFFF' }
    };

    template <typename Tsource>
    const typename PascalScanner<Tsource>::CharRange PascalScanner<Tsource>::p_asmalphanum[] = {
        { '_', '_' },
        { 'A', 'Z' },
        { 'a', 'z' },
        { '@', '@' },
        { L'\x0100', L'\xFFFF' },
        { '0', '9' }
    };

    template <typename Tsource>
    const typename PascalScanner<Tsource>::TokenChar PascalScanner<Tsource>::p_compounds[] = {
        ":=", ">=", "<=", "<>", "..", "(.", ".)"
    };

    template <typename Tsource>
    const typename PascalScanner<Tsource>::TokenChar PascalScanner<Tsource>::p_keywords[] = {
        "and", "array", "as", "asm", "begin", "break", "case", "class", "const", "constructor",
        "continue", "destructor", "div", "do", "downto", "else", "end", "except", "finalization",
        "finally", "for", "function", "goto", "if", "implementation", "in", "inherited",
        "initialization", "inline", "interface", "is", "label", "library", "mod", "new", "nil",
        "not", "object", "of", "on", "operator", "or", "out", "packed", "private", "procedure",
        "program", "property", "protected", "public", "published", "raise", "record", "repeat",
        "set", "shl", "shr", "strict", "string", "then", "threadvar", "to", "try", "type",
        "unit", "until", "uses", "var", "while", "with", "xor"
    };

    template <typename Tsource>
    PascalScanner<Tsource>::PascalScanner(Tsource &source) :
        Scanner(source)
    {}

    template <typename Tsource>
    typename PascalScanner<Tsource>::Token PascalScanner<Tsource>::ReadToken(Mode mode, bool includespacers)
    {
        auto it = It();

        if (it) {
            return Token();
        }

        switch (mode) {
            case Mode::Comment:
            case Mode::Directive: {
                auto result = ContinueTo(C("}"), true, nullptr, it);
                return Token(
                    mode == Mode::Comment ? TokenType::Comment : TokenType::Directive,
                    Scanner::ReadToken(it), result
                );
            }

            case Mode::OldStyleComment:
            case Mode::OldStyleDirective: {
                auto result = ContinueTo(C("*)"), true, nullptr, it);
                return Token(
                    mode == Mode::OldStyleComment ? TokenType::Comment : TokenType::Directive,
                    Scanner::ReadToken(it), result
                );
            }

            default: {
                auto hastoken = SkipToToken(it, true);

                if (includespacers && PeekToken(it)) {
                    return Token(TokenType::Spacer, Scanner::ReadToken(it), ScanResult::Match);
                }

                DiscardToken(it);

                if (!hastoken) {
                    return Token();
                }
            }
        }

        Token token;
        auto c = CharCurrent();

        // identifier starts with following characters, so it's most
        // high probability to try scan identifier first
        if (c == '_' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' ||
            c >= L'\x0100' || (c == '&' && mode == Mode::Assembler))
        {
            // try to scan identifier
            if (ScanIdent(it)) {
                token.Type = TokenType::Identifier;

                if (TokenCheckAnyCI(PeekToken(it), A(p_keywords)) != NO_MATCH) {
                    token.Type = TokenType::Keyword;
                }
            }
        }
        // next most frequent token type is comment, comments start with /
        // character, so try scan a comment when / encountered
        else if (c == '/' || c == '{' || c == '(')
        {
            if (!ScanComment(token, it) && !ScanDirective(token, it)) {
                CheckAny(C("/{("), it);
                token.Type = TokenType::Symbol;
                token.Result = ScanResult::Match;
            }
        }
        // from this character string literal can start
        // try scan string
        else if (c == '\'' || c == '#')
        {
            token.Result = ScanString(it);
            if (Match(token.Result)) {
                token.Type = TokenType::String;
            }
        }
        // asm string
        else if (c == '"' && mode == Mode::Assembler) {
            auto isstr = ScanDQString(it);
            if (Match(isstr)) {
                token.Type = TokenType::String;
            }
        }
        //
        else if (c == '$')
        {
            if (ScanHexadecimal(it)) {
                token.Type = TokenType::Number;
            }
        }
        // only number could start with digits, try to scan number
        else if (c >= '0' && c <= '9')
        {
            // it is at least some integer number token
            token.Type = TokenType::Number;

            ScanDecimal(it);

            if (ScanReal(it)) {
                token.Type = TokenType::RealNumber;
            }

        }

        // if none of previous checks detected any kind of token
        // this is symbol or invalid character token, check for it here
        // try to match compounds first, and single characters next
        if (token.Type == TokenType::None) {
            bool validsymbol =
                CheckAny(A(p_compounds), it) ||
                CheckAny(C(".();,=[]:<>+-*/^"), it);

            if (validsymbol) {
                token.Type = TokenType::Symbol;
            } else {
                // all other stuff (unknown/invalid symbols)
                GetCharToken(false, nullptr, it);
                token.Type = TokenType::Invalid;
            }
        }

        token.SourceToken = Scanner::ReadToken(it);

        return token;
    }

    template <typename Tsource>
    bool PascalScanner<Tsource>::ScanIdent(k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromSetWhile(p_alpha, p_alphanum, nullptr, it);
        return Match(result);
    }

    template <typename Tsource>
    bool PascalScanner<Tsource>::ScanComment(Token &token, ScannerSourceIterator &it) const
    {
        token.Result = FromTo(C("{"), C("}"), true, nullptr, it);
        if (Match(token.Result)) {
            token.Type = TokenType::Comment;
            return true;
        }

        token.Result = FromTo(C("(*"), C("*)"), true, nullptr, it);
        if (Match(token.Result)) {
            token.Type = TokenType::Comment;
            return true;
        }

        if (Match(FromToEndOfLine(C("//"), it))) {
            token.Type = TokenType::Comment;
            token.Result = ScanResult::Match;
            return true;
        }

        return false;
    }

    template <typename Tsource>
    bool PascalScanner<Tsource>::ScanDirective(Token &token, ScannerSourceIterator &it) const
    {
        // TODO: incremental
        auto result =
            Match(FromTo(C("{$"), C("}"), false, nullptr, it)) ||
            Match(FromTo(C("(*$"), C("*)"), false, nullptr, it));
        if (result) {
            token.Type = TokenType::Directive;
        }
        return result;
    }

    template <typename Tsource>
    k_parser::ScanResult PascalScanner<Tsource>::ScanString(ScannerSourceIterator &it) const
    {
        auto start = it;
        bool a, b;

        do {
            if (a = Check('#', it) != 0) {
                ScanHexadecimal(it) || ScanDecimal(it);
            }

            b = Match(FromTo(C("'"), C("'"), false, nullptr, it));
        } while (a || b);

        // TODO: refine result
        return Match(it - start) ? ScanResult::Match : ScanResult::NoMatch;
    }

    template <typename Tsource>
    k_parser::ScanResult PascalScanner<Tsource>::ScanDQString(ScannerSourceIterator &it) const
    {
        return FromTo(C("\""), C("\""), false, nullptr, it);
    }

    template <typename Tsource>
    bool PascalScanner<Tsource>::ScanHexadecimal(ScannerSourceIterator &it) const
    {
        auto result = FromTokenWhile(C("$"), p_hexadecimal, nullptr, false, it);
        return Match(result);
    }

    template <typename Tsource>
    bool PascalScanner<Tsource>::ScanDecimal(ScannerSourceIterator &it) const
    {
        auto result = FromSetWhile(p_numeric, p_numeric, nullptr, it);
        return Match(result);
    }

    template <typename Tsource>
    bool PascalScanner<Tsource>::ScanReal(ScannerSourceIterator &it) const
    {
        auto result = FromTokenWhile(C("."), p_numeric, nullptr, true, it);

        if (Match(result)) {
            // optional E/e part
            if (CheckAny(C("eE"), it) != 0) {
                // optional +/- after exponent sign
                CheckAny(C("+-"), it);
                // exponent digits
                ScanDecimal(it);
            }
        }

        return Match(result);
    }

} // namespace k_pascalparser
