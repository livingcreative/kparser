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

namespace k_pascalparser
{

    template <typename Tsource>
    class PascalScanner : public k_parser::Scanner<Tsource>
    {
    public:
        PascalScanner(Tsource &source);

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

        struct Token
        {
            Token() :
                Type(TokenType::None)
            {}

            Token(TokenType _type, const k_parser::SourceToken &_token) :
                Type(_type),
                SourceToken(_token)
            {}

            operator bool() const { return Type != TokenType::None; }

            TokenType             Type;
            k_parser::SourceToken SourceToken;
        };

        enum class IncrementalCurrentType
        {
            None,
            MultilineComment,
            MultilineOldStyleComment,
            MultilineDirective,
            MultilineOldStyleDirective
        };

        enum class Mode
        {
            Pascal,
            Assembler
        };

        Token ReadToken(bool includespacers);
        Token ReadToken(bool includespacers, k_parser::IncrementalScanData &data);

    private:
        Token ScanToken(Mode mode, k_parser::IncrementalScanData &data);

        bool ScanIdent(k_parser::ScannerSourceIterator &it) const;
        bool ScanComment(k_parser::IncrementalScanData &data, k_parser::ScannerSourceIterator &it) const;
        bool ScanDirective(k_parser::IncrementalScanData &data, k_parser::ScannerSourceIterator &it) const;
        typename PascalScanner<Tsource>::ScanResult ScanString(k_parser::ScannerSourceIterator &it) const;
        typename PascalScanner<Tsource>::ScanResult ScanDQString(k_parser::ScannerSourceIterator &it) const;
        bool ScanHexadecimal(k_parser::ScannerSourceIterator &it) const;
        bool ScanDecimal(k_parser::ScannerSourceIterator &it) const;
        bool ScanReal(k_parser::ScannerSourceIterator &it) const;

    private:
        typedef k_parser::Token<char> TokenChar;
        typedef k_parser::CharRange CharRange;

        static const CharRange p_numeric[1];     // numeric [0 - 9] characters set
        static const CharRange p_hexadecimal[3]; // hexadecimal [0 - 9, A - F, a - f] characters set
        static const CharRange p_alpha[4];       // alpha characters set (not exact, unicode range needs refinement)
        static const CharRange p_alphanum[5];    // alpha + numeric characters set
        static const CharRange p_asmalpha[5];    // inline assembler alpha characters set (not exact, unicode range needs refinement)
        static const CharRange p_asmalphanum[6]; // inline assembler alpha + numeric characters set

        static const TokenChar p_compounds[7];   // all compound sequences

        static const TokenChar p_keywords[71];   // all Pascal keywords
    };


    template <typename Tsource>
    const typename PascalScanner<Tsource>::CharRange PascalScanner<Tsource>::p_numeric[] = {
        { '0', '9' }
    };

    template <typename Tsource>
    const typename PascalScanner<Tsource>::CharRange PascalScanner<Tsource>::p_hexadecimal[] = {
        { '0', '9' },
        { 'A', 'F' },
        { 'a', 'f' }
    };

    template <typename Tsource>
    const typename PascalScanner<Tsource>::CharRange PascalScanner<Tsource>::p_alpha[] = {
        { '_', '_' },
        { 'A', 'Z' },
        { 'a', 'z' },
        // TODO: refine alpha range
        { L'\x0100', L'\xFFFF' }
    };

    template <typename Tsource>
    const typename PascalScanner<Tsource>::CharRange PascalScanner<Tsource>::p_alphanum[] = {
        { '_', '_' },
        { 'A', 'Z' },
        { 'a', 'z' },
        { L'\x0100', L'\xFFFF' },
        { '0', '9' }
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
    typename PascalScanner<Tsource>::Token PascalScanner<Tsource>::ReadToken(bool includespacers)
    {
        IncrementalScanData data;
        return ReadToken(includespacers, data);
    }

    template <typename Tsource>
    typename PascalScanner<Tsource>::Token PascalScanner<Tsource>::ReadToken(bool includespacers, k_parser::IncrementalScanData &data)
    {
        auto it = It();

        if (SkipToToken(it)) {
            if (includespacers && PeekToken(it)) {
                return Token(TokenType::Spacer, Scanner::ReadToken(it));
            } else {
                DiscardToken(it);
                return ScanToken(Mode::Pascal, data);
            }
        }

        DiscardToken(it);
        return Token();
    }

    template <typename Tsource>
    typename PascalScanner<Tsource>::Token PascalScanner<Tsource>::ScanToken(Mode mode, k_parser::IncrementalScanData &data)
    {
        Token token;
        auto it = It();

        if (data.Current != int(IncrementalCurrentType::None)) {
            switch (data.Current) {
                case IncrementalCurrentType::MultilineComment:
                case IncrementalCurrentType::MultilineOldStyleComment:
                case IncrementalCurrentType::MultilineDirective:
                case IncrementalCurrentType::MultilineOldStyleDirective: {
                    token.Type =
                        data.Current == int(IncrementalCurrentType::MultilineComment) ||
                        data.Current == int(IncrementalCurrentType::MultilineOldStyleComment) ?
                        TokenType::Comment : TokenType::Directive;

                    auto endseq =
                        data.Current == int(IncrementalCurrentType::MultilineComment) ||
                        data.Current == int(IncrementalCurrentType::MultilineDirective) ?
                        C("}") : C("*)");

                    auto result = ContinueTo(C(""), endseq, true, nullptr, it);

                    if (result != ScanResult::MatchTrimmedEOF) {
                        data.Current = int(IncrementalCurrentType::None);
                    }

                    break;
                }
            }

            token.SourceToken = Scanner::ReadToken(it);

            return token;
        }

        token.Type = TokenType::None;
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
            if (ScanComment(data, it)) {
                token.Type = TokenType::Comment;
            } else if (ScanDirective(data, it)) {
                token.Type = TokenType::Directive;
            } else {
                CheckAny(C("/{("), it);
                token.Type = TokenType::Symbol;
            }
        }
        // from this character string literal can start
        // try scan string
        else if (c == '\'' || c == '#')
        {
            auto isstr = ScanString(it);
            if (Match(isstr)) {
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
    bool PascalScanner<Tsource>::ScanComment(k_parser::IncrementalScanData &data, k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromTo(C("{"), C("}"), true, nullptr, it);
        if (Match(result)) {
            if (result == ScanResult::MatchTrimmedEOF) {
                data.Current = int(IncrementalCurrentType::MultilineComment);
            }
            return true;
        }

        result = FromTo(C("(*"), C("*)"), true, nullptr, it);
        if (Match(result)) {
            if (result == ScanResult::MatchTrimmedEOF) {
                data.Current = int(IncrementalCurrentType::MultilineOldStyleComment);
            }
            return true;
        }

        return Match(FromToEndOfLine(C("//"), it));
    }

    template <typename Tsource>
    bool PascalScanner<Tsource>::ScanDirective(k_parser::IncrementalScanData &data, k_parser::ScannerSourceIterator &it) const
    {
        // TODO: incremental
        auto result =
            Match(FromTo(C("{$"), C("}"), false, nullptr, it)) ||
            Match(FromTo(C("(*$"), C("*)"), false, nullptr, it));
        return result;
    }

    template <typename Tsource>
    typename PascalScanner<Tsource>::ScanResult
    PascalScanner<Tsource>::ScanString(k_parser::ScannerSourceIterator &it) const
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
    typename PascalScanner<Tsource>::ScanResult
    PascalScanner<Tsource>::ScanDQString(k_parser::ScannerSourceIterator &it) const
    {
        return FromTo(C("\""), C("\""), false, nullptr, it);
    }

    template <typename Tsource>
    bool PascalScanner<Tsource>::ScanHexadecimal(k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromTokenWhile(C("$"), p_hexadecimal, nullptr, false, it);
        return Match(result);
    }

    template <typename Tsource>
    bool PascalScanner<Tsource>::ScanDecimal(k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromSetWhile(p_numeric, p_numeric, nullptr, it);
        return Match(result);
    }

    template <typename Tsource>
    bool PascalScanner<Tsource>::ScanReal(k_parser::ScannerSourceIterator &it) const
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
