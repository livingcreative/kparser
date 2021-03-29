/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017 - 2021

    https://github.com/livingcreative/kparser

    kcsscanner.h
        Example c# scanner implementation, general example on using
        Scanner class
*/

#pragma once
#include "kparser/kscanner.h"

namespace k_csparser
{

    template <typename Tsource>
    class CSScanner : public k_parser::Scanner<Tsource>
    {
    public:
        CSScanner(Tsource &source);

        enum class TokenType
        {
            None,         // token haven't been scanned yet or scan finished
            Identifier,   // any valid identifier
            Keyword,      // keyword (special identifiers)
            Number,       // any integer number, decimal or hexadecimal (might be incomplete)
            RealNumber,   // any real (float or double) number (might be incomplete)
            Character,    // character (single quoted literal, might be malformed)
            String,       // any string (including $ and @ strings, might be incomplete or malformed)
            Comment,      // any comment (single- or multi-line)
            Symbol,       // any standalone character or compiund sequence
            Preprocessor, // preprocessor token (as a whole, not parsed, including possible comments inside)
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
            Comment,
            VerbatimString,
            VerbatimInterpolationString
        };

        Token ReadToken(bool includespacers);
        Token ReadToken(bool includespacers, k_parser::IncrementalScanData &data);

    private:
        Token ScanToken(k_parser::IncrementalScanData &data);

        enum EscapeCheckContext
        {
            eccIdentifier,
            eccCharacter
        };

        k_parser::SourceLength IsEscape(EscapeCheckContext context, const k_parser::ScannerSourceIterator &it) const;
        k_parser::SourceLength ScanInterpolationComment(bool multiline, const k_parser::ScannerSourceIterator &it) const;
        k_parser::SourceLength InterpolationInnerScan(bool checkcharescape, bool multiline, const k_parser::ScannerSourceIterator &it) const;

        bool ScanIdent(k_parser::ScannerSourceIterator &it) const;
        bool ScanComment(k_parser::IncrementalScanData &data, k_parser::ScannerSourceIterator &it) const;

        template <typename Tinner>
        typename k_parser::Scanner<Tsource>::ScanResult ScanString(Tinner inner, k_parser::ScannerSourceIterator &it) const;

        template <typename Tinner>
        typename k_parser::Scanner<Tsource>::ScanResult ScanVerbatimString(k_parser::IncrementalScanData &data, Tinner inner, k_parser::ScannerSourceIterator &it) const;

        bool ScanCharacter(k_parser::ScannerSourceIterator &it) const;
        bool ScanIntegerPostfix(k_parser::ScannerSourceIterator &it) const;
        bool ScanRealPostfix(k_parser::ScannerSourceIterator &it) const;
        bool ScanHexadecimal(k_parser::ScannerSourceIterator &it) const;
        bool ScanDecimal(k_parser::ScannerSourceIterator &it) const;
        bool ScanReal(k_parser::ScannerSourceIterator &it) const;
        bool ScanPreprocessor(k_parser::ScannerSourceIterator &it) const;

    private:
        typedef k_parser::Token<char> TokenChar;
        typedef typename k_parser::CharRange CharRange;

        static const CharRange p_numeric[1];     // numeric [0 - 9] characters set
        static const CharRange p_hexadecimal[3]; // hexadecimal [0 - 9, A - F, a - f] characters set
        static const CharRange p_alpha[4];       // alpha characters set (not exact, unicode range needs refinement)
        static const CharRange p_alphanum[5];    // alpha + numeric characters set

        static const TokenChar p_hexprefixes[2]; // hexadecimal prefixes
        static const TokenChar p_escapes[9];     // all predefined escape sequences
        static const TokenChar p_compounds[21];  // all compound sequences

        static const TokenChar p_keywords[75];
    };


    template <typename Tsource>
    const typename CSScanner<Tsource>::CharRange CSScanner<Tsource>::p_numeric[] = {
        { '0', '9' }
    };

    template <typename Tsource>
    const typename CSScanner<Tsource>::CharRange CSScanner<Tsource>::p_hexadecimal[] = {
        { '0', '9' },
        { 'A', 'F' },
        { 'a', 'f' }
    };

    template <typename Tsource>
    const typename CSScanner<Tsource>::CharRange CSScanner<Tsource>::p_alpha[] = {
        { '_', '_' },
        { 'A', 'Z' },
        { 'a', 'z' },
        // TODO: refine alpha range
        { L'\x0100', L'\xFFFF' }
    };

    template <typename Tsource>
    const typename CSScanner<Tsource>::CharRange CSScanner<Tsource>::p_alphanum[] = {
        { '_', '_' },
        { 'A', 'Z' },
        { 'a', 'z' },
        { L'\x0100', L'\xFFFF' },
        { '0', '9' }
    };

    template <typename Tsource>
    const typename CSScanner<Tsource>::TokenChar CSScanner<Tsource>::p_hexprefixes[] = {
        "0x", "0X"
    };

    template <typename Tsource>
    const typename CSScanner<Tsource>::TokenChar CSScanner<Tsource>::p_escapes[] = {
        "\\'",  "\\\"", "\\\\", "\\t", "\\r", "\\n", "\\b", "\\f", "\\0"
    };

    template <typename Tsource>
    const typename CSScanner<Tsource>::TokenChar CSScanner<Tsource>::p_compounds[] = {
        "<<=", ">>=",
        "==", "!=", "=>", "&&", "??", "++", "--", "||", ">=", "<=",
        "+=", "-=", "/=", "*=", "%=", "&=", "|=", "^=", "->"
    };

    template <typename Tsource>
    const typename CSScanner<Tsource>::TokenChar CSScanner<Tsource>::p_keywords[] = {
        "abstract", "as", "base", "bool", "break", "byte", "case", "catch",
        "char", "checked", "class", "const", "continue", "decimal", "default",
        "delegate", "do", "double", "else", "enum", "event", "extern", "false",
        "finally", "float", "for", "foreach", "get", "goto", "if", "int",
        "interface", "internal", "is", "lock", "long", "namespace", "new", "null",
        "object", "operator", "out", "override", "params", "partial", "private",
        "protected", "public", "readonly", "ref", "return", "sbyte", "set", "short",
        "sizeof", "static", "string", "struct", "switch", "this", "throw", "true",
        "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using",
        "var", "virtual", "void", "while", "yield"
    };


    template <typename Tsource>
    CSScanner<Tsource>::CSScanner(Tsource &source) :
        Scanner(source)
    {}

    template <typename Tsource>
    typename CSScanner<Tsource>::Token CSScanner<Tsource>::ReadToken(bool includespacers)
    {
        IncrementalScanData data;
        return ReadToken(includespacers, data);
    }

    template <typename Tsource>
    typename CSScanner<Tsource>::Token CSScanner<Tsource>::ReadToken(bool includespacers, k_parser::IncrementalScanData &data)
    {
        auto it = It();

        if (SkipToToken(it)) {
            if (includespacers && PeekToken(it)) {
                return Token(TokenType::Spacer, Scanner::ReadToken(it));
            } else {
                DiscardToken(it);
                return ScanToken(data);
            }
        }

        DiscardToken(it);
        return Token();
    }

    template <typename Tsource>
    typename CSScanner<Tsource>::Token CSScanner<Tsource>::ScanToken(k_parser::IncrementalScanData &data)
    {
        Token token;
        auto it = It();

        if (data.Current != int(IncrementalCurrentType::None)) {
            switch (data.Current) {
                case IncrementalCurrentType::Comment: {
                    token.Type = TokenType::Comment;

                    auto result = ContinueTo(C(), C("*/"), true, nullptr, it);

                    if (result != ScanResult::MatchTrimmedEOF) {
                        data.Current = int(IncrementalCurrentType::None);
                    }

                    break;
                }

                case IncrementalCurrentType::VerbatimString: {
                    token.Type = TokenType::String;

                    auto result = ContinueTo(C(), C("\""), true, nullptr, it);

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
            c >= L'\x0100' || c == '\\')
        {
            // try to scan identifier
            if (ScanIdent(it)) {
                token.Type = TokenType::Identifier;

                if (TokenCheckAny(PeekToken(it), A(p_keywords)) != NO_MATCH) {
                    token.Type = TokenType::Keyword;
                }
            }
        }
        // next most frequent token type is comment, comments start with /
        // character, so try scan a comment when / encountered
        else if (c == '/')
        {
            if (ScanComment(data, it)) {
                token.Type = TokenType::Comment;
            }
        }
        // from this character string literal can start
        // try scan string
        else if (c == '"')
        {
            auto isstr = ScanString([this](auto &it) { return IsEscape(eccCharacter, it); }, it);
            if (Match(isstr)) {
                token.Type = TokenType::String;
            }
        }
        // only number could start with digits, try to scan number
        else if (c >= '0' && c <= '9')
        {
            // it is at least some integer number token
            token.Type = TokenType::Number;

            // hexadecimal number literal can't have real part
            if (!ScanHexadecimal(it)) {
                // it's not hexadecimal number - it's integer or real
                ScanDecimal(it);

                // try scan integer postfix, if there's postfix it's integer
                // number
                if (!ScanIntegerPostfix(it)) {
                    if (ScanRealPostfix(it) || ScanReal(it)) {
                        token.Type = TokenType::RealNumber;
                    }
                }
            }
        }
        // from this character interpolated string literal can start
        // try scan interpolated string
        else if (c == '$')
        {
            // interpolated string could be usual double quoted string or
            // @ verbatim string, "eat" $ character and try to scan one of
            // string variants
            GetCharToken(nullptr, it);

            auto isstring =
                Match(ScanString([this](auto &it) { return InterpolationInnerScan(true, false, it); }, it)) ||
                Match(ScanVerbatimString(data, [this](auto &it) { return InterpolationInnerScan(false, true, it); }, it));

            token.Type = isstring ? TokenType::String : TokenType::Invalid;
        }
        // from . character real number can start, or it's a single dot
        else if (c == '.')
        {
            if (ScanReal(it)) {
                token.Type = TokenType::RealNumber;
            }
        }
        // from ' character only character literal can start
        else if (c == '\'')
        {
            ScanCharacter(it);
            token.Type = TokenType::Character;
        }
        // "verbatim" character can start string or @ident
        else if (c == '@')
        {
            if (Match(ScanVerbatimString(data, nullptr, it))) {
                token.Type = TokenType::String;
            } else {
                GetCharToken(nullptr, it);

                if (ScanIdent(it)) {
                    token.Type = TokenType::Identifier;
                } else {
                    token.Type = TokenType::Invalid;
                }
            }
        }
        // only preprocessor directive can start with # character
        else if (c == '#')
        {
            ScanPreprocessor(it);
            token.Type = TokenType::Preprocessor;
        }

        // if none of previous checks detected any kind of token
        // this is symbol or invalid character token, check for it here
        // try to match compounds first, and single characters next
        if (token.Type == TokenType::None) {
            bool validsymbol =
                CheckAny(A(p_compounds), it) ||
                CheckAny(C(".();,{}=[]:<>+-*/?%&|^!~"), it);

            if (validsymbol) {
                token.Type = TokenType::Symbol;
            } else {
                // all other stuff (unknown/invalid symbols)
                GetCharToken(nullptr, it);
                token.Type = TokenType::Invalid;
            }
        }

        token.SourceToken = Scanner::ReadToken(it);

        return token;
    }

    template <typename Tsource>
    k_parser::SourceLength CSScanner<Tsource>::IsEscape(EscapeCheckContext context, const k_parser::ScannerSourceIterator &it) const
    {
        auto current = it;

        auto unicodeescape = FromTokenWhile(C("\\u"), p_hexadecimal, nullptr, false, current);

        if (Match(unicodeescape)) {
            return current - it;
        }

        if (context == eccCharacter) {
            if (auto len = CheckAny(A(p_escapes), current)) {
                return len;
            }

            unicodeescape = FromTokenWhile(C("\\x"), p_hexadecimal, nullptr, false, current);

            if (Match(unicodeescape)) {
                return current - it;
            }
        }

        return 0;
    }

    template <typename Tsource>
    k_parser::SourceLength CSScanner<Tsource>::ScanInterpolationComment(bool multiline, const k_parser::ScannerSourceIterator &it) const
    {
        auto current = it;
        auto result = FromTo(C("/*"), C("*/"), multiline, nullptr, current);
        return Match(result) ? (current - it) : 0;
    }

    template <typename Tsource>
    k_parser::SourceLength CSScanner<Tsource>::InterpolationInnerScan(bool checkcharescape, bool multiline, const k_parser::ScannerSourceIterator &it) const
    {
        if (checkcharescape) {
            auto esc = IsEscape(eccCharacter, it);
            if (esc > 0) {
                return esc;
            }
        }

        auto current = it;
        auto result = FromToWithNesting(
            C("{"), C("}"), multiline,
            [this, multiline](auto &it) { return ScanInterpolationComment(multiline, it); },
            current
        );

        return Match(result) ? (current - it) : 0;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanIdent(k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromSetWhile(
            p_alpha, p_alphanum, [this](auto &it) { return IsEscape(eccIdentifier, it); },
            it
        );

        return Match(result);
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanComment(k_parser::IncrementalScanData &data, k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromToEndOfLine(C("//"), it);

        if (NoMatch(result)) {
            auto result = FromTo(C("/*"), C("*/"), true, nullptr, it);
            if (result == ScanResult::MatchTrimmedEOF) {
                data.Current = int(IncrementalCurrentType::Comment);
            }
            return Match(result);
        }

        return Match(result);
    }

    template <typename Tsource>
    template <typename Tinner>
    typename k_parser::Scanner<Tsource>::ScanResult CSScanner<Tsource>::ScanString(Tinner inner, k_parser::ScannerSourceIterator &it) const
    {
        return FromTo(C("\""), C("\""), false, inner, it);
    }

    template <typename Tsource>
    template <typename Tinner>
    typename k_parser::Scanner<Tsource>::ScanResult CSScanner<Tsource>::ScanVerbatimString(k_parser::IncrementalScanData &data, Tinner inner, k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromTo(C("@\""), C("\""), true, inner, it);

        switch (result) {
            // continue with contigous double quoted strings only if there was full match
            case ScanResult::Match: {
                ScanResult next;
                while (Match(next = FromTo(C("\""), C("\""), true, inner, it))) {
                    result = next;
                }
                break;
            }

            case ScanResult::MatchTrimmedEOF:
                data.Current = int(IncrementalCurrentType::VerbatimString);
                break;
        }

        return result;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanCharacter(k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromTo(
            C("'"), C("'"), false, [this](auto &it) { return IsEscape(eccCharacter, it); },
            it
        );

        return Match(result);
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanIntegerPostfix(k_parser::ScannerSourceIterator &it) const
    {
        return CheckAny(C("lLuU"), it) != 0;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanRealPostfix(k_parser::ScannerSourceIterator &it) const
    {
        return CheckAny(C("fFdDmM"), it) != 0;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanHexadecimal(k_parser::ScannerSourceIterator &it) const
    {
        auto result = Match(FromTokenWhile(A(p_hexprefixes), p_hexadecimal, nullptr, false, it));

        if (result) {
            ScanIntegerPostfix(it);
        }

        return result;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanDecimal(k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromSetWhile(p_numeric, p_numeric, nullptr, it);
        return Match(result);
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanReal(k_parser::ScannerSourceIterator &it) const
    {
        auto result = Match(FromTokenWhile(C("."), p_numeric, nullptr, true, it));

        if (result) {
            // optional E/e part
            if (CheckAny(C("eE"), it)) {
                // optional +/- after exponent sign
                CheckAny(C("+-"), it);
                // exponent digits
                ScanDecimal(it);
            }

            // optional postfix
            ScanRealPostfix(it);
        }

        return result;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanPreprocessor(k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromToEndOfLine(C("#"), it);
        return Match(result);
    }

} // namespace k_csparser
