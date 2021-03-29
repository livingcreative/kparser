/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017 - 2021

    https://github.com/livingcreative/kparser

    kcppscanner.h
        Example c# scanner implementation, general example on using
        Scanner class
*/

#pragma once
#include "kparser/kscanner.h"

namespace k_cppparser
{

    template <typename Tsource>
    class CPPScanner : public k_parser::Scanner<Tsource>
    {
    public:
        CPPScanner(Tsource &source);

        enum class TokenType
        {
            None,         // token haven't been scanned yet
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
            MultilineComment,
            SinglelineComment,
            Preprocessor
        };

        Token ReadToken(bool includespacers);
        Token ReadToken(bool includespacers, k_parser::IncrementalScanData &data);

    private:
        Token ScanToken(k_parser::IncrementalScanData &data);

        k_parser::SourceLength IsEscape(const k_parser::ScannerSourceIterator &it) const;

        bool ScanIdent(k_parser::ScannerSourceIterator &it) const;
        bool ScanComment(k_parser::IncrementalScanData &data, k_parser::ScannerSourceIterator &it) const;

        typename k_parser::Scanner<Tsource>::ScanResult ScanString(k_parser::ScannerSourceIterator &it) const;

        bool ScanCharacter(k_parser::ScannerSourceIterator &it) const;
        bool ScanIntegerPostfix(k_parser::ScannerSourceIterator &it) const;
        bool ScanRealPostfix(k_parser::ScannerSourceIterator &it) const;
        bool ScanHexadecimal(k_parser::ScannerSourceIterator &it) const;
        bool ScanDecimal(k_parser::ScannerSourceIterator &it) const;
        bool ScanReal(k_parser::ScannerSourceIterator &it) const;
        bool ScanPreprocessor(k_parser::IncrementalScanData &data, k_parser::ScannerSourceIterator &it) const;

    private:
        typedef k_parser::Token<char> TokenChar;
        typedef typename k_parser::CharRange CharRange;

        static const CharRange p_numeric[1];     // numeric [0 - 9] characters set
        static const CharRange p_hexadecimal[3]; // hexadecimal [0 - 9, A - F, a - f] characters set
        static const CharRange p_alpha[4];       // alpha characters set (not exact, unicode range needs refinement)
        static const CharRange p_alphanum[5];    // alpha + numeric characters set

        static const TokenChar p_hexprefixes[2]; // hexadecimal prefixes
        static const TokenChar p_escapes[9];     // all predefined escape sequences
        static const TokenChar p_compounds[24];  // all compound sequences

        static const TokenChar p_keywords[75];   // all c++ keywords
    };


    template <typename Tsource>
    const typename CPPScanner<Tsource>::CharRange CPPScanner<Tsource>::p_numeric[] = {
        { '0', '9' }
    };

    template <typename Tsource>
    const typename CPPScanner<Tsource>::CharRange CPPScanner<Tsource>::p_hexadecimal[] = {
        { '0', '9' },
        { 'A', 'F' },
        { 'a', 'f' }
    };

    template <typename Tsource>
    const typename CPPScanner<Tsource>::CharRange CPPScanner<Tsource>::p_alpha[] = {
        { '_', '_' },
        { 'A', 'Z' },
        { 'a', 'z' },
        // TODO: refine alpha range
        { L'\x0100', L'\xFFFF' }
    };

    template <typename Tsource>
    const typename CPPScanner<Tsource>::CharRange CPPScanner<Tsource>::p_alphanum[] = {
        { '_', '_' },
        { 'A', 'Z' },
        { 'a', 'z' },
        { L'\x0100', L'\xFFFF' },
        { '0', '9' }
    };

    template <typename Tsource>
    const typename CPPScanner<Tsource>::TokenChar CPPScanner<Tsource>::p_hexprefixes[] = {
        "0x", "0X"
    };

    template <typename Tsource>
    const typename CPPScanner<Tsource>::TokenChar CPPScanner<Tsource>::p_escapes[] = {
        "\\'",  "\\\"", "\\\\", "\\t", "\\r", "\\n", "\\b", "\\f", "\\0"
    };

    template <typename Tsource>
    const typename CPPScanner<Tsource>::TokenChar CPPScanner<Tsource>::p_compounds[] = {
        "<<=", ">>=", "->*", "...", "==", "!=", "=>", "&&", "::", "++",
        "--", "||", ">=", "<=", "+=", "-=", "/=", "*=", "%=", "&=",
        "|=", "^=", "->", ".*"
    };

    template <typename Tsource>
    const typename CPPScanner<Tsource>::TokenChar CPPScanner<Tsource>::p_keywords[] = {
        "alignas", "alignof", "asm", "auto", "bool", "break", "case", "catch",
        "char", "char16_t", "char32_t", "class", "const", "constexpr", "const_cast",
        "continue", "decltype", "default", "delete", "do", "double", "dynamic_cast",
        "else", "enum", "explicit", "export", "extern", "false", "final", "float",
        "for", "friend", "goto", "if", "inline", "int", "long", "mutable",
        "namespace", "new", "noexcept", "nullptr", "operator", "override", "private",
        "protected", "public", "register", "reinterpret_cast", "return", "short",
        "signed", "sizeof", "static", "static_assert", "static_cast", "struct",
        "switch", "template", "this", "thread_local", "throw", "true", "try",
        "typedef", "typedid", "typename", "union", "unsigned", "using", "virtual",
        "void", "volatile", "wchar_t", "while"
    };

    template <typename Tsource>
    CPPScanner<Tsource>::CPPScanner(Tsource &source) :
        Scanner(source)
    {}

    template <typename Tsource>
    typename CPPScanner<Tsource>::Token CPPScanner<Tsource>::ReadToken(bool includespacers)
    {
        IncrementalScanData data;
        return ReadToken(includespacers, data);
    }

    template <typename Tsource>
    typename CPPScanner<Tsource>::Token CPPScanner<Tsource>::ReadToken(bool includespacers, k_parser::IncrementalScanData &data)
    {
        auto it = It();

        if (SkipToToken(it)) {
            if (includespacers && PeekToken(it)) {
                return Token(TokenType::Spacer, Scanner::ReadToken(it));
            } else {
                DiscardToken(it);
                return ScanToken(data);
            }
        } else {
            switch (data.Current) {
                case IncrementalCurrentType::Preprocessor:
                    // empty line encountered while being inside preprocessor token,
                    // finish token
                    if (PeekToken(it).Position == 0) {
                        data.Current = int(IncrementalCurrentType::None);
                    }
                    break;
            }
            DiscardToken(it);
            return Token();
        }
    }

    template <typename Tsource>
    typename CPPScanner<Tsource>::Token CPPScanner<Tsource>::ScanToken(k_parser::IncrementalScanData &data)
    {
        Token token;
        auto it = It();

        if (data.Current != int(IncrementalCurrentType::None)) {
            switch (data.Current) {
                case IncrementalCurrentType::MultilineComment: {
                    token.Type = TokenType::Comment;

                    auto result = ContinueTo(C(), C("*/"), true, nullptr, it);

                    if (result != ScanResult::MatchTrimmedEOF) {
                        data.Current = int(IncrementalCurrentType::None);
                    }

                    break;
                }

                case IncrementalCurrentType::SinglelineComment:
                case IncrementalCurrentType::Preprocessor: {
                    token.Type =
                        data.Current == int(IncrementalCurrentType::SinglelineComment) ?
                        TokenType::Comment : TokenType::Preprocessor;

                    auto result = ContinueToEndOfLine(it);

                    // if sequence does not end with line merge - terminate incremental sequence
                    if (NoMatch(result) || !EndsWith(PeekToken(it), C("\\"))) {
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
            auto isstr = ScanString(it);
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
        // only preprocessor directive can start with # character
        else if (c == '#')
        {
            ScanPreprocessor(data, it);
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
    k_parser::SourceLength CPPScanner<Tsource>::IsEscape(const k_parser::ScannerSourceIterator &it) const
    {
        auto current = it;

        auto unicodeescape = FromTokenWhile(C("\\u"), p_hexadecimal, nullptr, false, current);

        if (Match(unicodeescape)) {
            return current - it;
        }

        if (auto len = CheckAny(A(p_escapes), current)) {
            return len;
        }

        unicodeescape = FromTokenWhile(C("\\x"), p_hexadecimal, nullptr, false, current);

        if (Match(unicodeescape)) {
            return current - it;
        }

        return 0;
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanIdent(k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromSetWhile(p_alpha, p_alphanum, nullptr, it);
        return Match(result);
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanComment(k_parser::IncrementalScanData &data, k_parser::ScannerSourceIterator &it) const
    {
        auto scanresult = FromTo(C("/*"), C("*/"), true, nullptr, it);
        if (Match(scanresult)) {
            if (scanresult == ScanResult::MatchTrimmedEOF) {
                data.Current = int(IncrementalCurrentType::MultilineComment);
            }
            return true;
        }

        auto start = it;
        auto result = FromToEndOfLine(C("//"), it);

        auto tailresult = result;
        while (Match(tailresult) && EndsWith(PeekToken(start, it), C("\\"))) {
            data.Current = int(IncrementalCurrentType::SinglelineComment);

            // in regular scan there will be line break at the end of the last scanned line,
            // it must be skipped
            SkipLineBreak(it);

            start = it;
            tailresult = ContinueToEndOfLine(it);
        }

        return Match(result);
    }

    template <typename Tsource>
    typename k_parser::Scanner<Tsource>::ScanResult
    CPPScanner<Tsource>::ScanString(k_parser::ScannerSourceIterator &it) const
    {
        return FromTo(C("\""), C("\""), false, [this](auto &it) { return IsEscape(it); }, it);
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanCharacter(k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromTo(C("'"), C("'"), false, [this](auto &it) { return IsEscape(it); }, it);
        return Match(result);
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanIntegerPostfix(k_parser::ScannerSourceIterator &it) const
    {
        auto start = it;

        while (CheckAny(C("lLuU"), it) != 0) {}

        return Match(it - start);
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanRealPostfix(k_parser::ScannerSourceIterator &it) const
    {
        return CheckAny(C("fFdD"), it) != 0;
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanHexadecimal(k_parser::ScannerSourceIterator &it) const
    {
        auto result = Match(FromTokenWhile(A(p_hexprefixes), p_hexadecimal, nullptr, false, it));

        if (result) {
            ScanIntegerPostfix(it);
        }

        return result;
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanDecimal(k_parser::ScannerSourceIterator &it) const
    {
        auto result = FromSetWhile(p_numeric, p_numeric, nullptr, it);
        return Match(result);
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanReal(k_parser::ScannerSourceIterator &it) const
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
    bool CPPScanner<Tsource>::ScanPreprocessor(k_parser::IncrementalScanData &data, k_parser::ScannerSourceIterator &it) const
    {
        auto start = it;
        auto result = Match(FromToEndOfLine(C("#"), it));

        auto tailresult = result;
        while (tailresult && EndsWith(PeekToken(start, it), C("\\"))) {
            data.Current = int(IncrementalCurrentType::Preprocessor);

            // in regular scan there will be line break at the end of the last scanned line,
            // it must be skipped
            SkipLineBreak(it);

            start = it;
            tailresult = Match(ContinueToEndOfLine(it));
        }

        return result;
    }

} // namespace k_cppparser
