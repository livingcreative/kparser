/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017

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
            Unknown,      // token haven't been scanned yet
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
                Type(TokenType::Unknown)
            {}

            Token(TokenType _type, const k_parser::SourceToken &_token) :
                Type(_type),
                SourceToken(_token)
            {}

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

        bool ReadToken(bool includespacers, Token &token);
        bool ReadToken(bool includespacers, k_parser::IncrementalScanData &data, Token &token);

    private:
        Token ScanToken(k_parser::IncrementalScanData &data);

        int IsEscape();
        int IsLineBreakMerge();

        bool ScanIdent(k_parser::SourceToken &token);
        bool ScanComment(k_parser::IncrementalScanData &data, k_parser::SourceToken &token);

        template <typename Tinner>
        ScanResult ScanString(Tinner inner, k_parser::SourceToken &token);

        bool ScanCharacter(k_parser::SourceToken &token);
        bool ScanIntegerPostfix();
        bool ScanRealPostfix();
        bool ScanHexadecimal(k_parser::SourceToken &token);
        bool ScanDecimal(k_parser::SourceToken &token);
        bool ScanReal(k_parser::SourceToken &token);
        bool ScanPreprocessor(k_parser::IncrementalScanData &data, k_parser::SourceToken &token);

    private:
        static const CharRange p_all[1];         // all characters set
        static const CharRange p_numeric[1];     // numeric [0 - 9] characters set
        static const CharRange p_hexadecimal[3]; // hexadecimal [0 - 9, A - F, a - f] characters set
        static const CharRange p_alpha[4];       // alpha characters set (not exact, unicode range needs refinement)
        static const CharRange p_alphanum[5];    // alpha + numeric characters set

        static const token_t p_hexprefixes[2];   // hexadecimal prefixes
        static const token_t p_escapes[9];       // all predefined escape sequences
        static const token_t p_compounds[24];    // all compound sequences
        static const token_t p_linemerge[4];     // all compound sequences which forms skip over line breaks

        static const token_t p_keywords[75];     // all c++ keywords
    };

    template <typename Tsource>
    const typename CPPScanner<Tsource>::CharRange CPPScanner<Tsource>::p_all[] = {
        { L'\x0001', L'\xFFFF' }
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
    const typename CPPScanner<Tsource>::token_t CPPScanner<Tsource>::p_hexprefixes[] = {
        L"0x", L"0X"
    };

    template <typename Tsource>
    const typename CPPScanner<Tsource>::token_t CPPScanner<Tsource>::p_escapes[] = {
        L"\\'",  L"\\\"", L"\\\\", L"\\t", L"\\r", L"\\n", L"\\b", L"\\f", L"\\0"
    };

    template <typename Tsource>
    const typename CPPScanner<Tsource>::token_t CPPScanner<Tsource>::p_compounds[] = {
        L"<<=", L">>=", L"->*", L"...", L"==", L"!=", L"=>", L"&&", L"::", L"++",
        L"--", L"||", L">=", L"<=", L"+=", L"-=", L"/=", L"*=", L"%=", L"&=",
        L"|=", L"^=", L"->", L".*"
    };

    template <typename Tsource>
    const typename CPPScanner<Tsource>::token_t CPPScanner<Tsource>::p_linemerge[] = {
        L"\\\n\r", L"\\\r\n", L"\\\n", L"\\"
    };

    template <typename Tsource>
    const typename CPPScanner<Tsource>::token_t CPPScanner<Tsource>::p_keywords[] = {
        L"alignas", L"alignof", L"asm", L"auto", L"bool", L"break", L"case", L"catch",
        L"char", L"char16_t", L"char32_t", L"class", L"const", L"constexpr", L"const_cast",
        L"continue", L"decltype", L"default", L"delete", L"do", L"double", L"dynamic_cast",
        L"else", L"enum", L"explicit", L"export", L"extern", L"false", L"final", L"float",
        L"for", L"friend", L"goto", L"if", L"inline", L"int", L"long", L"mutable",
        L"namespace", L"new", L"noexcept", L"nullptr", L"operator", L"override", L"private",
        L"protected", L"public", L"register", L"reinterpret_cast", L"return", L"short",
        L"signed", L"sizeof", L"static", L"static_assert", L"static_cast", L"struct",
        L"switch", L"template", L"this", L"thread_local", L"throw", L"true", L"try",
        L"typedef", L"typedid", L"typename", L"union", L"unsigned", L"using", L"virtual",
        L"void", L"volatile", L"wchar_t", L"while"
    };

    template <typename Tsource>
    CPPScanner<Tsource>::CPPScanner(Tsource &source) :
        Scanner(source)
    {}

    template <typename Tsource>
    bool CPPScanner<Tsource>::ReadToken(bool includespacers, Token &token)
    {
        k_parser::SourceToken stok;
        bool result = SkipToToken(stok);
        if (result) {
            if (includespacers && stok.Length > 0) {
                token.Type = TokenType::Spacer;
                token.SourceToken = stok;
            } else {
                IncrementalScanData data;
                token = ScanToken(data);
            }
        }
        return result;
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ReadToken(bool includespacers, k_parser::IncrementalScanData &data, Token &token)
    {
        k_parser::SourceToken stok;
        bool result = SkipToToken(stok);
        if (result) {
            if (includespacers && stok.Length > 0) {
                token.Type = TokenType::Spacer;
                token.SourceToken = stok;
            } else {
                token = ScanToken(data);
            }
        } else {
            switch (data.Current) {
                case IncrementalCurrentType::Preprocessor:
                    // empty line encountered while being inside preprocessor token,
                    // finish token
                    if (stok.Position == 0) {
                        data.Current = int(IncrementalCurrentType::None);
                    }
                    break;
            }
        }
        return result;
    }

    template <typename Tsource>
    typename CPPScanner<Tsource>::Token CPPScanner<Tsource>::ScanToken(k_parser::IncrementalScanData &data)
    {
        Token token;
        k_parser::SourceToken stok;

        if (data.Current != int(IncrementalCurrentType::None)) {
            switch (data.Current) {
                case IncrementalCurrentType::MultilineComment: {
                    token.Type = TokenType::Comment;

                    int level = 1;
                    auto result = ContinueTo(L"", L"*/", true, nullptr, false, stok, level);

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

                    int level = 1;
                    auto result = ContinueWhile(
                        p_all , false,
                        [=]() { return IsLineBreakMerge(); },
                        stok
                    );

                    if (!Match(result) || EndsWith(stok, p_linemerge) == NO_MATCH) {
                        data.Current = int(IncrementalCurrentType::None);
                    }

                    break;
                }
            }

            token.SourceToken = stok;

            return token;
        }

        token.Type = TokenType::Unknown;
        auto c = CharCurrent();

        // identifier starts with following characters, so it's most
        // high probability to try scan identifier first
        if (c == '_' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' ||
            c >= L'\x0100' || c == '\\')
        {
            // try to scan identifier
            if (ScanIdent(stok)) {
                token.Type = TokenType::Identifier;

                for (size_t n = 0; n < sizeof(p_keywords) / sizeof(p_keywords[0]); ++n) {
                    auto t = SourceTokenToToken(stok);
                    auto &kw = p_keywords[n];
                    if (t.Length == kw.Length && memcmp(t.Text, kw.Text, kw.Length * sizeof(char_t)) == 0) {
                        token.Type = TokenType::Keyword;
                        break;
                    }
                }
            }
        }
        // next most frequent token type is comment, comments start with /
        // character, so try scan a comment when / encountered
        else if (c == '/')
        {
            if (ScanComment(data, stok)) {
                token.Type = TokenType::Comment;
            }
        }
        // from this character string literal can start
        // try scan string
        else if (c == '"')
        {
            auto isstr = ScanString([this]() { return IsEscape(); }, stok);
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
            if (!ScanHexadecimal(stok)) {
                // it's not hexadecimal number - it's integer or real
                ScanDecimal(stok);

                // try scan integer postfix, if there's postfix it's integer
                // number
                if (ScanIntegerPostfix()) {
                    ++stok.Length;
                } else if (ScanRealPostfix()) {
                    ++stok.Length;
                    token.Type = TokenType::RealNumber;
                } else {
                    // try to scan "fractional" part of a number
                    SourceToken tok;
                    if (ScanReal(tok)) {
                        stok.Length += tok.Length;
                        token.Type = TokenType::RealNumber;
                    }
                }
            }
        }
        // from . character real number can start, or it's a single dot
        else if (c == '.')
        {
            if (ScanReal(stok)) {
                token.Type = TokenType::RealNumber;
            }
        }
        // from ' character only character literal can start
        else if (c == '\'')
        {
            ScanCharacter(stok);
            token.Type = TokenType::Character;
        }
        // only preprocessor directive can start with # character
        else if (c == '#')
        {
            ScanPreprocessor(data, stok);
            token.Type = TokenType::Preprocessor;
        }

        // if none of previous checks detected any kind of token
        // this is symbol or invalid character token, check for it here
        // try to match compounds first, and single characters next
        if (token.Type == TokenType::Unknown) {
            bool validsymbol =
                CheckAny(p_compounds, stok) ||
                CheckAny(L".();,{}=[]:<>+-*/?%&|^!~", stok);

            if (validsymbol) {
                token.Type = TokenType::Symbol;
            } else {
                // all other stuff (unknown/invalid symbols)
                GetCharToken(false, nullptr, stok);
                token.Type = TokenType::Invalid;
            }
        }

        token.SourceToken = stok;

        return token;
    }

    template <typename Tsource>
    int CPPScanner<Tsource>::IsEscape()
    {
        k_parser::SourceToken token;

        auto unicodeescape = FromTokenWhile(
            L"\\u", p_hexadecimal, false, nullptr,
            false, token, false
        );

        if (Match(unicodeescape)) {
            return token.Length;
        }

        int length;
        if (CheckAny(p_escapes, length, false) != NO_MATCH) {
            return length;
        }

        unicodeescape = FromTokenWhile(
            L"\\x", p_hexadecimal, false, nullptr,
            false, token, false
        );

        if (Match(unicodeescape)) {
            return token.Length;
        }

        return 0;
    }

    template <typename Tsource>
    int CPPScanner<Tsource>::IsLineBreakMerge()
    {
        int length;
        if (CheckAny(p_linemerge, length, false) != NO_MATCH) {
            return length;
        }

        return 0;
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanIdent(k_parser::SourceToken &token)
    {
        auto result = FromSetWhile(p_alpha, p_alphanum, false, nullptr, token);
        return Match(result);
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanComment(k_parser::IncrementalScanData &data, k_parser::SourceToken &token)
    {
        auto result = FromTo(L"/*", L"*/", true, nullptr, false, token);
        if (Match(result)) {
            if (result == ScanResult::MatchTrimmedEOF) {
                data.Current = int(IncrementalCurrentType::MultilineComment);
            }
            return true;
        }

        result = FromTokenWhile(L"//", p_all, false, nullptr, false, token);

        if (Match(result) && EndsWith(token, p_linemerge) != NO_MATCH) {
            data.Current = int(IncrementalCurrentType::SinglelineComment);
        }

        return Match(result);
    }

    template <typename Tsource>
    template <typename Tinner>
    typename CPPScanner<Tsource>::ScanResult CPPScanner<Tsource>::ScanString(Tinner inner, k_parser::SourceToken &token)
    {
        return FromTo(L"\"", L"\"", false, inner, false, token);
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanCharacter(k_parser::SourceToken &token)
    {
        auto result = FromTo(
            L"'", L"'", false, [this]() { return IsEscape(); },
            false, token
        );

        return Match(result);
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanIntegerPostfix()
    {
        return CheckAny(L"lLuU") != NO_MATCH;
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanRealPostfix()
    {
        return CheckAny(L"fFdD") != NO_MATCH;
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanHexadecimal(k_parser::SourceToken &token)
    {
        auto result = Match(FromTokenWhile(
            p_hexprefixes, p_hexadecimal, false, nullptr,
            false, token
        ));

        if (result && ScanIntegerPostfix()) {
            ++token.Length;
        }

        return result;
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanDecimal(k_parser::SourceToken &token)
    {
        auto result = FromSetWhile(p_numeric, p_numeric, false, nullptr, token);
        return Match(result);
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanReal(k_parser::SourceToken &token)
    {
        auto result = FromTokenWhile(L".", p_numeric, false, nullptr, true, token);

        if (Match(result)) {
            // optional E/e part
            if (CheckAny(L"eE") != NO_MATCH) {
                ++token.Length;

                // optional +/- after exponent sign
                if (CheckAny(L"+-") != NO_MATCH) {
                    ++token.Length;
                }

                // exponent digits
                SourceToken exp;
                if (ScanDecimal(exp)) {
                    token.Length += exp.Length;
                }
            }

            // optional postfix
            if (ScanRealPostfix()) {
                ++token.Length;
            }
        }

        return Match(result);
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanPreprocessor(k_parser::IncrementalScanData &data, k_parser::SourceToken &token)
    {
        auto result = Match(FromTokenWhile(
            L"#", p_all, false, [=]() { return IsLineBreakMerge(); },
            false, token
        ));

        if (result && EndsWith(token, p_linemerge) != NO_MATCH) {
            data.Current = int(IncrementalCurrentType::Preprocessor);
        }

        return result;
    }

} // namespace k_cppparser
