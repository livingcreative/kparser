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
    class CPPScanner : public k_parser::Scanner<wchar_t, Tsource>
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
            Comment,
            Preprocessor
        };

        bool ReadToken(bool includespacers, Token &token);
        bool ReadToken(bool includespacers, k_parser::IncrementalScanData &data, Token &token);

    private:
        Token ScanToken(k_parser::IncrementalScanData &data);

        int IsEscape();
        int IsPreprocessorLineBreak();

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
        CharSet p_all;            // all characters set
        CharSet p_numeric;        // numeric [0 - 9] characters set
        CharSet p_hexadecimal;    // hexadecimal [0 - 9, A - F, a - f] characters set
        CharSet p_alpha;          // alpha characters set (not exact, unicode range needs refinement)
        CharSet p_alphanum;       // alpha + numeric characters set

        token_t p_hexprefixes[2]; // hexadecimal prefixes
        token_t p_escapes[9];     // all predefined escape sequences
        token_t p_compounds[24];  // all compound sequences
        token_t p_pplbrk[4];      // all compound sequences which forms preprocessor line breaks

        token_t p_keywords[75];   // all c++ keywords
    };


    template <typename Tsource>
    CPPScanner<Tsource>::CPPScanner(Tsource &source) :
        Scanner(source)
    {
        p_all.add(L'\x0001', L'\xFFFF');

        p_numeric.add('0', '9');

        p_hexadecimal.add('0', '9');
        p_hexadecimal.add('A', 'F');
        p_hexadecimal.add('a', 'f');

        p_alpha.add('_');
        p_alpha.add('A', 'Z');
        p_alpha.add('a', 'z');
        // TODO: refine alpha range
        p_alpha.add(L'\x0100', L'\xFFFF');

        p_alphanum = CharSet(p_alpha);
        p_alphanum.add('0', '9');

        p_hexprefixes[0] = L"0x";
        p_hexprefixes[1] = L"0X";

        p_escapes[0] = L"\\'";
        p_escapes[1] = L"\\\"";
        p_escapes[2] = L"\\\\";
        p_escapes[3] = L"\\t";
        p_escapes[4] = L"\\r";
        p_escapes[5] = L"\\n";
        p_escapes[6] = L"\\b";
        p_escapes[7] = L"\\f";
        p_escapes[8] = L"\\0";

        p_compounds[0] = L"<<=";
        p_compounds[1] = L">>=";
        p_compounds[2] = L"->*";
        p_compounds[3] = L"...";
        p_compounds[4] = L"==";
        p_compounds[5] = L"!=";
        p_compounds[6] = L"=>";
        p_compounds[7] = L"&&";
        p_compounds[8] = L"::";
        p_compounds[9] = L"++";
        p_compounds[10] = L"--";
        p_compounds[11] = L"||";
        p_compounds[12] = L">=";
        p_compounds[13] = L"<=";
        p_compounds[14] = L"+=";
        p_compounds[15] = L"-=";
        p_compounds[16] = L"/=";
        p_compounds[17] = L"*=";
        p_compounds[18] = L"%=";
        p_compounds[19] = L"&=";
        p_compounds[20] = L"|=";
        p_compounds[21] = L"^=";
        p_compounds[22] = L"->";
        p_compounds[23] = L".*";

        p_pplbrk[0] = L"\\\n\r";
        p_pplbrk[1] = L"\\\r\n";
        p_pplbrk[2] = L"\\\n";
        p_pplbrk[3] = L"\\";

        int i = 0;
        p_keywords[i++] = L"alignas";
        p_keywords[i++] = L"alignof";
        p_keywords[i++] = L"asm";
        p_keywords[i++] = L"auto";
        p_keywords[i++] = L"bool";
        p_keywords[i++] = L"break";
        p_keywords[i++] = L"case";
        p_keywords[i++] = L"catch";
        p_keywords[i++] = L"char";
        p_keywords[i++] = L"char16_t";
        p_keywords[i++] = L"char32_t";
        p_keywords[i++] = L"class";
        p_keywords[i++] = L"const";
        p_keywords[i++] = L"constexpr";
        p_keywords[i++] = L"const_cast";
        p_keywords[i++] = L"continue";
        p_keywords[i++] = L"decltype";
        p_keywords[i++] = L"default";
        p_keywords[i++] = L"delete";
        p_keywords[i++] = L"do";
        p_keywords[i++] = L"double";
        p_keywords[i++] = L"dynamic_cast";
        p_keywords[i++] = L"else";
        p_keywords[i++] = L"enum";
        p_keywords[i++] = L"explicit";
        p_keywords[i++] = L"export";
        p_keywords[i++] = L"extern";
        p_keywords[i++] = L"false";
        p_keywords[i++] = L"final";
        p_keywords[i++] = L"float";
        p_keywords[i++] = L"for";
        p_keywords[i++] = L"friend";
        p_keywords[i++] = L"goto";
        p_keywords[i++] = L"if";
        p_keywords[i++] = L"inline";
        p_keywords[i++] = L"int";
        p_keywords[i++] = L"long";
        p_keywords[i++] = L"mutable";
        p_keywords[i++] = L"namespace";
        p_keywords[i++] = L"new";
        p_keywords[i++] = L"noexcept";
        p_keywords[i++] = L"nullptr";
        p_keywords[i++] = L"operator";
        p_keywords[i++] = L"override";
        p_keywords[i++] = L"private";
        p_keywords[i++] = L"protected";
        p_keywords[i++] = L"public";
        p_keywords[i++] = L"register";
        p_keywords[i++] = L"reinterpret_cast";
        p_keywords[i++] = L"return";
        p_keywords[i++] = L"short";
        p_keywords[i++] = L"signed";
        p_keywords[i++] = L"sizeof";
        p_keywords[i++] = L"static";
        p_keywords[i++] = L"static_assert";
        p_keywords[i++] = L"static_cast";
        p_keywords[i++] = L"struct";
        p_keywords[i++] = L"switch";
        p_keywords[i++] = L"template";
        p_keywords[i++] = L"this";
        p_keywords[i++] = L"thread_local";
        p_keywords[i++] = L"throw";
        p_keywords[i++] = L"true";
        p_keywords[i++] = L"try";
        p_keywords[i++] = L"typedef";
        p_keywords[i++] = L"typedid";
        p_keywords[i++] = L"typename";
        p_keywords[i++] = L"union";
        p_keywords[i++] = L"unsigned";
        p_keywords[i++] = L"using";
        p_keywords[i++] = L"virtual";
        p_keywords[i++] = L"void";
        p_keywords[i++] = L"volatile";
        p_keywords[i++] = L"wchar_t";
        p_keywords[i++] = L"while";
    }

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
                case IncrementalCurrentType::Comment: {
                    token.Type = TokenType::Comment;

                    int level = 1;
                    auto result = ContinueTo(L"", L"*/", true, nullptr, false, stok, level);

                    if (result != srMatchTrimmedEOF) {
                        data.Current = int(IncrementalCurrentType::None);
                    }

                    break;
                }

                case IncrementalCurrentType::Preprocessor: {
                    token.Type = TokenType::Preprocessor;

                    int level = 1;
                    auto result = ContinueWhile(
                        p_all , false,
                        [=]() { return IsPreprocessorLineBreak(); },
                        stok
                    );

                    if (!Match(result) || EndsWith(stok, p_pplbrk) == NO_MATCH) {
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
    int CPPScanner<Tsource>::IsPreprocessorLineBreak()
    {
        int length;
        if (CheckAny(p_pplbrk, length, false) != NO_MATCH) {
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
        auto result = AnyMatch(
            token,
            [this](auto &t) { return FromTokenWhile(L"//", p_all, false, nullptr, false, t); },
            [this](auto &t) { return FromTo(L"/*", L"*/", true, nullptr, false, t); }
        );

        if (result == srMatchTrimmedEOF) {
            data.Current = int(IncrementalCurrentType::Comment);
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
            L"#", p_all, false,
            [=]() { return IsPreprocessorLineBreak(); },
            false, token
        ));

        if (result && EndsWith(token, p_pplbrk) != NO_MATCH) {
            data.Current = int(IncrementalCurrentType::Preprocessor);
        }

        return result;
    }

} // namespace k_cppparser
