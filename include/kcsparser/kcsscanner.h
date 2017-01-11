/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017

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
    class CSScanner : public k_parser::Scanner<wchar_t, Tsource>
    {
    public:
        CSScanner(Tsource &source);

        enum class TokenType
        {
            Unknown,      // token haven't been scanned yet
            Identifier,   // any valid identifier (including possible keywords)
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

        bool ReadToken(bool includespacers, Token &token);

    private:
        enum EscapeCheckContext
        {
            eccIdentifier,
            eccCharacter
        };

        int IsEscape(EscapeCheckContext context);
        int ScanInterpolationComment(bool multiline);
        int InterpolationInnerScan(bool checkcharescape, bool multiline);

        bool ScanIdent(k_parser::SourceToken &token);
        bool ScanComment(k_parser::SourceToken &token);

        template <typename Tinner>
        ScanResult ScanString(Tinner inner, k_parser::SourceToken &token);

        template <typename Tinner>
        ScanResult ScanVerbatimString(Tinner inner, k_parser::SourceToken &token);

        bool ScanCharacter(k_parser::SourceToken &token);
        bool ScanIntegerPostfix(k_parser::SourceToken &token);
        bool ScanHexadecimal(k_parser::SourceToken &token);
        bool ScanDecimal(k_parser::SourceToken &token);
        bool ScanReal(k_parser::SourceToken &token);
        bool ScanPreprocessor(k_parser::SourceToken &token);

    private:
        CharSet p_all;            // all characters set
        CharSet p_numeric;        // numeric [0 - 9] characters set
        CharSet p_hexadecimal;    // hexadecimal [0 - 9, A - F, a - f] characters set
        CharSet p_alpha;          // alpha characters set (not exact, unicode range needs refinement)
        CharSet p_alphanum;       // alpha + numeric characters set

        token_t p_hexprefixes[2]; // hexadecimal prefixes
        token_t p_escapes[9];     // all predefined escape sequences
        token_t p_compounds[21];  // all compound sequences
    };


    template <typename Tsource>
    CSScanner<Tsource>::CSScanner(Tsource &source) :
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
        p_compounds[2] = L"==";
        p_compounds[3] = L"!=";
        p_compounds[4] = L"=>";
        p_compounds[5] = L"&&";
        p_compounds[6] = L"??";
        p_compounds[7] = L"++";
        p_compounds[8] = L"--";
        p_compounds[9] = L"||";
        p_compounds[10] = L">=";
        p_compounds[11] = L"<=";
        p_compounds[12] = L"+=";
        p_compounds[13] = L"-=";
        p_compounds[14] = L"/=";
        p_compounds[15] = L"*=";
        p_compounds[16] = L"%=";
        p_compounds[17] = L"&=";
        p_compounds[18] = L"|=";
        p_compounds[19] = L"^=";
        p_compounds[20] = L"->";
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ReadToken(bool includespacers, Token &token)
    {
        k_parser::SourceToken stok;
        bool result = SkipToToken(stok);
        if (result) {
            if (includespacers && stok.Length > 0) {
                token.Type = TokenType::Spacer;
                token.SourceToken = stok;
            } else {
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
                    }
                }
                // next most frequent token type is comment, comments start with /
                // character, so try scan a comment when / encountered
                else if (c == '/')
                {
                    if (ScanComment(stok)) {
                        token.Type = TokenType::Comment;
                    }
                }
                // from this character string literal can start
                // try scan string
                else if (c == '"')
                {
                    auto isstr = ScanString([this]() { return IsEscape(eccCharacter); }, stok);
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
                        SourceToken tok;
                        if (ScanIntegerPostfix(tok)) {
                            stok.Length += tok.Length;
                        } else {
                            // try to scan "fractional" part of a number
                            if (ScanReal(tok)) {
                                stok.Length += tok.Length;
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
                    GetCharToken(false, nullptr, stok);

                    k_parser::SourceToken tok;
                    bool isstring = Match(AnyMatch(
                        tok,

                        [this](auto t) {
                            return ScanString(
                                [this]() { return InterpolationInnerScan(true, false); },
                                t
                            );
                        },

                        [this](auto t) {
                            return ScanVerbatimString(
                                [this]() { return InterpolationInnerScan(false, true); },
                                t
                            );
                        }
                    ));

                    if (isstring) {
                        stok.Length += tok.Length;
                    }

                    token.Type = isstring ? TokenType::String : TokenType::Invalid;
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
                // "verbatim" character can start string or @ident
                else if (c == '@')
                {
                    if (Match(ScanVerbatimString(nullptr, stok))) {
                        token.Type = TokenType::String;
                    } else {
                        GetCharToken(false, nullptr, stok);

                        SourceToken ident;
                        if (!ScanIdent(ident)) {
                            token.Type = TokenType::Invalid;
                        } else {
                            stok.Length += ident.Length;
                            token.Type = TokenType::Identifier;
                        }
                    }
                }
                // only preprocessor directive can start with # character
                else if (c == '#')
                {
                    ScanPreprocessor(stok);
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
            }
        }
        return result;
    }

    template <typename Tsource>
    int CSScanner<Tsource>::IsEscape(EscapeCheckContext context)
    {
        k_parser::SourceToken token;

        auto unicodeescape = FromTokenWhile(
            L"\\u", p_hexadecimal, false, nullptr,
            false, token, false
        );

        if (Match(unicodeescape)) {
            return token.Length;
        }

        if (context == eccCharacter) {
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
        }

        return 0;
    }

    template <typename Tsource>
    int CSScanner<Tsource>::ScanInterpolationComment(bool multiline)
    {
        k_parser::SourceToken token;
        auto result = FromTo(L"/*", L"*/", multiline, nullptr, false, token, false);

        return Match(result) ? token.Length : 0;
    }

    template <typename Tsource>
    int CSScanner<Tsource>::InterpolationInnerScan(bool checkcharescape, bool multiline)
    {
        if (checkcharescape) {
            auto esc = IsEscape(eccCharacter);
            if (esc > 0) {
                return esc;
            }
        }

        k_parser::SourceToken interp;
        auto result = FromTo(
            L"{", L"}", multiline,
            [this, multiline]() { return ScanInterpolationComment(multiline); },
            true, interp, false
        );

        return Match(result) ? interp.Length : 0;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanIdent(k_parser::SourceToken &token)
    {
        auto result = FromSetWhile(
            p_alpha, p_alphanum, false, [this]() { return IsEscape(eccIdentifier); },
            token
        );

        return Match(result);
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanComment(k_parser::SourceToken &token)
    {
        return Match(AnyMatch(
            token,
            [this](auto token) { return FromTokenWhile(L"//", p_all, false, nullptr, false, token); },
            [this](auto token) { return FromTo(L"/*", L"*/", true, nullptr, false, token); }
        ));
    }

    template <typename Tsource>
    template <typename Tinner>
    typename CSScanner<Tsource>::ScanResult CSScanner<Tsource>::ScanString(Tinner inner, k_parser::SourceToken &token)
    {
        return FromTo(L"\"", L"\"", false, inner, false, token);
    }

    template <typename Tsource>
    template <typename Tinner>
    typename CSScanner<Tsource>::ScanResult CSScanner<Tsource>::ScanVerbatimString(Tinner inner, k_parser::SourceToken &token)
    {
        auto result = FromTo(L"@\"", L"\"", true, inner, false, token);

        // continue with contigous double quoted strings only if there was full match
        if (result == srMatch) {
            SourceToken tok;
            ScanResult next;
            while (Match(next = FromTo(L"\"", L"\"", true, inner, false, tok))) {
                result = next;
                token.Length += tok.Length;
            }
        }

        return result;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanCharacter(k_parser::SourceToken &token)
    {
        auto result = FromTo(
            L"'", L"'", false, [this]() { return IsEscape(eccCharacter); },
            false, token
        );

        return Match(result);
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanIntegerPostfix(k_parser::SourceToken &token)
    {
        return CheckAny(L"lLuU", token);
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanHexadecimal(k_parser::SourceToken &token)
    {
        auto result = Match(FromTokenWhile(
            p_hexprefixes, p_hexadecimal, false, nullptr,
            false, token
        ));

        k_parser::SourceToken pf;
        if (result && ScanIntegerPostfix(pf)) {
            token.Length += pf.Length;
        }

        return result;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanDecimal(k_parser::SourceToken &token)
    {
        auto result = FromSetWhile(p_numeric, p_numeric, false, nullptr, token);
        return Match(result);
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanReal(k_parser::SourceToken &token)
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
            if (CheckAny(L"fdFD") != NO_MATCH) {
                ++token.Length;
            }
        }

        return Match(result);
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanPreprocessor(k_parser::SourceToken &token)
    {
        auto result = FromTokenWhile(L"#", p_all, false, nullptr, false, token);
        return Match(result);
    }

} // namespace k_csscanner
