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

        int IsEscape(EscapeCheckContext context);
        int ScanInterpolationComment(bool multiline);
        int InterpolationInnerScan(bool checkcharescape, bool multiline);

        bool ScanIdent(k_parser::SourceToken &token);
        bool ScanComment(k_parser::IncrementalScanData &data, k_parser::SourceToken &token);

        template <typename Tinner>
        typename k_parser::Scanner<Tsource>::ScanResult ScanString(Tinner inner, k_parser::SourceToken &token);

        template <typename Tinner>
        typename k_parser::Scanner<Tsource>::ScanResult ScanVerbatimString(k_parser::IncrementalScanData &data, Tinner inner, k_parser::SourceToken &token);

        bool ScanCharacter(k_parser::SourceToken &token);
        bool ScanIntegerPostfix();
        bool ScanRealPostfix();
        bool ScanHexadecimal(k_parser::SourceToken &token);
        bool ScanDecimal(k_parser::SourceToken &token);
        bool ScanReal(k_parser::SourceToken &token);
        bool ScanPreprocessor(k_parser::SourceToken &token);

    private:
        typedef k_parser::Token<char> TokenChar;
        typedef typename k_parser::CharRange CharRange;

        static const CharRange p_all[1];         // all characters set
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
    const typename CSScanner<Tsource>::CharRange CSScanner<Tsource>::p_all[] = {
        { L'\x0001', L'\xFFFF' }
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
        k_parser::SourceToken stok;
        Token result;
        if (SkipToToken(stok)) {
            if (includespacers && stok.Length > 0) {
                result.Type = TokenType::Spacer;
                result.SourceToken = stok;
            } else {
                result = ScanToken(data);
            }
        }
        return result;
    }

    template <typename Tsource>
    typename CSScanner<Tsource>::Token CSScanner<Tsource>::ScanToken(k_parser::IncrementalScanData &data)
    {
        Token token;
        k_parser::SourceToken stok;

        if (data.Current != int(IncrementalCurrentType::None)) {
            switch (data.Current) {
                case IncrementalCurrentType::Comment: {
                    token.Type = TokenType::Comment;

                    int level = 1;
                    auto result = ContinueTo(C(""), C("*/"), true, nullptr, false, stok, level);

                    if (result != ScanResult::MatchTrimmedEOF) {
                        data.Current = int(IncrementalCurrentType::None);
                    }

                    break;
                }

                case IncrementalCurrentType::VerbatimString: {
                    token.Type = TokenType::String;

                    int level = 1;
                    auto result = ContinueTo(C(""), C("\""), true, nullptr, false, stok, level);

                    if (result != ScanResult::MatchTrimmedEOF) {
                        data.Current = int(IncrementalCurrentType::None);
                    }

                    break;
                }
            }

            token.SourceToken = stok;

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
            if (ScanIdent(stok)) {
                token.Type = TokenType::Identifier;

                if (TokenCheckAny(stok, A(p_keywords)) != NO_MATCH) {
                    token.Type = TokenType::Keyword;
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

                [this](auto &t) {
                    return ScanString(
                        [this]() { return InterpolationInnerScan(true, false); },
                        t
                    );
                },

                [this, &data](auto &t) {
                    return ScanVerbatimString(
                        data,
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
            if (Match(ScanVerbatimString(data, nullptr, stok))) {
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
        if (token.Type == TokenType::None) {
            bool validsymbol =
                CheckAny(A(p_compounds), stok, p_it) ||
                CheckAny(C(".();,{}=[]:<>+-*/?%&|^!~"), stok, p_it);

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
    int CSScanner<Tsource>::IsEscape(EscapeCheckContext context)
    {
        k_parser::SourceToken token;

        auto unicodeescape = FromTokenWhile(
            C("\\u"), p_hexadecimal, false, nullptr,
            false, token, false
        );

        if (Match(unicodeescape)) {
            return token.Length;
        }

        if (context == eccCharacter) {
            int length;
            auto it = p_it;
            if (CheckAny(A(p_escapes), length, it) != NO_MATCH) {
                return length;
            }

            unicodeescape = FromTokenWhile(
                C("\\x"), p_hexadecimal, false, nullptr,
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
        auto result = FromTo(C("/*"), C("*/"), multiline, nullptr, false, token, false);

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
            C("{"), C("}"), multiline,
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
    bool CSScanner<Tsource>::ScanComment(k_parser::IncrementalScanData &data, k_parser::SourceToken &token)
    {
        auto result = AnyMatch(
            token,
            [this](auto &t) { return FromTokenWhile(C("//"), p_all, false, nullptr, false, t); },
            [this](auto &t) { return FromTo(C("/*"), C("*/"), true, nullptr, false, t); }
        );

        if (result == ScanResult::MatchTrimmedEOF) {
            data.Current = int(IncrementalCurrentType::Comment);
        }

        return Match(result);
    }

    template <typename Tsource>
    template <typename Tinner>
    typename k_parser::Scanner<Tsource>::ScanResult CSScanner<Tsource>::ScanString(Tinner inner, k_parser::SourceToken &token)
    {
        return FromTo(C("\""), C("\""), false, inner, false, token);
    }

    template <typename Tsource>
    template <typename Tinner>
    typename k_parser::Scanner<Tsource>::ScanResult CSScanner<Tsource>::ScanVerbatimString(k_parser::IncrementalScanData &data, Tinner inner, k_parser::SourceToken &token)
    {
        auto result = FromTo(C("@\""), C("\""), true, inner, false, token);

        switch (result) {
            // continue with contigous double quoted strings only if there was full match
            case ScanResult::Match: {
                SourceToken tok;
                ScanResult next;
                while (Match(next = FromTo(C("\""), C("\""), true, inner, false, tok))) {
                    result = next;
                    token.Length += tok.Length;
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
    bool CSScanner<Tsource>::ScanCharacter(k_parser::SourceToken &token)
    {
        auto result = FromTo(
            C("'"), C("'"), false, [this]() { return IsEscape(eccCharacter); },
            false, token
        );

        return Match(result);
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanIntegerPostfix()
    {
        return CheckAny(C("lLuU"), p_it) != NO_MATCH;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanRealPostfix()
    {
        return CheckAny(C("fFdDmM"), p_it) != NO_MATCH;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanHexadecimal(k_parser::SourceToken &token)
    {
        auto result = Match(FromTokenWhile(
            A(p_hexprefixes), p_hexadecimal, false, nullptr,
            false, token
        ));

        if (result && ScanIntegerPostfix()) {
            ++token.Length;
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
        auto result = FromTokenWhile(C("."), p_numeric, false, nullptr, true, token);

        if (Match(result)) {
            // optional E/e part
            if (CheckAny(C("eE"), p_it) != NO_MATCH) {
                ++token.Length;

                // optional +/- after exponent sign
                if (CheckAny(C("+-"), p_it) != NO_MATCH) {
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
    bool CSScanner<Tsource>::ScanPreprocessor(k_parser::SourceToken &token)
    {
        auto result = FromTokenWhile(C("#"), p_all, false, nullptr, false, token);
        return Match(result);
    }

} // namespace k_csparser
