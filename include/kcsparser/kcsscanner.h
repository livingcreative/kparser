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

        // namespaces are shit
        using ScannerSourceIterator = k_parser::ScannerSourceIterator;
        using SourceLength = k_parser::SourceLength;
        using ScanResult = k_parser::ScanResult;

        enum class TokenType
        {
            None,              // token haven't been scanned yet or scan finished
            Identifier,        // any valid identifier
            Keyword,           // keyword (special identifiers)
            Number,            // any integer number, decimal or hexadecimal (might be incomplete)
            RealNumber,        // any real (float or double) number (might be incomplete)
            Character,         // character (single quoted literal, might be malformed)
            String,            // string
            InterpolationString,
            VerbatimString,
            VerbatimInterpolationString,
            SingleLineComment, //
            MultiLineComment,  //
            Symbol,            // any standalone character or compiund sequence
            Preprocessor,      // preprocessor token (as a whole, not parsed, including possible comments inside)
            Spacer,            // sequence of spaces/line breaks
            Invalid            // invalid token/character
        };

        using Token = k_parser::ScannerToken<TokenType>;

        enum class Mode
        {
            Source,
            MultiLineComment,
            VerbatimString,
            InterpolationString,
            VerbatimInterpolationString
        };

        Token ReadToken(Mode mode, bool includespacers = false);

    private:
        enum EscapeCheckContext
        {
            eccIdentifier,
            eccCharacter
        };

        SourceLength IsEscape(EscapeCheckContext context, const ScannerSourceIterator &it) const;
        SourceLength ScanInterpolationComment(bool multiline, const ScannerSourceIterator &it) const;
        SourceLength InterpolationInnerScan(bool checkcharescape, bool multiline, const ScannerSourceIterator &it) const;

        void ScanIdent(Token &token, ScannerSourceIterator &it) const;
        void ScanComment(Token &token, ScannerSourceIterator &it) const;
        void ScanString(Token &token, ScannerSourceIterator &it) const;
        void ScanInterpolatedString(Token &token, ScannerSourceIterator &it) const;
        void ScanVerbatimString(Token &token, ScannerSourceIterator &it) const;
        void ScanNumber(Token &token, ScannerSourceIterator &it) const;
        void ScanCharacter(Token &token, ScannerSourceIterator &it) const;
        void ScanPreprocessor(Token &token, ScannerSourceIterator &it) const;

        template <typename Tinner>
        ScanResult ScanString(Tinner inner, ScannerSourceIterator &it) const;

        template <typename Tinner>
        ScanResult ScanVerbatimString(Tinner inner, ScannerSourceIterator &it) const;

        bool ScanIntegerPostfix(ScannerSourceIterator &it) const;
        bool ScanRealPostfix(ScannerSourceIterator &it) const;
        bool ScanHexadecimal(ScannerSourceIterator &it) const;
        bool ScanDecimal(ScannerSourceIterator &it) const;
        bool ScanReal(ScannerSourceIterator &it) const;

    private:
        using TokenChar = k_parser::TokenT<char>;
        using CharRange = k_parser::CharRange;

        static const CharRange p_numeric[1];     // numeric [0 - 9] characters set
        static const CharRange p_hexadecimal[3]; // hexadecimal [0 - 9, A - F, a - f] characters set
        static const CharRange p_alpha[4];       // alpha characters set (not exact, unicode range needs refinement)
        static const CharRange p_alphanum[5];    // alpha + numeric characters set

        static const TokenChar p_hexprefixes[2]; // hexadecimal prefixes
        static const TokenChar p_escapes[9];     // all predefined escape sequences
        static const TokenChar p_compounds[21];  // all compound sequences

        static const TokenChar p_keywords[75];   // all global keywords
    };


    template <typename Tsource>
    CSScanner<Tsource>::CSScanner(Tsource &source) :
        Scanner(source)
    {}

    template <typename Tsource>
    typename CSScanner<Tsource>::Token CSScanner<Tsource>::ReadToken(Mode mode, bool includespacers)
    {
        auto it = It();

        if (it) {
            return Token();
        }

        switch (mode) {
            case Mode::MultiLineComment: {
                // this scan can not fail in any case, however can be still trimmed
                //      NOTE: multiline can be set to false to return trimmed comments line by line
                //      if this is not partial scan (this should be consistent with regular comment scans)
                auto result = ContinueTo(C("*/"), true, nullptr, it);
                // result passed as is, in case of trimmed EOL/EOF scan must continue in MultiLineComment mode
                return Token(TokenType::MultiLineComment, Scanner::ReadToken(it), result);
            }

            case Mode::VerbatimString: {
                auto result = ContinueTo(C("\""), true, nullptr, it);
                return Token(TokenType::VerbatimString, Scanner::ReadToken(it), result);
            }

            case Mode::InterpolationString: {
                auto result = ContinueTo(
                    C("\""), C("{"), false,
                    [this](auto &it) { return InterpolationInnerScan(true, false, it); },
                    it
                );
                return Token(TokenType::InterpolationString, Scanner::ReadToken(it), result);
            }

            case Mode::VerbatimInterpolationString:
                break;
        }

        // regular skip to next token
        auto hastoken = SkipToToken(it);

        if (includespacers && PeekToken(it)) {
            return Token(TokenType::Spacer, Scanner::ReadToken(it), ScanResult::Match);
        }

        DiscardToken(it);

        if (!hastoken) {
            return Token();
        }


        Token token;
        auto c = CharCurrent();

        // identifier starts with following characters, so it's most
        // high probability to try scan identifier first
        if (c == '_' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' ||
            c >= L'\x0100' || c == '\\')
        {
            ScanIdent(token, it);
        }
        // next most frequent token type is comment, comments start with /
        // character, so try scan a comment when / encountered
        else if (c == '/')
        {
            ScanComment(token, it);
        }
        // from this character string literal can start
        // try scan string
        else if (c == '"')
        {
            ScanString(token, it);
        }
        // only number could start with digits, try to scan number
        else if (c >= '0' && c <= '9')
        {
            ScanNumber(token, it);
        }
        // from this character interpolated string literal can start
        // try scan interpolated string
        else if (c == '$')
        {
            ScanInterpolatedString(token, it);
        }
        // from . character real number can start, or it's a single dot
        else if (c == '.')
        {
            if (ScanReal(it)) {
                token.Type = TokenType::RealNumber;
                token.Result = ScanResult::Match;
            }
        }
        // from ' character only character literal can start
        else if (c == '\'')
        {
            ScanCharacter(token, it);
        }
        // "verbatim" character can start string or @ident
        else if (c == '@')
        {
            auto result = ScanVerbatimString(nullptr, it);
            if (Match(result)) {
                token.Type = TokenType::VerbatimString;
                token.Result = result;
            } else {
                GetCharToken(nullptr, it);
                ScanIdent(token, it);
            }
        }
        // only preprocessor directive can start with # character
        else if (c == '#')
        {
            ScanPreprocessor(token, it);
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
                token.Result = ScanResult::Match;
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
    k_parser::SourceLength CSScanner<Tsource>::IsEscape(EscapeCheckContext context, const ScannerSourceIterator &it) const
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
    k_parser::SourceLength CSScanner<Tsource>::ScanInterpolationComment(bool multiline, const ScannerSourceIterator &it) const
    {
        auto current = it;
        auto result = FromTo(C("/*"), C("*/"), multiline, nullptr, current);
        return Match(result) ? (current - it) : 0;
    }

    template <typename Tsource>
    k_parser::SourceLength CSScanner<Tsource>::InterpolationInnerScan(bool checkcharescape, bool multiline, const ScannerSourceIterator &it) const
    {
        if (checkcharescape) {
            auto esc = IsEscape(eccCharacter, it);
            if (esc > 0) {
                return esc;
            }
        }

        auto current = it;
        /*
        auto result = FromToWithNesting(
            C("{"), C("}"), multiline,
            [this, multiline](auto &it) { return ScanInterpolationComment(multiline, it); },
            current
        );
        */

        auto result = Check(C("{{"), current) || Check(C("}}"), current);

        return Match(result) ? (current - it) : 0;
    }

    template <typename Tsource>
    void CSScanner<Tsource>::ScanIdent(Token &token, ScannerSourceIterator &it) const
    {
        auto result = FromSetWhile(
            p_alpha, p_alphanum,
            [this](auto &it) { return IsEscape(eccIdentifier, it); },
            it
        );
        if (Match(result)) {
            token.Type = TokenType::Identifier;

            // NOTE: leave keywords check to parser?
            if (TokenCheckAny(PeekToken(it), A(p_keywords)) != NO_MATCH) {
                token.Type = TokenType::Keyword;
            }

            token.Result = ScanResult::Match;
        }
    }

    template <typename Tsource>
    void CSScanner<Tsource>::ScanComment(Token &token, ScannerSourceIterator &it) const
    {
        if (Match(FromToEndOfLine(C("//"), it))) {
            token.Type = TokenType::SingleLineComment;
            token.Result = ScanResult::Match;
            return;
        }


        auto result = FromTo(C("/*"), C("*/"), true, nullptr, it);
        if (Match(result)) {
            token.Type = TokenType::MultiLineComment;
            token.Result = result;
        }
    }

    template <typename Tsource>
    void CSScanner<Tsource>::ScanString(Token &token, ScannerSourceIterator &it) const
    {
        auto result = ScanString([this](auto &it) { return IsEscape(eccCharacter, it); }, it);
        if (Match(result)) {
            token.Type = TokenType::String;
            token.Result = result;
        }
    }

    template <typename Tsource>
    void CSScanner<Tsource>::ScanInterpolatedString(Token &token, ScannerSourceIterator &it) const
    {
        // interpolated string could be usual double quoted string or
        // @ verbatim string, "eat" $ character and try to scan one of
        // string variants
        GetCharToken(nullptr, it);

        //auto result = ScanString([this](auto &it) { return InterpolationInnerScan(true, false, it); }, it);
        auto result = FromTo(
            C("\""), C("\""), C("{"), false,
            [this](auto &it) { return InterpolationInnerScan(true, false, it); },
            it
        );
        if (Match(result)) {
            token.Type = TokenType::InterpolationString;
            token.Result = result;
            return;
        }

        result = ScanVerbatimString([this](auto &it) { return InterpolationInnerScan(false, true, it); }, it);
        if (Match(result)) {
            token.Type = TokenType::VerbatimInterpolationString;
            token.Result = result;
            return;
        }

        token.Type = TokenType::Invalid;
    }

    template <typename Tsource>
    void CSScanner<Tsource>::ScanNumber(Token &token, ScannerSourceIterator &it) const
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

        token.Result = ScanResult::Match;
    }



    template <typename Tsource>
    template <typename Tinner>
    k_parser::ScanResult CSScanner<Tsource>::ScanString(Tinner inner, ScannerSourceIterator &it) const
    {
        return FromTo(C("\""), C("\""), false, inner, it);
    }

    template <typename Tsource>
    template <typename Tinner>
    k_parser::ScanResult CSScanner<Tsource>::ScanVerbatimString(Tinner inner, ScannerSourceIterator &it) const
    {
        auto result = FromTo(C("@\""), C("\""), true, inner, it);

        switch (result) {
            // continue with contigous double quoted strings only if there was full match
            case ScanResult::Match: {
                ScanResult next;
                // TODO: also should account full match only
                while (Match(next = FromTo(C("\""), C("\""), true, inner, it))) {
                    result = next;
                }
                break;
            }

            case ScanResult::MatchTrimmedEOF:
                break;
        }

        return result;
    }

    template <typename Tsource>
    void CSScanner<Tsource>::ScanCharacter(Token &token, ScannerSourceIterator &it) const
    {
        auto result = FromTo(
            C("'"), C("'"), false, [this](auto &it) { return IsEscape(eccCharacter, it); },
            it
        );

        if (Match(result)) {
            token.Type = TokenType::Character;
            token.Result = ScanResult::Match;
        }
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanIntegerPostfix(ScannerSourceIterator &it) const
    {
        return CheckAny(C("lLuU"), it) != 0;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanRealPostfix(ScannerSourceIterator &it) const
    {
        return CheckAny(C("fFdDmM"), it) != 0;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanHexadecimal(ScannerSourceIterator &it) const
    {
        auto result = Match(FromTokenWhile(A(p_hexprefixes), p_hexadecimal, nullptr, false, it));

        if (result) {
            ScanIntegerPostfix(it);
        }

        return result;
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanDecimal(ScannerSourceIterator &it) const
    {
        auto result = FromSetWhile(p_numeric, p_numeric, nullptr, it);
        return Match(result);
    }

    template <typename Tsource>
    bool CSScanner<Tsource>::ScanReal(ScannerSourceIterator &it) const
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
    void CSScanner<Tsource>::ScanPreprocessor(Token &token, ScannerSourceIterator &it) const
    {
        auto result = FromToEndOfLine(C("#"), it);
        if (Match(result)) {
            token.Type = TokenType::Preprocessor;
            token.Result = ScanResult::Match;
        }
    }


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

} // namespace k_csparser
