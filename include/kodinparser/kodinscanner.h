/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017 - 2025

    https://github.com/livingcreative/kparser

    kodinscanner.h
        Example Odin scanner implementation, general example on using
        Scanner class
*/

#pragma once

#include "kparser/kscanner.h"
#include "kparser/kscannercommon.h"


namespace k_odinparser
{

    template <typename Tsource>
    class OdinScanner : public k_parser::Scanner<Tsource>, public k_parser::ScannerCommonDefs
    {
    public:
        OdinScanner(Tsource &source);

        // namespaces are shit
        using ScannerSourceIterator = k_parser::ScannerSourceIterator;
        using SourceLength = k_parser::SourceLength;
        using ScanResult = k_parser::ScanResult;

        enum class TokenType
        {
            None,              // token haven't been scanned yet
            Identifier,        // any valid identifier
            Keyword,           // keyword (special identifiers)
            Number,            // any integer number, decimal or hexadecimal (might be incomplete)
            RealNumber,        // any real (float or double) number (might be incomplete)
            Character,         // character (single quoted literal, might be malformed)
            String,            // any string (might be incomplete or malformed)
            SingleLineComment, // single line comment // ...
            MultiLineComment,  // multi line comment  /* ... */
            Symbol,            // any standalone character or compound sequence
            Directive,         // # directive
            Spacer,            // sequence of spaces/line breaks
            Invalid            // invalid token/character
        };

        // token information returned by scanner
        //      return token might be incomplete or partial
        //      result indicates that
        //      in case of incomplete token scan must be continued with respective mode
        //      caller (typically parser class) should maintain current scan mode
        using Token = k_parser::ScannerToken<TokenType>;

        // TODO: modes should make sence only for scanner itself
        //       currently some of these modes are actually parser state
        enum class Mode
        {
            Source,                  // regular source mode
            MultiLineComment,        // multiline comment mode
            Assembler                // inline assembly
        };

        Token ReadToken(Mode mode, int &nesting, bool includespacers = false);

    private:
        // inner scans
        SourceLength IsEscape(const ScannerSourceIterator &it) const;

        // primary scans (will set token type and advance if succeded)
        void ScanIdent(Token &token, ScannerSourceIterator &it) const;
        void ScanComment(Token &token, ScannerSourceIterator &it, int &nesting) const;
        void ScanString(Token &token, ScannerSourceIterator &it) const;
        void ScanRawString(Token &token, ScannerSourceIterator &it) const;
        void ScanNumber(Token &token, ScannerSourceIterator &it) const;
        void ScanCharacter(Token &token, ScannerSourceIterator &it) const;
        void ScanDirective(Token &token, ScannerSourceIterator &it) const;

        // comment scan helpers
        ScanResult ScanSingleLineComment(ScannerSourceIterator &it) const;
        ScanResult ScanMultiLineComment(ScannerSourceIterator &it, int &nesting) const;

        // number scan helpers
        bool ScanRealPostfix(ScannerSourceIterator &it) const;
        bool ScanHexadecimal(ScannerSourceIterator &it) const;
        bool ScanDecimal(ScannerSourceIterator &it) const;
        bool ScanReal(ScannerSourceIterator &it) const;

    private:
        static const TokenChar p_hexprefixes[6]; // hexadecimal prefixes
        static const TokenChar p_escapes[9];     // all predefined escape sequences
        static const TokenChar p_compounds[27];  // all compound sequences

        static const TokenChar p_keywords[39];   // all Odin keywords
    };


    template <typename Tsource>
    OdinScanner<Tsource>::OdinScanner(Tsource &source) :
        Scanner(source)
    {}

    template <typename Tsource>
    typename OdinScanner<Tsource>::Token OdinScanner<Tsource>::ReadToken(Mode mode, int &nesting, bool includespacers)
    {
        // start at current iterator position
        auto it = It();

        // check if source ended and return empty token
        if (it) {
            return Token();
        }

        // continue to scan trimmed comments or skip to next token depending on mode
        switch (mode) {
            case Mode::MultiLineComment: {
                // this scan can not fail in any case, however can be still trimmed
                //      NOTE: multiline can be set to false to return trimmed comments line by line
                //      if this is not partial scan (this should be consistent with regular comment scans)
                auto result = ContinueTo(C("/*"), C("*/"), true, true, nullptr, it, nesting);
                // result passed as is, in case of trimmed BRK/EOL/EOF scan must continue in MultiLineComment mode
                return Token(TokenType::MultiLineComment, Scanner::ReadToken(it), result);
            }

            default: {
                // regular skip to next token
                auto hastoken = SkipToToken(it);

                if (includespacers && PeekToken(it)) {
                    return Token(TokenType::Spacer, Scanner::ReadToken(it), ScanResult::Match);
                }

                DiscardToken(it);

                if (!hastoken) {
                    return Token();
                }
            }
        }


        // other modes are usual scans with minor diff in modes
        Token token;
        auto c = CharCurrent();

        // identifier starts with following characters, so it's most
        // high probability to try scan identifier first
        if (c == '_' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' ||
            c >= L'\x0100')
        {
            ScanIdent(token, it);
        }
        // next most frequent token type is comment, comments start with /
        // character, so try scan a comment when / encountered
        else if (c == '/')
        {
            ScanComment(token, it, nesting);
        }
        // from this character string literal can start
        // try scan string
        else if (c == '"')
        {
            ScanString(token, it);
        }
        else if (c == '`')
        {
            ScanRawString(token, it);
        }
        // only number could start with digits, try to scan number
        else if (c >= '0' && c <= '9')
        {
            ScanNumber(token, it);
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
        else if (c == '#')
        {
            ScanDirective(token, it);
        }

        // if none of previous checks detected any kind of token
        // this is symbol or invalid character token, check for it here
        // try to match compounds first, and single characters next
        if (token.Type == TokenType::None) {
            auto validsymbol =
                CheckAny(A(p_compounds), it) ||
                CheckAny(C(".();,{}=[]:<>+-*/?%&|^!~@$"), it);

            if (validsymbol) {
                token.Type = TokenType::Symbol;
                token.Result = ScanResult::Match;
            } else {
                // all other stuff (unknown/invalid symbols)
                GetCharToken(it);
                token.Type = TokenType::Invalid;
            }
        }

        token.SourceToken = Scanner::ReadToken(it);

        return token;
    }


    template <typename Tsource>
    k_parser::SourceLength OdinScanner<Tsource>::IsEscape(const ScannerSourceIterator &it) const
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
    void OdinScanner<Tsource>::ScanIdent(Token &token, ScannerSourceIterator &it) const
    {
        auto result = FromSetWhile(p_alpha, p_alphanum, nullptr, it);
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
    void OdinScanner<Tsource>::ScanComment(Token &token, ScannerSourceIterator &it, int &nesting) const
    {
        token.Result = ScanSingleLineComment(it);
        if (Match(token.Result)) {
            token.Type = TokenType::SingleLineComment;
            return;
        }

        token.Result = ScanMultiLineComment(it, nesting);
        if (Match(token.Result)) {
            token.Type = TokenType::MultiLineComment;
        }
    }

    template <typename Tsource>
    void OdinScanner<Tsource>::ScanString(Token &token, ScannerSourceIterator &it) const
    {
        token.Result = FromTo(
            C("\""), C("\""), false,
            [this](auto &it) { return IsEscape(it); },
            it
        );

        if (Match(token.Result)) {
            token.Type = TokenType::String;
        }
    }

    template <typename Tsource>
    void OdinScanner<Tsource>::ScanRawString(Token &token, ScannerSourceIterator &it) const
    {
        token.Result = FromTo(C("`"), C("`"), false, nullptr, it);

        if (Match(token.Result)) {
            token.Type = TokenType::String;
        }
    }

    template <typename Tsource>
    void OdinScanner<Tsource>::ScanNumber(Token &token, ScannerSourceIterator &it) const
    {
        // it is at least some integer number token
        token.Type = TokenType::Number;

        // hexadecimal number literal can't have real part and it starts with 0, try to scan it
        if (!ScanHexadecimal(it)) {
            // it's not hexadecimal number - it's integer or real
            ScanDecimal(it);

            // otherwise it's real
            if (ScanRealPostfix(it) || ScanReal(it)) {
                token.Type = TokenType::RealNumber;
            }
        }

        token.Result = ScanResult::Match;
    }

    template <typename Tsource>
    void OdinScanner<Tsource>::ScanCharacter(Token &token, ScannerSourceIterator &it) const
    {
        token.Result = FromTo(C("'"), C("'"), false, [this](auto &it) { return IsEscape(it); }, it);
        token.Type = TokenType::Character;
    }

    template <typename Tsource>
    void OdinScanner<Tsource>::ScanDirective(Token &token, ScannerSourceIterator &it) const
    {
        Eat(1, it);
        Check('+', it);
        FromSetWhile(p_alpha, p_alphanum, nullptr, it);
        token.Result = ScanResult::Match;
        token.Type = TokenType::Directive;
    }

    template <typename Tsource>
    k_parser::ScanResult OdinScanner<Tsource>::ScanSingleLineComment(ScannerSourceIterator &it) const
    {
        auto res = FromToEndOfLine(C("//"), it);
        return res >= 2 ? ScanResult::Match : ScanResult::NoMatch;
    }

    template <typename Tsource>
    k_parser::ScanResult OdinScanner<Tsource>::ScanMultiLineComment(ScannerSourceIterator &it, int &nesting) const
    {
        return FromToWithNesting(C("/*"), C("*/"), true, true, nullptr, nesting, it);
    }

    template <typename Tsource>
    bool OdinScanner<Tsource>::ScanRealPostfix(ScannerSourceIterator &it) const
    {
        return Check('i', it) != 0;
    }

    template <typename Tsource>
    bool OdinScanner<Tsource>::ScanHexadecimal(ScannerSourceIterator &it) const
    {
        return Match(FromTokenWhile(
            A(p_hexprefixes), p_hexadecimal,
            nullptr, false, it
        ));
    }

    template <typename Tsource>
    bool OdinScanner<Tsource>::ScanDecimal(ScannerSourceIterator &it) const
    {
        auto result = FromSetWhile(p_numeric, p_numeric, nullptr, it);
        return Match(result);
    }

    template <typename Tsource>
    bool OdinScanner<Tsource>::ScanReal(ScannerSourceIterator &it) const
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
    const typename OdinScanner<Tsource>::TokenChar OdinScanner<Tsource>::p_hexprefixes[] = {
        "0x", "0d", "0o", "0z", "0h", "0b"
    };

    template <typename Tsource>
    const typename OdinScanner<Tsource>::TokenChar OdinScanner<Tsource>::p_escapes[] = {
        "\\'",  "\\\"", "\\\\", "\\t", "\\r", "\\n", "\\b", "\\f", "\\0"
    };

    template <typename Tsource>
    const typename OdinScanner<Tsource>::TokenChar OdinScanner<Tsource>::p_compounds[] = {
        "<<=", ">>=", "%%=", "&&=", "||=", "..=", "..<", "---",
        "==", "!=", "%%", "&&", "..", "++",
        "--", "||", ">=", "<=", "+=", "-=", "/=", "*=", "%=", "&=",
        "|=", "^=", "->"
    };

    template <typename Tsource>
    const typename OdinScanner<Tsource>::TokenChar OdinScanner<Tsource>::p_keywords[] = {
        "asm","auto_cast", "bit_field", "bit_set", "break", "case", "cast",
        "context", "continue", "defer", "distinct", "do", "dynamic", "else",
        "enum", "fallthrough", "for", "foreign", "if", "import", "in", "map",
        "matrix", "not_in", "or_break", "or_continue", "or_else", "or_return",
        "package", "proc", "return", "struct", "switch", "transmute",
        "typeid", "union", "using", "when", "where"
    };

} // namespace k_odinparser
