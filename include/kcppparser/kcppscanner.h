/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017 - 2021

    https://github.com/livingcreative/kparser

    kcppscanner.h
        Example c++ scanner implementation, general example on using
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
            Preprocessor,      // preprocessor token (# sign which start preprocessor line)
            PreprocessorHash,  // preprocessor hash (stringify, #), valid inside preprocessor mode
            PreprocessorPaste, // preprocessor paste (##), valid inside preprocessor mode
            Spacer,            // sequence of spaces/line breaks
            Invalid            // invalid token/character
        };

        // token information returned by scanner
        //      return token might be incomplete or partial
        //      result indicates that
        //      in case of incomplete token scan must be continued with respective mode
        //      caller (typically parser class) should maintain current scan mode
        struct Token
        {
            Token() :
                Type(TokenType::None),
                Result(k_parser::ScanResult::NoMatch)
            {}

            Token(TokenType _type, const k_parser::SourceToken &_token, k_parser::ScanResult _result) :
                Type(_type),
                SourceToken(_token),
                Result(_result)
            {}

            operator bool() const { return Type != TokenType::None; }

            TokenType             Type;
            k_parser::SourceToken SourceToken;
            k_parser::ScanResult  Result;
        };

        enum class Mode
        {
            Source,              // regular source mode
            SingleLineComment,   // single line comment mode
            MultiLineComment,    // multiline comment mode
            Preprocessor,        // regular preprocessor mode
            PreprocessorInclude, // preprocessor include directive
            Assembler            // inline assembly
        };

        Token ReadToken(Mode mode, bool includespacers = false);

    private:
        // inner scans
        k_parser::SourceLength IsEscape(const k_parser::ScannerSourceIterator &it) const;

        // primary scans (will set token type and advance if succeded)
        void ScanIdent(Token &token, k_parser::ScannerSourceIterator &it) const;
        void ScanComment(Token &token, k_parser::ScannerSourceIterator &it) const;
        void ScanString(Token &token, bool preprocessor, k_parser::ScannerSourceIterator &it) const;
        void ScanPreprocessorString(Token &token, k_parser::ScannerSourceIterator &it) const;
        void ScanNumber(Token &token, k_parser::ScannerSourceIterator &it) const;
        void ScanCharacter(Token &token, k_parser::ScannerSourceIterator &it) const;
        void ScanPreprocessor(Token &token, k_parser::ScannerSourceIterator &it) const;

        // wide string/char helper
        bool ScanWideStringOrCharacter(Token &token, k_parser::ScannerSourceIterator &it) const;

        // comment scan helpers
        k_parser::ScanResult ScanSingleLineComment(k_parser::ScannerSourceIterator &it) const;
        k_parser::ScanResult ContinueSingleLineComment(k_parser::ScannerSourceIterator &it) const;
        k_parser::ScanResult ScanMultiLineComment(k_parser::ScannerSourceIterator &it) const;

        // number scan helpers
        bool ScanIntegerPostfix(k_parser::ScannerSourceIterator &it) const;
        bool ScanRealPostfix(k_parser::ScannerSourceIterator &it) const;
        bool ScanHexadecimal(k_parser::ScannerSourceIterator &it) const;
        bool ScanDecimal(k_parser::ScannerSourceIterator &it) const;
        bool ScanReal(k_parser::ScannerSourceIterator &it) const;

    private:
        using TokenChar = k_parser::Token<char>;
        using CharRange = k_parser::CharRange;

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
    CPPScanner<Tsource>::CPPScanner(Tsource &source) :
        Scanner(source)
    {}

    template <typename Tsource>
    typename CPPScanner<Tsource>::Token CPPScanner<Tsource>::ReadToken(Mode mode, bool includespacers)
    {
        // start at current iterator position
        auto it = It();

        // check if source ended and return empty token
        if (it) {
            return Token();
        }

        // continue to scan trimmed comments or skip to next token depending on mode
        switch (mode) {
            case Mode::SingleLineComment: {
                // this scan can not fail in any case
                auto result = ContinueSingleLineComment(it);
                return Token(TokenType::SingleLineComment, Scanner::ReadToken(it), result);
            }

            case Mode::MultiLineComment: {
                // this scan can not fail in any case, however can be still trimmed
                //      NOTE: multiline can be set to false to return trimmed comments line by line
                //      if this is not partial scan (this should be consistent with regular comment scans)
                auto result = ContinueTo(C("*/"), true, nullptr, it);
                // result passed as is, in case of trimmed EOL/EOF scan must continue in MultiLineComment mode
                return Token(TokenType::MultiLineComment, Scanner::ReadToken(it), result);
            }

            default: {
                // regular skip to next token
                auto hastoken = SkipToToken(it, mode != Mode::Preprocessor);

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
            // L can start "wide" string/character, try it first
            if (c != 'L' || !ScanWideStringOrCharacter(token, it)) {
                ScanIdent(token, it);
            }
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
            // in preprocessor include mode escapes inside the string are ignored
            ScanString(token, mode == Mode::PreprocessorInclude, it);
        }
        // preprocessor include string
        else if (c == '<' && mode == Mode::PreprocessorInclude) {
            ScanPreprocessorString(token, it);
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
        // only preprocessor directive can start with # character
        else if (c == '#')
        {
            switch (mode) {
                case Mode::Preprocessor:
                    ScanPreprocessor(token, it);
                    break;

                case Mode::Source:
                    GetCharToken(it);
                    token.Type = TokenType::Preprocessor;
                    token.Result = ScanResult::Match;
                    break;
            }
        }
        else if (c == '\\' && (mode == Mode::Preprocessor || mode == Mode::PreprocessorInclude))
        {
            GetCharToken(it);
            token.Type = TokenType::Symbol;
            token.Result = ScanResult::Match;
        }

        // if none of previous checks detected any kind of token
        // this is symbol or invalid character token, check for it here
        // try to match compounds first, and single characters next
        if (token.Type == TokenType::None) {
            auto validsymbol =
                CheckAny(A(p_compounds), it) ||
                CheckAny(C(".();,{}=[]:<>+-*/?%&|^!~"), it);

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
    void CPPScanner<Tsource>::ScanIdent(Token &token, k_parser::ScannerSourceIterator &it) const
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
    void CPPScanner<Tsource>::ScanComment(Token &token, k_parser::ScannerSourceIterator &it) const
    {
        token.Result = ScanSingleLineComment(it);
        if (Match(token.Result)) {
            token.Type = TokenType::SingleLineComment;
            return;
        }

        token.Result = ScanMultiLineComment(it);
        if (Match(token.Result)) {
            token.Type = TokenType::MultiLineComment;
        }
    }

    template <typename Tsource>
    void CPPScanner<Tsource>::ScanString(Token &token, bool preprocessor, k_parser::ScannerSourceIterator &it) const
    {
        token.Result = FromTo(
            C("\""), C("\""), false,
            [this, preprocessor](auto &it) { return preprocessor ? 0 : IsEscape(it); },
            it
        );

        if (Match(token.Result)) {
            token.Type = TokenType::String;
        }
    }

    template <typename Tsource>
    void CPPScanner<Tsource>::ScanPreprocessorString(Token &token, k_parser::ScannerSourceIterator &it) const
    {
        token.Result = FromTo(C("<"), C(">"), false, nullptr, it);
        if (Match(token.Result)) {
            token.Type = TokenType::String;
        }
    }

    template <typename Tsource>
    bool CPPScanner<Tsource>::ScanWideStringOrCharacter(Token &token, k_parser::ScannerSourceIterator &it) const
    {
        auto current = it;

        // eat L
        GetCharToken(current);

        ScanString(token, false, current);
        if (Match(token.Result)) {
            it = current;
            return true;
        }

        ScanCharacter(token, current);
        if (Match(token.Result)) {
            it = current;
            return true;
        }

        return false;
    }

    template <typename Tsource>
    void CPPScanner<Tsource>::ScanNumber(Token &token, k_parser::ScannerSourceIterator &it) const
    {
        // it is at least some integer number token
        token.Type = TokenType::Number;

        // hexadecimal number literal can't have real part and it starts with 0, try to scan it
        if (!ScanHexadecimal(it)) {
            // it's not hexadecimal number - it's integer or real
            ScanDecimal(it);

            // try scan integer postfix, if there's postfix it's integer
            // number
            if (!ScanIntegerPostfix(it)) {
                // otherwise it's real
                if (ScanRealPostfix(it) || ScanReal(it)) {
                    token.Type = TokenType::RealNumber;
                }
            }
        }

        token.Result = ScanResult::Match;
    }

    template <typename Tsource>
    void CPPScanner<Tsource>::ScanCharacter(Token &token, k_parser::ScannerSourceIterator &it) const
    {
        token.Result = FromTo(C("'"), C("'"), false, [this](auto &it) { return IsEscape(it); }, it);
        token.Type = TokenType::Character;
    }

    template <typename Tsource>
    void CPPScanner<Tsource>::ScanPreprocessor(Token &token, k_parser::ScannerSourceIterator &it) const
    {
        token.Result = ScanResult::Match;

        if (Check(C("##"), it)) {
            token.Type = TokenType::PreprocessorPaste;
            return;
        }

        GetCharToken(it);
        token.Type = TokenType::PreprocessorHash;
     }


    template <typename Tsource>
    k_parser::ScanResult CPPScanner<Tsource>::ScanSingleLineComment(k_parser::ScannerSourceIterator &it) const
    {
        if (NoMatch(Check(C("//"), it))) {
            return ScanResult::NoMatch;
        }

        // check the rest of possible comment with multiline escapes
        return ContinueSingleLineComment(it);
    }

    template <typename Tsource>
    k_parser::ScanResult CPPScanner<Tsource>::ContinueSingleLineComment(k_parser::ScannerSourceIterator &it) const
    {
        auto start = it;
        auto scan = ContinueToEndOfLine(it);

        // currently it's complete match
        auto hasescape = false;

        // NOTE: this loop is not required, it allows to return complete comment if
        // it's not partial scan
        while (Match(scan) && EndsWith(PeekToken(start, it), C("\\"))) {
            hasescape = true;

            // in regular scan there will be line break at the end of the last scanned line,
            // it must be skipped
            SkipLineBreak(it);

            start = it;
            scan = ContinueToEndOfLine(it);
        }

        return hasescape && NoMatch(scan) && it ?
            ScanResult::MatchTrimmedEOF :
            ScanResult::Match;
    }

    template <typename Tsource>
    k_parser::ScanResult CPPScanner<Tsource>::ScanMultiLineComment(k_parser::ScannerSourceIterator &it) const
    {
        return FromTo(C("/*"), C("*/"), true, nullptr, it);
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
        "char", "char16_t", "char32_t", "class", "const", "const_cast", "constexpr",
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

} // namespace k_cppparser
