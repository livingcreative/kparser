/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017

    https://github.com/livingcreative/kparser

    kscanner.h
        basic types for scanner and scanner class template
        declarations
*/

#pragma once

#include "khelpers.h"
#include <algorithm>


namespace k_parser
{
    //
    //  common types
    //
    typedef int        SourceSize;     // unit of text location measure
    typedef SourceSize SourcePosition; // position inside source text
    typedef SourceSize SourceLength;   // number of characters in source text


    // source token - character sequence location and count inside source
    struct SourceToken
    {
        // initialize empty source token
        constexpr SourceToken() noexcept :
            Position(0),
            Length(0)
        {}

        // initialize source token with given position and length
        constexpr SourceToken(SourcePosition position, SourceLength length = 0) noexcept :
            Position(position),
            Length(length)
        {}

        constexpr operator bool() const noexcept { return Length > 0; }

        SourcePosition Position; // token position inside source
        SourceLength   Length;   // token length
    };


    // common incremental scanner data struct
    //      holds intermediate scanner state
    //      TODO: should this have state parameter type as a template parameter?
    struct IncrementalScanData
    {
        // initialize empty structure
        IncrementalScanData() :
            Current(0),
            NestingLevel(0)
        {}

        int Current;      // current scanner context
        int NestingLevel; // current nesting level
    };


    // token (sequence of characters, sort of string)
    //      T - type for character, like char or wchar_t
    template <typename T>
    struct Token
    {
        // construct empty token
        constexpr Token() noexcept :
            Text(nullptr),
            Length(0)
        {}

        // construct token from constant literal string
        template <size_t length>
        constexpr Token(const T (&text)[length]) noexcept :
            Text(text),
            Length(length - 1) // NOTE: C strings have implicit 0 at the end, it's not included in length
        {}

        // construct token from given text and length
        constexpr Token(const T *text, SourceSize length) noexcept :
            Text(text),
            Length(length)
        {}

        // construct token from given source text and SourceToken
        constexpr Token(const T *source, const SourceToken &token) noexcept :
            Text(source + token.Position),
            Length(token.Length)
        {}

        // const iterators
        constexpr const T* begin() const noexcept { return Text; }
        constexpr const T* end() const noexcept { return Text + Length; }

        const T    *Text;   // pointer to first token character
        SourceSize  Length; // character count
    };


    // iterator forward declaration
    class ScannerSourceIterator;


    // -------------------------------------------------------------------------------
    //  ScannerSource
    // -------------------------------------------------------------------------------
    //
    //  example class of scanner source interface
    //      this class has no implementation!
    //      it just shows what functions real ScannerSource class should implement
    //      this could be done as abstract class, however that will harm overall performance
    //      and it's even not required
    //      since there's no way to declare "static" C++ interface without
    //      virtual functions this declaration left here as an example
    //      T - type for character, like char or wchar_t

    template <typename T>
    class ScannerSource
    {
    public:
        // actual character type definition
        typedef T char_t;

        // length of the whole source text
        constexpr SourceLength Length() const noexcept;

        // returns source character at current Position
        //      reading at invalid Position is not allowed
        //      Scanner won't read at invalid position
        constexpr T CharCurrent(const ScannerSourceIterator &it) const noexcept;

        // converts source token to token
        constexpr Token<T> SourceTokenToToken(const SourceToken &token) const noexcept;
    };


    // -------------------------------------------------------------------------------
    //  ScannerSourceIterator
    // -------------------------------------------------------------------------------
    //
    // source iterator - holds position inside source
    // used by scanner to track current scanning position
    class ScannerSourceIterator
    {
    public:
        constexpr ScannerSourceIterator() noexcept :
            p_position(0),
            p_length(0)
        {}

        constexpr ScannerSourceIterator(const ScannerSourceIterator &source) noexcept :
            p_position(source.p_position),
            p_length(source.p_length)
        {}

        template <typename T>
        constexpr ScannerSourceIterator(const T &source) noexcept :
            p_position(0),
            p_length(source.Length())
        {}

        // end of source indicator
        //      true when Position reached end of source (Position == Length)
        //      false otherwise
        constexpr operator bool() const noexcept { return p_position >= p_length; }

        constexpr ScannerSourceIterator& operator=(const ScannerSourceIterator &source) noexcept
        {
            p_position = source.p_position;
            p_length = source.p_length;
            return *this;
        }

        // iterator advance/decrement operators

        constexpr SourceLength operator-(const ScannerSourceIterator &rhs) const noexcept
        {
            return p_position - rhs.p_position;
        }

        constexpr ScannerSourceIterator operator+(SourceLength advance) const noexcept
        {
            return ScannerSourceIterator(p_position + advance, p_length);
        }

        constexpr ScannerSourceIterator& operator+=(SourceLength advance) noexcept
        {
            p_position += advance;
            return *this;
        }

        constexpr ScannerSourceIterator operator-(SourceLength advance) const noexcept
        {
            return ScannerSourceIterator(p_position - advance, p_length);
        }

        constexpr ScannerSourceIterator& operator-=(SourceLength advance) noexcept
        {
            p_position -= advance;
            return *this;
        }

        // current position inside source text
        constexpr SourcePosition Position() const noexcept { return p_position; }

        // make SourceToken from to iterators
        //      b must be equal or further than a
        static constexpr SourceToken difftotoken(const ScannerSourceIterator &a, const ScannerSourceIterator &b) noexcept
        {
            return SourceToken(a.p_position, b.p_position - a.p_position);
        }

    private:
        constexpr ScannerSourceIterator(SourcePosition position, SourceLength length) noexcept :
            p_position(position),
            p_length(length)
        {}

    private:
        SourcePosition p_position;
        SourceLength   p_length;
    };


    // -------------------------------------------------------------------------------
    //  ScannerStringSource
    // -------------------------------------------------------------------------------
    //
    //  basic scanner source from string implementation class
    //      use this type as Tsource when source text presented as a string
    //      T - type for character, like char or wchar_t

    template <typename T>
    class ScannerStringSource
    {
    public:
        // initialize source with pointer to characters and its length
        // position is always initialized to 0
        constexpr ScannerStringSource(const T *source, SourceLength length) noexcept :
            p_source(source),
            p_length(length)
        {}

        // followings are ScannerSource implementation

        typedef T char_t;

        constexpr SourceLength Length() const noexcept { return p_length; }

        constexpr T CharCurrent(const ScannerSourceIterator &it) const noexcept { return p_source[it.Position()]; }

        constexpr Token<T> SourceTokenToToken(const SourceToken &token) const noexcept
        {
            return Token<T>(p_source + token.Position, token.Length);
        }

    private:
        const T      *p_source;
        SourceLength  p_length;
    };


    // -------------------------------------------------------------------------------
    //  ScannerSpecialCharChecker
    // -------------------------------------------------------------------------------
    //
    //  basic scanner special characters checker class
    //      this class is used for special characters check implementation
    //      this one is default implementation with common special character set

    template <typename Tsource>
    class ScannerSpecialCharChecker
    {
    public:
        // check if there's a space character at current source position
        static constexpr bool IsSpace(const Tsource &source, const ScannerSourceIterator &it) noexcept;
        // check if there's a line break character or sequence at current source position
        //      return 0 if there's no line break or
        //      length of line break sequence if there's one
        static constexpr SourceLength IsBreak(const Tsource &source, const ScannerSourceIterator &it) noexcept;
    };


    // -------------------------------------------------------------------------------
    //  Scanner
    // -------------------------------------------------------------------------------
    //
    //  basic scanner implementation class

    template <typename Tsource, typename Tchecker = ScannerSpecialCharChecker<Tsource>>
    class Scanner
    {
    public:
        // template parameters type inner definitions
        typedef typename Tsource::char_t char_t;
        typedef Tsource source_t;
        typedef Token<char_t> token_t;

        // bring some types from khelpers.h for ease of use in specific scanners
        typedef CharRange CharRange;
        typedef CharSet CharSet;

    public:
        // construct scanner for given source
        constexpr Scanner(const Tsource &source) noexcept;
        // construct scanner for given source and existing iterator to start scanning from given position
        //      iterator MUST be derived from given source
        constexpr Scanner(const Tsource &source, const ScannerSourceIterator &start) noexcept;

    protected:
        // scan result returned by some of scanner functions
        enum class ScanResult
        {
            NoMatch,         // no match
            Match,           // full match
            MatchTrimmedEOL, // partial match, terminated by line end
            MatchTrimmedEOF  // partial match, terminated by end of source
        };

    protected:
        // handy checks for ScanResult value
        static constexpr bool Match(ScanResult result) noexcept { return result != ScanResult::NoMatch; }
        static constexpr bool NoMatch(ScanResult result) noexcept { return result == ScanResult::NoMatch; }

        // handy conversion of bool to ScanResult
        static constexpr ScanResult Match(bool result) noexcept { return result ? ScanResult::Match : ScanResult::NoMatch; }

        // variable arguments AnyMatch implementation
        // tries to match one of given match functions
        // returns ScanResult and token when matched
        // basically handy way to implement "or" grammar checks, like
        //      seqence1 || seqence2 || seqence3 ...
        // where sequenceN is one of predefined scan functions

        template <typename Tc>
        static constexpr ScanResult AnyMatch(SourceToken &token, Tc func) noexcept;

        template <typename Tc, typename... Args>
        static constexpr ScanResult AnyMatch(SourceToken &token, Tc first, Args... args) noexcept;

        // variable arguments SequenceMatch implementaion
        // tries to match all given match functions as one token
        // returns Scanresult and token when matched
        // basically handy way to implement checks for tokens composed of
        // several checks in particular order

        template <typename Tc>
        static constexpr ScanResult SequenceMatch(SourceToken &token, Tc func) noexcept;

        template <typename Tc, typename... Args>
        static constexpr ScanResult SequenceMatch(SourceToken &token, Tc first, Args... args) noexcept;

        // helper functions to compare tokens of same or different character types
        template <typename T1, typename T2>
        static constexpr int CompareTokens(
            const Token<T1> &a, const Token<T2> &b,
            SourceSize limit = -1
        );

        // helper functions to help awsome c++ compiler with types

        template <typename T, size_t length>
        static constexpr FixedArray<T> A(T (&items)[length]) noexcept { return FixedArray<T>(items); }

        template <typename T, size_t length>
        static constexpr Token<T> C(T (&items)[length]) noexcept { return Token<T>(items); }

        // helper functions to check if token starts or ends with one of given sequences

        template <typename T>
        constexpr bool StartsWith(const SourceToken &token, const Token<T> &sequence) noexcept;

        template <typename T>
        constexpr int StartsWith(const SourceToken &token, const FixedArray<const Token<T>> &sequences) noexcept;

        template <typename T>
        constexpr bool EndsWith(const SourceToken &token, const Token<T> &sequence) noexcept;

        template <typename T>
        constexpr int EndsWith(const SourceToken &token, const FixedArray<const Token<T>> &sequences) noexcept;

        // helper function to convert SourceToken to token
        constexpr token_t SourceTokenToToken(const SourceToken &token) const noexcept
        {
            return p_source.SourceTokenToToken(token);
        }

        constexpr char_t CharCurrent() const noexcept { return p_source.CharCurrent(p_it); }
        constexpr SourceLength LineCount() const noexcept { return p_lines; }

        // checks if there's at least count characters before source end
        constexpr bool HasCharacters(SourceLength count) const noexcept
        {
            return count <= (p_source.Length() - p_it.Position());
        }

        // skip all whitespace (and optionally, line break) characters till start of the
        // next possible token
        // optionally returns skipped sequence as a source token

        constexpr bool SkipToToken(SourceToken &token, bool nextline = true) noexcept;

        constexpr bool SkipToToken(bool nextline = true) noexcept
        {
            SourceToken token;
            return SkipToToken(token, nextline);
        }

        // checks from ScannerSpecialCharChecker

        constexpr bool IsSpace() const noexcept { return Tchecker::IsSpace(p_source, p_it); }
        constexpr SourceLength IsBreak() const noexcept { return Tchecker::IsBreak(p_source, p_it); }

        // check that there's a match of a character or sequence at current position

        template <typename T>
        bool Check(T c, bool increment = true);

        template <typename T>
        bool Check(const Token<T> &s, bool increment = true);

        // following CheckAny functions are sort of high-level versions of Check to help
        // perform checks against multiple characters or sequences

        static constexpr int NO_MATCH = -1;

        template <typename T>
        int CheckAny(const Token<T> &characters, bool increment = true);

        template <typename T>
        bool CheckAny(const Token<T> &characters, SourceToken &token, bool increment = true);

        template <typename T>
        int CheckAny(
            const FixedArray<const Token<T>> &compounds, int &length,
            bool increment = true
        );

        template <typename T>
        bool CheckAny(
            const FixedArray<const Token<T>> &compounds, SourceToken &token,
            bool increment = true
        );

        // helper functions to check already scanned token
        template <typename T>
        int TokenCheckAny(const SourceToken &token, const Token<T> &characters);

        template <typename T>
        int TokenCheckAny(
            const SourceToken &token,
            const FixedArray<const Token<T>> &sequences
        );

        // following are core scan functions which perform scans for common patterns
        // Tinner template parameter designates "inner scan" lambda function
        // (thanks to awesome language design where you can't declare something you
        // can actually use)
        // inner scan function has following signature: int (void) and returns length of
        // detected nested sequence (or 0 if there's no seqence detected)

        template <typename Tinner>
        bool GetCharToken(
            bool nextline, Tinner inner, SourceToken &token,
            bool increment = true
        );

        template <typename Tinner>
        bool CheckCharToken(
            const CharSet &set, bool nextline, Tinner inner, SourceToken &token,
            bool increment = true
        );

        template <typename Tinner>
        ScanResult ContinueWhile(
            const CharSet &whileset, bool multiline, Tinner inner, SourceToken &token,
            bool increment = true
        );

        template <typename T, typename Tinner>
        ScanResult ContinueTo(
            const Token<T> &fromtoken, const Token<T> &totoken, bool multiline,
            Tinner inner, bool allownesting, SourceToken &token, int &nestinglevel,
            bool increment = true
        );

        template <typename Tinner>
        ScanResult FromSetWhile(
            const CharSet &from, const CharSet &whileset, bool multiline,
            Tinner inner, SourceToken &token, bool increment = true
        );

        template <typename T, typename Tinner>
        ScanResult FromTokenWhile(
            const Token<T> &from, const CharSet &whileset, bool multiline,
            Tinner inner, bool notemptywhile, SourceToken &token, bool increment = true
        );

        template <typename T, typename Tinner>
        ScanResult FromTokenWhile(
            const FixedArray<const Token<T>> &from, const CharSet &whileset, bool multiline,
            Tinner inner, bool notemptywhile, SourceToken &token, bool increment = true
        );

        template <typename T, typename Tinner>
        ScanResult FromTo(
            const Token<T> &fromtoken, const Token<T> &totoken, bool multiline,
            Tinner inner, bool allownesting, SourceToken &token, bool increment = true
        );

    private:
        // template ugly "magic" for allowing to pass lambda and nullptr values
        // as callback parameters (for inner scans)
        template <typename Tc>
        struct caller
        {
            static SourceLength call(Tc callee) { return callee(); }
        };

        template <>
        struct caller<decltype(nullptr)>
        {
            static constexpr SourceLength call(decltype(nullptr) callee) { return 0; }
        };

    private:
        const Tsource         &p_source;
        ScannerSourceIterator  p_it;
        SourceLength           p_lines;
    };


    template <typename Tsource>
    constexpr bool ScannerSpecialCharChecker<Tsource>::IsSpace(const Tsource &source, const ScannerSourceIterator &it) noexcept
    {
        if (it) {
            return false;
        }

        auto c = source.CharCurrent(it);

        return c >= 0 && c <= ' ' && c != '\r' && c != '\n';
    }

    template <typename Tsource>
    constexpr SourceLength ScannerSpecialCharChecker<Tsource>::IsBreak(const Tsource &source, const ScannerSourceIterator &it) noexcept
    {
        if (it) {
            return 0;
        }

        auto seqit = it;
        auto c = source.CharCurrent(seqit);

        if (c == '\r') {
            seqit += 1;
            c = source.CharCurrent(seqit);
        }

        if (c == '\n') {
            seqit += 1;
        }

        return seqit - it;
    }


    template <typename Tsource, typename Tchecker>
    constexpr Scanner<Tsource, Tchecker>::Scanner(const Tsource &source) noexcept :
        p_source(source),
        p_it(source),
        p_lines(0)
    {}

    template <typename Tsource, typename Tchecker>
    constexpr Scanner<Tsource, Tchecker>::Scanner(const Tsource &source, const ScannerSourceIterator &start) noexcept :
        p_source(source),
        p_it(start),
        p_lines(0)
    {}

    template <typename Tsource, typename Tchecker>
    template <typename Tc>
    constexpr typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::AnyMatch(SourceToken &token, Tc func) noexcept
    {
        return func(token);
    }

    template <typename Tsource, typename Tchecker>
    template <typename Tc, typename... Args>
    constexpr typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::AnyMatch(SourceToken &token, Tc first, Args... args) noexcept
    {
        auto result = first(token);
        if (result == ScanResult::NoMatch) {
            result = AnyMatch(token, args...);
        }
        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename Tc>
    constexpr typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::SequenceMatch(SourceToken &token, Tc func) noexcept
    {
        return func(token);
    }

    template <typename Tsource, typename Tchecker>
    template <typename Tc, typename... Args>
    constexpr typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::SequenceMatch(SourceToken &token, Tc first, Args... args) noexcept
    {
        auto result = first(token);
        if (result == ScanResult::Match) {
            SourceToken next;
            result = SequenceMatch(next, args...);
            if (result == ScanResult::NoMatch) {
                p_source -= token.Length;
            } else {
                token.Length += next.Length;
            }
        }
        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T1, typename T2>
    constexpr int Scanner<Tsource, Tchecker>::CompareTokens(const Token<T1> &a, const Token<T2> &b, SourceSize limit)
    {
        auto minlen = a.Length < b.Length ? a.Length : b.Length;

        if (limit >= 0 && minlen > limit) {
            minlen = limit;
        }

        auto pa = a.Text;
        auto pb = b.Text;

        while (minlen-- > 0) {
            // TODO: refine diff comparison
            auto diff = int(*pa++) - int(*pb++);
            if (diff != 0) {
                return diff;
            }
        }

        if (limit >= 0) {
            return 0;
        }

        return a.Length - b.Length;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    constexpr bool Scanner<Tsource, Tchecker>::StartsWith(const SourceToken &token, const Token<T> &sequence) noexcept
    {
        auto tok = SourceTokenToToken(token);
        bool result =
            tok.Length >= sequence.Length &&
            CompareTokens(tok, sequence, sequence.Length) == 0;
        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    constexpr int Scanner<Tsource, Tchecker>::StartsWith(const SourceToken &token, const FixedArray<const Token<T>> &sequences) noexcept
    {
        auto found = std::find_if(
            sequences.begin(), sequences.end(),
            [&token, this](const auto &s) { return StartsWith(token, s); }
        );

        return found == sequences.end() ? NO_MATCH : int(found - sequences.begin());
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    constexpr bool Scanner<Tsource, Tchecker>::EndsWith(const SourceToken &token, const Token<T> &sequence) noexcept
    {
        auto tok = SourceTokenToToken(token);
        bool result =
            tok.Length >= sequence.Length &&
            CompareTokens(
                token_t(tok.Text + tok.Length - sequence.Length, sequence.Length),
                sequence, sequence.Length
            ) == 0;
        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    constexpr int Scanner<Tsource, Tchecker>::EndsWith(const SourceToken &token, const FixedArray<const Token<T>> &sequences) noexcept
    {
        auto found = std::find_if(
            sequences.begin(), sequences.end(),
            [&token, this](const auto &s) { return EndsWith(token, s); }
        );

        return found == sequences.end() ? NO_MATCH : int(found - sequences.begin());
    }

    template <typename Tsource, typename Tchecker>
    constexpr bool Scanner<Tsource, Tchecker>::SkipToToken(SourceToken &token, bool nextline) noexcept
    {
        token = SourceToken(p_it.Position());

        while (true) {
            if (Tchecker::IsSpace(p_source, p_it)) {
                p_it += 1;
                ++token.Length;
            } else {
                auto br = Tchecker::IsBreak(p_source, p_it);

                if (br == 0) {
                    return !p_it;
                }

                if (nextline) {
                    p_it += br;
                    token.Length += br;
                    ++p_lines;
                } else {
                    return false;
                }
            }
        }
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    bool Scanner<Tsource, Tchecker>::Check(T c, bool increment)
    {
        // TODO: handle different type char comparison
        bool result = !p_it && p_source.CharCurrent(p_it) == c;

        if (result && increment) {
            it += 1;
        }

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    bool Scanner<Tsource, Tchecker>::Check(const Token<T> &s, bool increment)
    {
        bool result =
            HasCharacters(s.Length) &&
            CompareTokens(
                s,
                p_source.SourceTokenToToken(SourceToken(p_it.Position(), s.Length))
            ) == 0;

        if (result && increment) {
            it += s.Length;
        }

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    int Scanner<Tsource, Tchecker>::CheckAny(const Token<T> &characters, bool increment)
    {
        auto found = std::find_if(
            characters.begin(), characters.end(),
            [increment, this](const auto &c) { return Check(c, increment); }
        );

        return found == characters.end() ? NO_MATCH : int(found - characters.begin());
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    bool Scanner<Tsource, Tchecker>::CheckAny(const Token<T> &characters, SourceToken &token, bool increment)
    {
        auto pos = p_it.Position();
        auto result = CheckAny(characters, increment) != NO_MATCH;

        token = SourceToken(pos, result ? 1 : 0);

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    int Scanner<Tsource, Tchecker>::CheckAny(const FixedArray<const Token<T>> &compounds, int &length, bool increment)
    {
        length = 0;

        auto found = std::find_if(
            compounds.begin(), compounds.end(),
            [increment, this](const auto &c) { return Check(c, increment); }
        );

        if (found == compounds.end()) {
            return NO_MATCH;
        }

        length = found->Length;

        return int(found - compounds.begin());
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    bool Scanner<Tsource, Tchecker>::CheckAny(const FixedArray<const Token<T>> &compounds, SourceToken &token, bool increment)
    {
        auto length = 0;
        auto pos = p_it.Position();

        bool result = CheckAny(compounds, length, increment) != NO_MATCH;

        token = SourceToken(pos, length);

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    int Scanner<Tsource, Tchecker>::TokenCheckAny(const SourceToken &token, const Token<T> &characters)
    {
        if (token.Length != 1) {
            return NO_MATCH;
        }

        auto t = SourceTokenToToken(token);

        auto found = std::find_if(
            characters.begin(), characters.end(),
            [&t](const auto &c) {
                return Helpers::CharValue(c) == Helpers::CharValue(t.Text[0]);
            }
        );

        return found == characters.end() ? NO_MATCH : int(found - characters.begin());
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    int Scanner<Tsource, Tchecker>::TokenCheckAny(const SourceToken &token, const FixedArray<const Token<T>> &sequences)
    {
        auto t = SourceTokenToToken(token);

        auto bound = std::lower_bound(
            sequences.begin(), sequences.end(), t,
            [](const auto &a, const auto &b) {
                return CompareTokens(a, b) < 0;
            }
        );

        if (bound != sequences.end() && CompareTokens(t, *bound) >= 0) {
            return int(bound - sequences.begin());
        }

        return NO_MATCH;
    }

    template <typename Tsource, typename Tchecker>
    template <typename Tinner>
    bool Scanner<Tsource, Tchecker>::GetCharToken(bool nextline, Tinner inner, SourceToken &token, bool increment)
    {
        token = SourceToken(p_it.Position());

        auto result = false;
        auto len = Tchecker::IsBreak(p_source, p_it);

        if (len == 0) {
            len = caller<Tinner>::call(inner);
            if (len == 0 && p_it) {
                return false;
            }
            token.Length = len == 0 ? 1 : len;
            result = true;
        } else {
            if (nextline) {
                token.Length = len;
                ++p_lines;
            }
            result = nextline;
        }

        if (result && increment) {
            p_it += token.Length;
        }

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename Tinner>
    bool Scanner<Tsource, Tchecker>::CheckCharToken(const CharSet &set, bool nextline, Tinner inner, SourceToken &token, bool increment)
    {
        auto result =
            GetCharToken(nextline, inner, token, false) &&
            (token.Length > 1 || set.in(p_source.CharCurrent(p_it)));

        if (result && increment) {
            p_it += token.Length;
        }

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename Tinner>
    typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::ContinueWhile(const CharSet &whileset, bool multiline, Tinner inner, SourceToken &token, bool increment)
    {
        token = SourceToken(p_it.Position());

        SourceToken cs;
        while (CheckCharToken(whileset, multiline, inner, cs)) {
            token.Length += cs.Length;
        }

        if (!increment) {
            p_it -= token.Length;
        }

        return ScanResult::Match;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T, typename Tinner>
    typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::ContinueTo(const Token<T> &fromtoken, const Token<T> &totoken, bool multiline, Tinner inner, bool allownesting, SourceToken &token, int &nestinglevel, bool increment)
    {
        token = SourceToken(p_it.Position());

        auto result = ScanResult::NoMatch;

        SourceToken cs;
        while (GetCharToken(multiline, inner, cs, false)) {
            if (allownesting && Check(fromtoken)) {
                ++nestinglevel;
                token.Length += fromtoken.Length;
                continue;
            }

            if (Check(totoken)) {
                --nestinglevel;
                token.Length += totoken.Length;

                if (nestinglevel == 0) {
                    result = ScanResult::Match;
                    break;
                }

                continue;
            }

            p_it += cs.Length;
            token.Length += cs.Length;
        }

        if (result != ScanResult::Match)
        {
            if (p_it) {
                result = ScanResult::MatchTrimmedEOF;
            } else {
                result = ScanResult::MatchTrimmedEOL;
            }
        }

        if (!increment) {
            p_it -= token.Length;
        }

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename Tinner>
    typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::FromSetWhile(const CharSet &from, const CharSet &whileset, bool multiline, Tinner inner, SourceToken &token, bool increment)
    {
        if (!CheckCharToken(from, multiline, inner, token)) {
            return ScanResult::NoMatch;
        }

        SourceToken cs;
        auto result = ContinueWhile(whileset, multiline, inner, cs);
        token.Length += cs.Length;

        if (!increment) {
            p_it -= token.Length;
        }

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T, typename Tinner>
    typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::FromTokenWhile(const Token<T> &from, const CharSet &whileset, bool multiline, Tinner inner, bool notemptywhile, SourceToken &token, bool increment)
    {
        token = SourceToken(p_it.Position());

        if (!Check(from)) {
            return ScanResult::NoMatch;
        }

        token.Length += from.Length;

        SourceToken cs;
        auto result = ContinueWhile(whileset, multiline, inner, cs);
        token.Length += cs.Length;

        if (notemptywhile && token.Length <= from.Length) {
            p_it -= token.Length;
            return ScanResult::NoMatch;
        }

        if (!increment) {
            p_it -= token.Length;
        }

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T, typename Tinner>
    typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::FromTokenWhile(const FixedArray<const Token<T>> &from, const CharSet &whileset, bool multiline, Tinner inner, bool notemptywhile, SourceToken &token, bool increment)
    {
        auto result = ScanResult::NoMatch;

        for (auto &&f : from) {
            result = FromTokenWhile(
                f, whileset, multiline, inner,
                notemptywhile, token, increment
            );

            if (result != ScanResult::NoMatch) {
                return result;
            }
        }

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T, typename Tinner>
    typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::FromTo(const Token<T> &fromtoken, const Token<T> &totoken, bool multiline, Tinner inner, bool allownesting, SourceToken &token, bool increment)
    {
        token = SourceToken(p_it.Position());

        if (!Check(fromtoken)) {
            return ScanResult::NoMatch;
        }

        token.Length += fromtoken.Length;

        int nestinglevel = 1;

        SourceToken cs;
        auto result = ContinueTo(fromtoken, totoken, multiline, inner, allownesting, cs, nestinglevel);
        token.Length += cs.Length;

        if (!increment) {
            p_it -= token.Length;
        }

        return result;
    }
} // namespace k_scanner
