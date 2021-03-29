/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017 - 2021

    https://github.com/livingcreative/kparser

    kscanner.h
        basic types for scanner and scanner class template
        declarations
*/

#pragma once

#include "khelpers.h"
#include <algorithm>
#include <cassert>


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
        {
            assert(position >= 0 && length >= 0);
        }

        // casting to bool returns true if token isn't zero length
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
        constexpr const T *begin() const noexcept { return Text; }
        constexpr const T *end() const noexcept { return Text + Length; }

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
        // construct empty iterator
        constexpr ScannerSourceIterator() noexcept :
            p_position(0),
            p_line(0),
            p_length(0)
        {}

        // copy iterator
        constexpr ScannerSourceIterator(const ScannerSourceIterator &source) noexcept :
            p_position(source.p_position),
            p_line(source.p_line),
            p_length(source.p_length)
        {}

        // construct iterator from "abstract" source
        //      iterator will point at source start
        //      T type must have Length() function defined which returns total length of the source
        template <typename T>
        constexpr ScannerSourceIterator(const T &source) noexcept :
            p_position(0),
            p_line(0),
            p_length(source.Length())
        {}

        // end of source indicator
        //      true when Position reached end of source (Position == Length)
        //      false otherwise
        constexpr operator bool() const noexcept { return p_position >= p_length; }

        // reassignment
        constexpr ScannerSourceIterator &operator=(const ScannerSourceIterator &source) noexcept
        {
            p_position = source.p_position;
            p_line = source.p_line;
            p_length = source.p_length;
            return *this;
        }

        // iterator advance/decrement operators

        // compute distance between two iterators
        constexpr SourceLength operator-(const ScannerSourceIterator &rhs) const noexcept
        {
            return p_position - rhs.p_position;
        }

        // mutate given iterator by advance
        constexpr ScannerSourceIterator &operator+=(SourceLength advance) noexcept
        {
            p_position += advance;
            return *this;
        }

        // advance internal line counter
        void AdvanceLine()
        {
            ++p_line;
        }

        // current position inside source text
        constexpr SourcePosition Position() const noexcept { return p_position; }
        constexpr SourcePosition Line() const noexcept { return p_line; }

        // make SourceToken from to iterators
        //      b must be equal or further than a
        static constexpr SourceToken difftotoken(const ScannerSourceIterator &a, const ScannerSourceIterator &b) noexcept
        {
            assert(b.p_position >= a.p_position);
            return SourceToken(a.p_position, b.p_position - a.p_position);
        }

    private:
        constexpr ScannerSourceIterator(SourcePosition position, SourceLength line, SourceLength length) noexcept :
            p_position(position),
            p_line(line),
            p_length(length)
        {}

    private:
        SourcePosition p_position;
        SourcePosition p_line;
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
        constexpr ScannerStringSource(const T *source, SourceLength length) noexcept :
            p_source(source),
            p_length(length)
        {}

        template <size_t length>
        constexpr ScannerStringSource(const T(&source)[length]) noexcept :
            p_source(source),
            p_length(length - 1) // NOTE: c strings are 0 terminated, exclude trailing 0
        {}

        // copy
        constexpr ScannerStringSource(const ScannerStringSource &source) noexcept :
            p_source(source.p_source),
            p_length(source.p_length)
        {}

        // no reason for moving
        ScannerStringSource(ScannerStringSource &&) = delete;

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
        typedef typename Tsource::char_t char_t;   // current character type
        typedef Tsource                  source_t; // current source type
        typedef Token<char_t>            token_t;  // current Token type

    public:
        // construct scanner for given source
        constexpr Scanner(const Tsource &source) noexcept;

        // construct scanner for given source and existing iterator to start scanning from given position
        //      iterator MUST be derived from given source
        constexpr Scanner(const Tsource &source, const ScannerSourceIterator &start) noexcept;

        // prevent copy/move
        Scanner(const Scanner &) = delete;
        Scanner(Scanner &&) = delete;

    protected:
        // scan result returned by some of scanner functions
        enum class ScanResult
        {
            NoMatch,         // no match
            Match,           // full match
            MatchTrimmedEOL, // partial match, terminated by line end
            MatchTrimmedEOF  // partial match, terminated by end of source
        };

        // handy checks for ScanResult value
        static constexpr bool Match(ScanResult result) noexcept { return result != ScanResult::NoMatch; }
        static constexpr bool NoMatch(ScanResult result) noexcept { return result == ScanResult::NoMatch; }

        // handy checks for SourceLength result (for check semantics comp.)
        static constexpr bool Match(SourceLength result) noexcept { return result != 0; }
        static constexpr bool NoMatch(SourceLength result) noexcept { return result == 0; }

        // handy conversion of bool to ScanResult
        //static constexpr ScanResult Match(bool result) noexcept { return result ? ScanResult::Match : ScanResult::NoMatch; }

        // handy type to designate comparison result
        typedef int ComparisonResult;

        // handy type to designate indexed search result type
        typedef int SearchResult;

        // search no match result value
        static constexpr int NO_MATCH = -1;

    protected:
        // props
        // ---------------------------------------------------------------------

        // iterator
        constexpr ScannerSourceIterator It() const { return p_it; }

        // props helpers
        constexpr char_t CharCurrent() const noexcept { return p_source.CharCurrent(p_it); }
        constexpr SourceLength LineCount() const noexcept { return p_it.Line(); }

        // checks if there's at least count characters before source end
        constexpr bool HasCharacters(const ScannerSourceIterator &it, SourceLength count) const noexcept
        {
            return count <= (p_source.Length() - it.Position());
        }

        // checks from ScannerSpecialCharChecker (checks at current position)
        constexpr bool IsSpace(const ScannerSourceIterator &it) const noexcept { return Tchecker::IsSpace(p_source, it); }
        constexpr SourceLength IsBreak(const ScannerSourceIterator &it) const noexcept { return Tchecker::IsBreak(p_source, it); }

        // helper function to convert SourceToken to token
        constexpr token_t SourceTokenToToken(const SourceToken &token) const noexcept
        {
            return p_source.SourceTokenToToken(token);
        }


        // helpers
        // ---------------------------------------------------------------------

        // helper functions to compare tokens of same or different character types
        template <typename T1, typename T2>
        static constexpr ComparisonResult CompareTokens(
            const Token<T1> &a, const Token<T2> &b,
            SourceSize limit = -1
        ) noexcept;

        template <typename T1, typename T2>
        static constexpr ComparisonResult CompareTokensCI(
            const Token<T1> &a, const Token<T2> &b,
            SourceSize limit = -1
        ) noexcept;

        // helper functions to help awsome c++ compiler with types

        // convert c array to FixedArray
        template <typename T, size_t length>
        static constexpr FixedArray<T> A(T(&items)[length]) noexcept { return FixedArray<T>(items); }

        // convert c character array to token
        template <typename T, size_t length>
        static constexpr Token<T> C(T(&items)[length]) noexcept { return Token<T>(items); }

        static constexpr Token<const char> C() noexcept { return Token<const char>(); }


        // SourceToken helpers
        // ---------------------------------------------------------------------

        // helper functions to check if token starts or ends with one of given sequences

        template <typename T>
        constexpr bool StartsWith(const SourceToken &token, const Token<T> &sequence) const noexcept;

        template <typename T>
        constexpr SearchResult StartsWith(const SourceToken &token, const FixedArray<const Token<T>> &sequences) const noexcept;

        template <typename T>
        constexpr bool EndsWith(const SourceToken &token, const Token<T> &sequence) const noexcept;

        template <typename T>
        constexpr SearchResult EndsWith(const SourceToken &token, const FixedArray<const Token<T>> &sequences) const noexcept;

        // helper functions to check already scanned token

        template <typename T>
        constexpr SearchResult TokenCheckAny(const SourceToken &token, const Token<T> &characters) const noexcept;

        template <typename T>
        constexpr SearchResult TokenCheckAny(const SourceToken &token, const FixedArray<const Token<T>> &sequences) const noexcept;

        template <typename T>
        constexpr SearchResult TokenCheckAnyCI(const SourceToken &token, const FixedArray<const Token<T>> &sequences) const noexcept;


        // basic checks
        // ---------------------------------------------------------------------

        // check that there's a match of a character or sequence at "it" position
        //      return 0 if none, length of the match if match found
        //      iterator is advanced by length of match on success

        template <typename T>
        constexpr SourceLength Check(T c, ScannerSourceIterator &it, bool advance = true) const noexcept;

        template <typename T>
        constexpr SourceLength Check(const Token<T> &s, ScannerSourceIterator &it, bool advance = true ) const noexcept;

        // following CheckAny functions are sort of high-level versions of Check to help
        // perform checks against multiple characters or sequences

        template <typename T>
        constexpr SourceLength CheckAny(const Token<T> &characters, ScannerSourceIterator &it, bool advance = true) const noexcept;

        template <typename T>
        constexpr SourceLength CheckAny(const FixedArray<const Token<T>> &compounds, ScannerSourceIterator &it, bool advance = true) const noexcept;


        // primitive scans
        // ---------------------------------------------------------------------

        // following are core scan functions which perform scans for common patterns
        // main difference from Check functions is optional account for line breaks and inner scan sequences
        //      Tinner template parameter designates "inner scan" lambda function
        //      (thanks to awesome language design where you can't declare something you
        //      can actually use)
        //      inner scan function has following signature:
        //          SourceLength (const ScannerSourceIterator &it)
        //      and it should return length of
        //      detected nested sequence (or 0 if there's no seqence detected) at given iterator

        template <typename Tinner>
        SourceLength GetCharToken(Tinner inner, ScannerSourceIterator &it, bool advance = true) const;

        // get or peek single character or inner sequence at given iterator
        // return length of detected char or inner sequence
        //      nextline - controls of how to deal with line breaks
        //          if true - function returns any detected line break sequence as ordinal character
        //          if false and it at line break - function returns 0 (thus, stopping at line break)
        //      advance - set to true if given iterator should be advanced by detected sequence length
        template <typename Tinner>
        SourceLength GetCharToken(bool nextline, Tinner inner, ScannerSourceIterator &it, bool advance = true) const;

        // get or peek single character in given set or inner sequence at given iterator
        // same as previous one, but checks single character against given character set if nothing else
        // matched (inner sequence)
        template <typename Tinner>
        SourceLength GetCharToken(const CharSet &set, Tinner inner, ScannerSourceIterator &it, bool advance = true) const;

        // matches all consequtive characters in whileset, linebreaks or inner sequences
        template <typename Tinner>
        SourceLength ContinueWhile(const CharSet &whileset, Tinner inner, ScannerSourceIterator &it) const;

        // matches everything up to line break or end of source
        SourceLength ContinueToEndOfLine(ScannerSourceIterator &it) const;

        // matches all characters up to totoken sequence
        //      fromtoken - sequence that was used as starting sequence
        //          it is used to control nested matches, if no nesting is required
        //          it can be empty
        //      totoken - token which terminates sequence
        //          if nesting is allowed there must be nestinglevel occurencies of this
        //          token in order to terminate the sequence
        //      nestinglevel - current nesting level (for incremental scans)
        //          must be at least 1
        //          with this version nesting is allowed and scan will be finished only on trim or
        //          nesting level reaching 0
        template <typename T, typename Tinner>
        ScanResult ContinueTo(
            const Token<T> &fromtoken, const Token<T> &totoken, bool multiline,
            Tinner inner, ScannerSourceIterator &it
        ) const;

        template <typename T, typename Tinner>
        ScanResult ContinueTo(
            const Token<T> &fromtoken, const Token<T> &totoken, bool multiline,
            Tinner inner, ScannerSourceIterator &it,
            int &nestinglevel
        ) const;

        // matches sequence starting with from set and continued with whileset
        // including all inner sequences
        template <typename Tinner>
        SourceLength FromSetWhile(
            const CharSet &from, const CharSet &whileset,
            Tinner inner, ScannerSourceIterator &it
        ) const;

        // matches sequence starting with from sequence and continued with whileset
        // icncluding all inner sequences
        //      multiline - when true line breaks are included
        //      notemptyehile - while part must not be empty, if this is true
        //      and only from sequence matched whole match will fail
        template <typename T, typename Tinner>
        SourceLength FromTokenWhile(
            const Token<T> &from, const CharSet &whileset,
            Tinner inner, bool notemptywhile, ScannerSourceIterator &it
        ) const;

        // same as above with multiple tokens to start from
        template <typename T, typename Tinner>
        SourceLength FromTokenWhile(
            const FixedArray<const Token<T>> &from, const CharSet &whileset,
            Tinner inner, bool notemptywhile, ScannerSourceIterator &it
        ) const;

        // match everything from given token till the end of line or source
        template <typename T>
        SourceLength FromToEndOfLine(const Token<T> &fromtoken, ScannerSourceIterator &it) const;

        // matches sequence surrounded by fromtoken and totoken tokes
        //      multiline - include line breaks
        //      WithNesting version - allow nesting of sequences
        //          FromTo { x { xxx } x } will match completely
        //          FromToWithNesting { x { xxx } x } will match as { x { xxx }
        template <typename T, typename Tinner>
        ScanResult FromTo(
            const Token<T> &fromtoken, const Token<T> &totoken, bool multiline,
            Tinner inner, ScannerSourceIterator &it
        ) const;

        template <typename T, typename Tinner>
        ScanResult FromToWithNesting(
            const Token<T> &fromtoken, const Token<T> &totoken, bool multiline,
            Tinner inner, ScannerSourceIterator &it
        ) const;


        // tokens and advancing
        // ---------------------------------------------------------------------

        // query token between two iterators
        static constexpr SourceToken PeekToken(const ScannerSourceIterator &a, const ScannerSourceIterator &b) noexcept
        {
            return ScannerSourceIterator::difftotoken(a, b);
        }

        // query token between current iterator and other iterator
        constexpr SourceToken PeekToken(const ScannerSourceIterator &b) const noexcept
        {
            return ScannerSourceIterator::difftotoken(p_it, b);
        }

        // query token and advance current iterator
        constexpr SourceToken ReadToken(const ScannerSourceIterator &b) noexcept
        {
            auto result = ScannerSourceIterator::difftotoken(p_it, b);
            p_it = b;
            return result;
        }

        constexpr void DiscardToken(const ScannerSourceIterator &b) noexcept
        {
            p_it = b;
        }

        constexpr bool SkipLineBreak(ScannerSourceIterator &it) const noexcept;

        // skip all whitespace (and optionally, line break) characters till start of the
        // next possible token
        //      advances current scanner iterator to the first potential token
        // returns true if there's a potential token found
        // false otherwise (end of source reached or line break when nextline is false)

        constexpr bool SkipToToken(ScannerSourceIterator &it, bool nextline = true) const noexcept;

    private:
        // template ugly "magic" for allowing to pass lambda and nullptr values
        // as callback parameters (for inner scans)
        template <typename Tc>
        struct caller
        {
            static constexpr SourceLength call(Tc callee, const ScannerSourceIterator &it) { return callee(it); }
        };

        template <>
        struct caller<decltype(nullptr)>
        {
            static constexpr SourceLength call(decltype(nullptr) callee, const ScannerSourceIterator &) { return 0; }
        };

    private:
        const Tsource         &p_source;
        ScannerSourceIterator  p_it;
    };


    template <typename Tsource>
    constexpr bool ScannerSpecialCharChecker<Tsource>::IsSpace(const Tsource &source, const ScannerSourceIterator &it) noexcept
    {
        // false if iterator at the end of the source
        if (it) {
            return false;
        }

        // get current character
        auto c = source.CharCurrent(it);

        // check if it's space character
        return
            c >= 0 && c <= ' ' &&   // default space character range
            c != '\r' && c != '\n'; // except new line characters!
    }

    template <typename Tsource>
    constexpr SourceLength ScannerSpecialCharChecker<Tsource>::IsBreak(const Tsource &source, const ScannerSourceIterator &it) noexcept
    {
        // no sequence if iterator at the end of the source
        if (it) {
            return 0;
        }

        // TODO: this should be refactored and checked for broken sequences
        //      basically there are 3 types of line end sequences
        //          \n  \r\n \n\r
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
        p_it(source)
    {}

    template <typename Tsource, typename Tchecker>
    constexpr Scanner<Tsource, Tchecker>::Scanner(const Tsource &source, const ScannerSourceIterator &start) noexcept :
        p_source(source),
        p_it(start)
    {}

    template <typename Tsource, typename Tchecker>
    template <typename T1, typename T2>
    constexpr typename Scanner<Tsource, Tchecker>::ComparisonResult
    Scanner<Tsource, Tchecker>::CompareTokens(const Token<T1> &a, const Token<T2> &b, SourceSize limit) noexcept
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
    template <typename T1, typename T2>
    constexpr typename Scanner<Tsource, Tchecker>::ComparisonResult
        Scanner<Tsource, Tchecker>::CompareTokensCI(const Token<T1> &a, const Token<T2> &b, SourceSize limit) noexcept
    {
        auto minlen = a.Length < b.Length ? a.Length : b.Length;

        if (limit >= 0 && minlen > limit) {
            minlen = limit;
        }

        auto pa = a.Text;
        auto pb = b.Text;

        // NOTE: this functions is meant to compare ASCII charset only
        auto upcase = [](int ch) {
            if (ch >= 'a' && ch <= 'z') {
                return ch - ('a' - 'A');
            }
            return ch;
        };

        while (minlen-- > 0) {
            // TODO: refine diff comparison
            auto diff = upcase(int(*pa++)) - upcase(int(*pb++));
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
    constexpr bool Scanner<Tsource, Tchecker>::StartsWith(const SourceToken &token, const Token<T> &sequence) const noexcept
    {
        auto tok = SourceTokenToToken(token);
        bool result =
            tok.Length >= sequence.Length &&
            CompareTokens(tok, sequence, sequence.Length) == 0;
        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    constexpr typename Scanner<Tsource, Tchecker>::SearchResult
    Scanner<Tsource, Tchecker>::StartsWith(const SourceToken &token, const FixedArray<const Token<T>> &sequences) const noexcept
    {
        auto found = std::find_if(
            sequences.begin(), sequences.end(),
            [&token, this](const auto &s) { return StartsWith(token, s); }
        );

        return found == sequences.end() ? NO_MATCH : int(found - sequences.begin());
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    constexpr bool Scanner<Tsource, Tchecker>::EndsWith(const SourceToken &token, const Token<T> &sequence) const noexcept
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
    constexpr typename Scanner<Tsource, Tchecker>::SearchResult
    Scanner<Tsource, Tchecker>::EndsWith(const SourceToken &token, const FixedArray<const Token<T>> &sequences) const noexcept
    {
        auto found = std::find_if(
            sequences.begin(), sequences.end(),
            [&token, this](const auto &s) { return EndsWith(token, s); }
        );

        return found == sequences.end() ? NO_MATCH : int(found - sequences.begin());
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    constexpr typename Scanner<Tsource, Tchecker>::SearchResult
    Scanner<Tsource, Tchecker>::TokenCheckAny(const SourceToken &token, const Token<T> &characters) const noexcept
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
    constexpr typename Scanner<Tsource, Tchecker>::SearchResult
    Scanner<Tsource, Tchecker>::TokenCheckAny(const SourceToken &token, const FixedArray<const Token<T>> &sequences) const noexcept
    {
        auto t = SourceTokenToToken(token);

        auto bound = std::lower_bound(
            sequences.begin(), sequences.end(), t,
            [](const auto &a, const auto &b) { return CompareTokens(a, b) < 0; }
        );

        if (bound != sequences.end() && CompareTokens(t, *bound) >= 0) {
            return int(bound - sequences.begin());
        }

        return NO_MATCH;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    constexpr typename Scanner<Tsource, Tchecker>::SearchResult
        Scanner<Tsource, Tchecker>::TokenCheckAnyCI(const SourceToken &token, const FixedArray<const Token<T>> &sequences) const noexcept
    {
        auto t = SourceTokenToToken(token);

        auto bound = std::lower_bound(
            sequences.begin(), sequences.end(), t,
            [](const auto &a, const auto &b) { return CompareTokensCI(a, b) < 0; }
        );

        if (bound != sequences.end() && CompareTokensCI(t, *bound) >= 0) {
            return int(bound - sequences.begin());
        }

        return NO_MATCH;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    constexpr SourceLength Scanner<Tsource, Tchecker>::Check(T c, ScannerSourceIterator &it, bool advance) const noexcept
    {
        bool result =
            !it &&
            Helpers::CharValue(p_source.CharCurrent(it)) == Helpers::CharValue(c);

        if (result) {
            if (advance) {
                it += 1;
            }
            return 1;
        }

        return 0;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    constexpr SourceLength Scanner<Tsource, Tchecker>::Check(const Token<T> &s, ScannerSourceIterator &it, bool advance) const noexcept
    {
        bool result =
            HasCharacters(it, s.Length) &&
            CompareTokens(
                s,
                p_source.SourceTokenToToken(SourceToken(it.Position(), s.Length))
            ) == 0;

        if (result) {
            if (advance) {
                it += s.Length;
            }
            return s.Length;
        }

        return 0;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    constexpr SourceLength Scanner<Tsource, Tchecker>::CheckAny(const Token<T> &characters, ScannerSourceIterator &it, bool advance) const noexcept
    {
        auto found = std::find_if(
            characters.begin(), characters.end(),
            [&it, advance, this](const auto c) { return Check(c, it, advance) != 0; }
        );

        // NOTE: iterator is advanced by inner Check call, captured by reference

        return found == characters.end() ? 0 : 1;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    constexpr SourceLength Scanner<Tsource, Tchecker>::CheckAny(const FixedArray<const Token<T>> &compounds, ScannerSourceIterator &it, bool advance) const noexcept
    {
        auto found = std::find_if(
            compounds.begin(), compounds.end(),
            [&it, advance, this](const auto &c) { return Check(c, it, advance) != 0; }
        );

        // NOTE: iterator is advanced by inner Check call, captured by reference

        return found == compounds.end() ? 0 : found->Length;
    }

    template <typename Tsource, typename Tchecker>
    template <typename Tinner>
    SourceLength Scanner<Tsource, Tchecker>::GetCharToken(Tinner inner, ScannerSourceIterator &it, bool advance) const
    {
        auto len = Tchecker::IsBreak(p_source, it);

        if (len > 0) {
            return 0;
        }

        len = caller<Tinner>::call(inner, it);
        if (len == 0 && !it) {
            len = 1;
        }

        if (advance) {
            it += len;
        }

        return len;
    }

    template <typename Tsource, typename Tchecker>
    template <typename Tinner>
    SourceLength Scanner<Tsource, Tchecker>::GetCharToken(bool nextline, Tinner inner, ScannerSourceIterator &it, bool advance) const
    {
        auto len = Tchecker::IsBreak(p_source, it);

        if (len == 0) {
            len = caller<Tinner>::call(inner, it);
            if (len == 0 && !it) {
                len = 1;
            }
        } else {
            if (nextline) {
                if (advance) {
                    it.AdvanceLine();
                }
            } else {
                len = 0;
            }
        }

        if (advance) {
            it += len;
        }

        return len;
    }

    template <typename Tsource, typename Tchecker>
    template <typename Tinner>
    SourceLength Scanner<Tsource, Tchecker>::GetCharToken(const CharSet &set, Tinner inner, ScannerSourceIterator &it, bool advance) const
    {
        auto result = GetCharToken(inner, it, false);

        if (result > 1 || result == 1 && set.in(p_source.CharCurrent(it))) {
            if (advance) {
                it += result;
            }
            return result;
        }

        return 0;
    }

    template <typename Tsource, typename Tchecker>
    SourceLength Scanner<Tsource, Tchecker>::ContinueToEndOfLine(ScannerSourceIterator &it) const
    {
        auto start = it;
        while (GetCharToken(nullptr, it)) {}
        return it - start;
    }

    template <typename Tsource, typename Tchecker>
    template <typename Tinner>
    SourceLength Scanner<Tsource, Tchecker>::ContinueWhile(const CharSet &whileset, Tinner inner, ScannerSourceIterator &it) const
    {
        auto start = it;
        while (GetCharToken(whileset, inner, it)) {}
        return it - start;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T, typename Tinner>
    typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::ContinueTo(const Token<T> &fromtoken, const Token<T> &totoken, bool multiline, Tinner inner, ScannerSourceIterator &it) const
    {
        auto result = ScanResult::NoMatch;

        while (auto adv = GetCharToken(multiline, inner, it, false)) {
            if (Check(totoken, it)) {
                result = ScanResult::Match;
                break;
            }

            it += adv;
        }

        if (result != ScanResult::Match) {
            result = it ? ScanResult::MatchTrimmedEOF : ScanResult::MatchTrimmedEOL;
        }

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T, typename Tinner>
    typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::ContinueTo(const Token<T> &fromtoken, const Token<T> &totoken, bool multiline, Tinner inner, ScannerSourceIterator &it, int &nestinglevel) const
    {
        assert(nestinglevel > 0);

        auto result = ScanResult::NoMatch;

        while (auto adv = GetCharToken(multiline, inner, it, false)) {
            if (Check(fromtoken, it)) {
                ++nestinglevel;
                continue;
            }

            if (Check(totoken, it)) {
                --nestinglevel;

                // NOTE: actually this should never reach negative value
                if (nestinglevel <= 0) {
                    result = ScanResult::Match;
                    break;
                }

                continue;
            }

            it += adv;
        }

        if (result != ScanResult::Match) {
            result = it ? ScanResult::MatchTrimmedEOF : ScanResult::MatchTrimmedEOL;
        }

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename Tinner>
    SourceLength Scanner<Tsource, Tchecker>::FromSetWhile(const CharSet &from, const CharSet &whileset, Tinner inner, ScannerSourceIterator &it) const
    {
        auto start = it;

        if (!GetCharToken(from, inner, it)) {
            return 0;
        }

        ContinueWhile(whileset, inner, it);

        return it - start;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T, typename Tinner>
    SourceLength Scanner<Tsource, Tchecker>::FromTokenWhile(const Token<T> &from, const CharSet &whileset, Tinner inner, bool notemptywhile, ScannerSourceIterator &it) const
    {
        auto current = it;

        if (!Check(from, current)) {
            return 0;
        }

        auto result = ContinueWhile(whileset, inner, current);
        if (notemptywhile && NoMatch(result)) {
            return 0;
        }

        result = current - it;
        it = current;

        return result;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T, typename Tinner>
    SourceLength Scanner<Tsource, Tchecker>::FromTokenWhile(const FixedArray<const Token<T>> &from, const CharSet &whileset, Tinner inner, bool notemptywhile, ScannerSourceIterator &it) const
    {
        for (auto &&f : from) {
            if (auto result = FromTokenWhile(f, whileset, inner, notemptywhile, it)) {
                return result;
            }
        }

        return 0;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T>
    SourceLength Scanner<Tsource, Tchecker>::FromToEndOfLine(const Token<T> &fromtoken, ScannerSourceIterator &it) const
    {
        auto start = it;

        if (!Check(fromtoken, it)) {
            return 0;
        }

        while (GetCharToken(nullptr, it)) {}

        return start - it;
    }

    template <typename Tsource, typename Tchecker>
    template <typename T, typename Tinner>
    typename Scanner<Tsource, Tchecker>::ScanResult
        Scanner<Tsource, Tchecker>::FromTo(const Token<T> &fromtoken, const Token<T> &totoken, bool multiline, Tinner inner, ScannerSourceIterator &it) const
    {
        if (!Check(fromtoken, it)) {
            return ScanResult::NoMatch;
        }

        return ContinueTo(fromtoken, totoken, multiline, inner, it);
    }

    template <typename Tsource, typename Tchecker>
    template <typename T, typename Tinner>
    typename Scanner<Tsource, Tchecker>::ScanResult
    Scanner<Tsource, Tchecker>::FromToWithNesting(const Token<T> &fromtoken, const Token<T> &totoken, bool multiline, Tinner inner, ScannerSourceIterator &it) const
    {
        if (!Check(fromtoken, it)) {
            return ScanResult::NoMatch;
        }

        int nestinglevel = 1;

        return ContinueTo(fromtoken, totoken, multiline, inner, it, nestinglevel);
    }

    template <typename Tsource, typename Tchecker>
    constexpr bool Scanner<Tsource, Tchecker>::SkipLineBreak(ScannerSourceIterator &it) const noexcept
    {
        auto brk = Tchecker::IsBreak(p_source, it);

        if (brk) {
            it += brk;
            it.AdvanceLine();
            return true;
        }

        // TODO: or should it return !it to return false only at source end?
        return false;
    }

    template <typename Tsource, typename Tchecker>
    constexpr bool Scanner<Tsource, Tchecker>::SkipToToken(ScannerSourceIterator &it, bool nextline) const noexcept
    {
        while (true) {
            if (Tchecker::IsSpace(p_source, it)) {
                it += 1;
            } else {
                auto br = Tchecker::IsBreak(p_source, it);

                if (br == 0) {
                    return !it;
                }

                if (nextline) {
                    it += br;
                    it.AdvanceLine();
                } else {
                    return false;
                }
            }
        }
    }

} // namespace k_scanner
