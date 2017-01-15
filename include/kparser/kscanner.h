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
#include <vector>

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
        SourceToken() :
            Position(0),
            Length(0)
        {}

        SourceToken(SourcePosition position, SourceLength length = 0) :
            Position(position),
            Length(length)
        {}

        SourcePosition Position;
        SourceLength   Length;
    };


    // handy template class for passing static/dynamic arrays with known fixed size
    template <typename T>
    class FixedArray
    {
    public:
        FixedArray() :
            p_items(nullptr),
            p_count(0)
        {}

        template <size_t length>
        FixedArray(T (&items)[length]) :
            p_items(items),
            p_count(length)
        {}

        FixedArray(T *items, size_t count) :
            p_items(items),
            p_count(count)
        {}

        T& operator[](size_t index) { return p_items[index]; }
        T& operator[](size_t index) const { return p_items[index]; }
        size_t count() const { return p_count; }

        T* begin() { return p_items; }
        T* end() { return p_items + p_count; }

        T* begin() const { return p_items; }
        T* end() const { return p_items + p_count; }

    private:
        T      *p_items;
        size_t  p_count;
    };


    // token (sequence of characters, sort of string)
    template <typename T>
    struct Token
    {
        // construct empty token
        Token() :
            Text(nullptr),
            Length(0)
        {}

        // construct token from constant literal string
        template <size_t length>
        Token(const T (&text)[length]) :
            Text(text),
            Length(length - 1)
        {}

        // construct token from given text and length
        Token(const T *text, SourceSize length) :
            Text(text),
            Length(length)
        {}

        // construct token from given source text and SourceToken
        Token(const T *source, const SourceToken &token) :
            Text(source + token.Position),
            Length(token.Length)
        {}

        const T    *Text;
        SourceSize  Length;
    };


    // -------------------------------------------------------------------------------
    //  ScannerSource
    // -------------------------------------------------------------------------------
    //
    //  example class of scanner source interface

    template <typename T>
    class ScannerSource
    {
    public:
        // current position inside source text
        SourcePosition Position() const;
        // length of the whole source text
        SourceLength Length() const;

        // end of source indicator
        //      true when Position reached end of source (Position == Length)
        //      false otherwise
        bool IsEnd() const;

        // returns source character at current Position
        //      reading at invalid Position is not allowed
        //      Scanner won't read at invalid position
        T CharCurrent() const;
        // returns source character at specific offset form current position
        //      reading at invalid position is not allowed
        //      Scanner won't read at invalid position
        T CharAt(SourcePosition offset) const;

        // moves current Position by specified advance
        //      both negative and positive advances are allowed
        //      moving outside valid source text range is not allowed
        //      Scanner won't asvance to invalid position except position
        //      where IsEnd will return true
        void Advance(SourcePosition advance = 1);

        // converts source token to token
        Token<T> SourceTokenToToken(const SourceToken &token) const;
    };


    // -------------------------------------------------------------------------------
    //  ScannerStringSource
    // -------------------------------------------------------------------------------
    //
    //  basic scanner source from string implementation class

    template <typename T>
    class ScannerStringSource
    {
    public:
        ScannerStringSource(const T *source, SourceLength length) :
            p_source(source),
            p_position(0),
            p_length(length)
        {}

        SourcePosition Position() const { return p_position; }
        SourceLength Length() const { return p_length; }
        bool IsEnd() const { return p_position >= p_length; }

        T CharCurrent() const { return p_source[p_position]; }
        T CharAt(SourcePosition offset) const { return p_source[p_position + offset]; }

        void Advance(SourcePosition advance = 1)
        {
            p_position += advance;
        }

        Token<T> SourceTokenToToken(const SourceToken &token) const
        {
            return Token<T>(p_source + token.Position, token.Length);
        }

    private:
        const T        *p_source;
        SourcePosition  p_position;
        SourceLength    p_length;
    };


    // -------------------------------------------------------------------------------
    //  ScannerSpecialCharChecker
    // -------------------------------------------------------------------------------
    //
    //  basic scanner special characters checker class

    template <typename Tsource>
    class ScannerSpecialCharChecker
    {
    public:
        static inline bool IsSpace(const Tsource &source);
        static inline SourceLength IsBreak(const Tsource &source);
    };


    // -------------------------------------------------------------------------------
    //  Scanner
    // -------------------------------------------------------------------------------
    //
    //  basic scanner implementation class

    template <typename T, typename Tsource, typename Tchecker = ScannerSpecialCharChecker<Tsource>>
    class Scanner
    {
    public:
        typedef T char_t;
        typedef Tsource source_t;
        typedef Token<T> token_t;

    public:
        Scanner(Tsource &source);

    protected:
        enum ScanResult
        {
            srNoMatch,         // no match
            srMatch,           // full match
            srMatchTrimmedEOL, // partial match, terminated by line end
            srMatchTrimmedEOF  // partial match, terminated by end of source
        };

        // simple class for holding set of character ranges
        class CharSet
        {
        private:
            struct CharRange
            {
                T left;
                T right;
            };

        public:
            CharSet()
            {}

            CharSet(const CharSet &source) :
                p_ranges(source.p_ranges)
            {}

            void add(T single)
            {
                CharRange r = {
                    single, single
                };
                p_ranges.push_back(r);
            }

            void add(T left, T right)
            {
                CharRange r = {
                    left, right
                };
                p_ranges.push_back(r);
            }

            bool in(T value) const
            {
                for (auto r : p_ranges) {
                    if (value >= r.left && value <= r.right) {
                        return true;
                    }
                }

                return false;
            }

        private:
            std::vector<CharRange> p_ranges;
        };

    protected:
        static bool Match(ScanResult result) { return result != srNoMatch; }
        static bool NotMatch(ScanResult result) { return result == srNoMatch; }

        template <typename Tc>
        static ScanResult AnyMatch(SourceToken &token, Tc func)
        {
            return func(token);
        }

        template <typename Tc, typename... Args>
        static ScanResult AnyMatch(SourceToken &token, Tc first, Args... args)
        {
            auto result = first(token);
            if (result == srNoMatch) {
                result = AnyMatch(token, args...);
            }
            return result;
        }

        token_t SourceTokenToToken(const SourceToken &token) const
        {
            return p_source.SourceTokenToToken(token);
        }

        T CharCurrent() const { return p_source.CharCurrent(); }
        SourceLength LineCount() const { return p_lines; }

        bool HasCharacters(SourceLength count) const
        {
            return count < (p_source.Length() - p_source.Position());
        }

        bool SkipToToken(SourceToken &token, bool nextline = true);

        bool SkipToToken(bool nextline = true)
        {
            SourceToken token;
            return SkipToToken(token, nextline);
        }

        bool Check(T c, bool increment = true);
        bool Check(const token_t &s, bool increment = true);

        static const int NO_MATCH = -1;

        int CheckAny(const token_t &characters, bool increment = true);
        bool CheckAny(const token_t &characters, SourceToken &token, bool increment = true);
        int CheckAny(const FixedArray<token_t> &compounds, int &length, bool increment = true);
        bool CheckAny(const FixedArray<token_t> &compounds, SourceToken &token, bool increment = true);

        template <typename Tinner>
        bool GetCharToken(bool nextline, Tinner inner, SourceToken &token, bool increment = true);

        template <typename Tinner>
        bool CheckCharToken(const CharSet &set, bool nextline, Tinner inner, SourceToken &token, bool increment = true);

        template <typename Tinner>
        ScanResult ContinueWhile(const CharSet &whileset, bool multiline, Tinner inner, SourceToken &token, bool increment = true);

        template <typename Tinner>
        ScanResult ContinueTo(const token_t &fromtoken, const token_t &totoken, bool multiline, Tinner inner, bool allownesting, SourceToken &token, int &nestinglevel, bool increment = true);

        template <typename Tinner>
        ScanResult FromSetWhile(const CharSet &from, const CharSet &whileset, bool multiline, Tinner inner, SourceToken &token, bool increment = true);

        template <typename Tinner>
        ScanResult FromTokenWhile(const token_t &from, const CharSet &whileset, bool multiline, Tinner inner, bool notemptywhile, SourceToken &token, bool increment = true);

        template <typename Tinner>
        ScanResult FromTokenWhile(const FixedArray<token_t> from, const CharSet &whileset, bool multiline, Tinner inner, bool notemptywhile, SourceToken &token, bool increment = true);

        template <typename Tinner>
        ScanResult FromTo(const token_t &fromtoken, const token_t &totoken, bool multiline, Tinner inner, bool allownesting, SourceToken &token, bool increment = true);

    private:
        // template ugly "magic" for allowing to pass lambda and nullptr values
        // as callback parameters (for inner scans)
        template <typename Tc, typename dummy = void>
        struct caller
        {
            static inline bool assigned(Tc callee);
            static inline int call(Tc callee);
        };

        template <typename dummy>
        struct caller<nullptr_t, dummy>
        {
            static inline bool assigned(nullptr_t callee);
            static inline int call(nullptr_t callee);
        };

    private:
        Tsource      &p_source;
        SourceLength  p_lines;
    };


    template <typename Tsource>
    bool ScannerSpecialCharChecker<Tsource>::IsSpace(const Tsource &source)
    {
        if (source.IsEnd()) {
            return false;
        }

        auto c = source.CharCurrent();

        return c >= 0 && c <= ' ' && c != '\r' && c != '\n';
    }

    template <typename Tsource>
    SourceLength ScannerSpecialCharChecker<Tsource>::IsBreak(const Tsource &source)
    {
        if (source.IsEnd()) {
            return 0;
        }

        auto c = source.CharCurrent();
        SourceLength brk = 0;

        if (c == '\r') {
            ++brk;
            c = source.CharAt(brk);
        }

        if (c == '\n') {
            ++brk;
        }

        return brk;
    }


    template <typename T, typename Tsource, typename Tchecker>
    Scanner<T, Tsource, Tchecker>::Scanner(Tsource &source) :
        p_source(source),
        p_lines(0)
    { }

    template <typename T, typename Tsource, typename Tchecker>
    bool Scanner<T, Tsource, Tchecker>::SkipToToken(SourceToken &token, bool nextline)
    {
        token = SourceToken(p_source.Position());

        while (true) {
            if (Tchecker::IsSpace(p_source)) {
                p_source.Advance();
                ++token.Length;
            } else {
                auto br = Tchecker::IsBreak(p_source);

                if (br == 0) {
                    return !p_source.IsEnd();
                }

                if (nextline) {
                    p_source.Advance(br);
                    token.Length += br;
                    ++p_lines;
                } else {
                    return false;
                }
            }
        }
    }

    template <typename T, typename Tsource, typename Tchecker>
    bool Scanner<T, Tsource, Tchecker>::Check(T c, bool increment)
    {
        bool result = !p_source.IsEnd() && p_source.CharCurrent() == c;

        if (result && increment) {
            p_source.Advance();
        }

        return result;
    }

    template <typename T, typename Tsource, typename Tchecker>
    bool Scanner<T, Tsource, Tchecker>::Check(const token_t &s, bool increment)
    {
        bool result =
            HasCharacters(s.Length) &&
            memcmp(
                s.Text,
                p_source.SourceTokenToToken(SourceToken(p_source.Position(), s.Length)).Text,
                s.Length * sizeof(T)
            ) == 0;

        if (result && increment) {
            p_source.Advance(s.Length);
        }

        return result;
    }

    template <typename T, typename Tsource, typename Tchecker>
    int Scanner<T, Tsource, Tchecker>::CheckAny(const token_t &characters, bool increment)
    {
        for (auto n = 0; n < characters.Length; ++n) {
            if (Check(characters.Text[n], increment)) {
                return n;
            }
        }

        return NO_MATCH;
    }

    template <typename T, typename Tsource, typename Tchecker>
    bool Scanner<T, Tsource, Tchecker>::CheckAny(const token_t &characters, SourceToken &token, bool increment)
    {
        auto pos = p_source.Position();
        auto result = CheckAny(characters, increment) != NO_MATCH;

        token = SourceToken(pos, result ? 1 : 0);

        return result;
    }

    template <typename T, typename Tsource, typename Tchecker>
    int Scanner<T, Tsource, Tchecker>::CheckAny(const FixedArray<token_t> &compounds, int &length, bool increment)
    {
        length = 0;

        for (size_t n = 0; n < compounds.count(); ++n) {
            auto &c = compounds[n];
            if (Check(c, increment)) {
                length = c.Length;
                return n;
            }
        }

        return NO_MATCH;
    }

    template <typename T, typename Tsource, typename Tchecker>
    bool Scanner<T, Tsource, Tchecker>::CheckAny(const FixedArray<token_t> &compounds, SourceToken &token, bool increment)
    {
        auto length = 0;
        auto pos = p_source.Position();

        bool result = CheckAny(compounds, length, increment) != NO_MATCH;

        token = SourceToken(pos, length);

        return result;
    }

    template <typename T, typename Tsource, typename Tchecker>
    template <typename Tinner>
    bool Scanner<T, Tsource, Tchecker>::GetCharToken(bool nextline, Tinner inner, SourceToken &token, bool increment)
    {
        token = SourceToken(p_source.Position(), 0);

        auto result = false;
        auto len = Tchecker::IsBreak(p_source);

        if (len == 0) {
            if (caller<Tinner>::assigned(inner)) {
                len = caller<Tinner>::call(inner);
            }
            if (len == 0 && p_source.IsEnd()) {
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
            p_source.Advance(token.Length);
        }

        return result;
    }

    template <typename T, typename Tsource, typename Tchecker>
    template <typename Tinner>
    bool Scanner<T, Tsource, Tchecker>::CheckCharToken(const CharSet &set, bool nextline, Tinner inner, SourceToken &token, bool increment)
    {
        auto result =
            GetCharToken(nextline, inner, token, false) &&
            (token.Length > 1 || set.in(p_source.CharCurrent()));

        if (result && increment) {
            p_source.Advance(token.Length);
        }

        return result;
    }

    template <typename T, typename Tsource, typename Tchecker>
    template <typename Tinner>
    typename Scanner<T, Tsource, Tchecker>::ScanResult Scanner<T, Tsource, Tchecker>::ContinueWhile(const CharSet &whileset, bool multiline, Tinner inner, SourceToken &token, bool increment)
    {
        token = SourceToken(p_source.Position());

        SourceToken cs;
        while (CheckCharToken(whileset, multiline, inner, cs)) {
            token.Length += cs.Length;
        }

        if (!increment) {
            p_source.Advance(-token.Length);
        }

        return srMatch;
    }

    template <typename T, typename Tsource, typename Tchecker>
    template <typename Tinner>
    typename Scanner<T, Tsource, Tchecker>::ScanResult Scanner<T, Tsource, Tchecker>::ContinueTo(const token_t &fromtoken, const token_t &totoken, bool multiline, Tinner inner, bool allownesting, SourceToken &token, int &nestinglevel, bool increment)
    {
        token = SourceToken(p_source.Position());

        auto result = srNoMatch;

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
                    result = srMatch;
                    break;
                }

                continue;
            }

            p_source.Advance(cs.Length);
            token.Length += cs.Length;
        }

        if (result != srMatch)
        {
            if (p_source.IsEnd()) {
                result = srMatchTrimmedEOF;
            } else {
                result = srMatchTrimmedEOL;
            }
        }

        if (!increment) {
            p_source.Advance(-token.Length);
        }

        return result;
    }

    template <typename T, typename Tsource, typename Tchecker>
    template <typename Tinner>
    typename Scanner<T, Tsource, Tchecker>::ScanResult Scanner<T, Tsource, Tchecker>::FromSetWhile(const CharSet &from, const CharSet &whileset, bool multiline, Tinner inner, SourceToken &token, bool increment)
    {
        if (!CheckCharToken(from, multiline, inner, token)) {
            return srNoMatch;
        }

        SourceToken cs;
        auto result = ContinueWhile(whileset, multiline, inner, cs);
        token.Length += cs.Length;

        if (!increment) {
            p_source.Advance(-token.Length);
        }

        return result;
    }

    template <typename T, typename Tsource, typename Tchecker>
    template <typename Tinner>
    typename Scanner<T, Tsource, Tchecker>::ScanResult Scanner<T, Tsource, Tchecker>::FromTokenWhile(const token_t &from, const CharSet &whileset, bool multiline, Tinner inner, bool notemptywhile, SourceToken &token, bool increment)
    {
        token = SourceToken(p_source.Position());

        if (!Check(from)) {
            return srNoMatch;
        }

        token.Length += from.Length;

        SourceToken cs;
        auto result = ContinueWhile(whileset, multiline, inner, cs);
        token.Length += cs.Length;

        if (notemptywhile && token.Length <= from.Length) {
            p_source.Advance(-token.Length);
            return srNoMatch;
        }

        if (!increment) {
            p_source.Advance(-token.Length);
        }

        return result;
    }

    template <typename T, typename Tsource, typename Tchecker>
    template <typename Tinner>
    typename Scanner<T, Tsource, Tchecker>::ScanResult Scanner<T, Tsource, Tchecker>::FromTokenWhile(const FixedArray<token_t> from, const CharSet &whileset, bool multiline, Tinner inner, bool notemptywhile, SourceToken &token, bool increment)
    {
        auto result = srNoMatch;

        for (auto &&f : from) {
            result = FromTokenWhile(
                f, whileset, multiline, inner,
                notemptywhile, token, increment
            );

            if (result != srNoMatch) {
                return result;
            }
        }

        return result;
    }

    template <typename T, typename Tsource, typename Tchecker>
    template <typename Tinner>
    typename Scanner<T, Tsource, Tchecker>::ScanResult Scanner<T, Tsource, Tchecker>::FromTo(const token_t &fromtoken, const token_t &totoken, bool multiline, Tinner inner, bool allownesting, SourceToken &token, bool increment)
    {
        token = SourceToken(p_source.Position());

        if (!Check(fromtoken)) {
            return srNoMatch;
        }

        token.Length += fromtoken.Length;

        int nestinglevel = 1;

        SourceToken cs;
        auto result = ContinueTo(fromtoken, totoken, multiline, inner, allownesting, cs, nestinglevel);
        token.Length += cs.Length;

        if (!increment) {
            p_source.Advance(-token.Length);
        }

        return result;
    }

    template <typename T, typename Tsource, typename Tchecker>
    template <typename dummy>
    bool Scanner<T, Tsource, Tchecker>::caller<nullptr_t, dummy>::assigned(nullptr_t)
    {
        return false;
    }

    template <typename T, typename Tsource, typename Tchecker>
    template <typename dummy>
    int Scanner<T, Tsource, Tchecker>::caller<nullptr_t, dummy>::call(nullptr_t)
    {
        return 0;
    }

    template <typename T, typename Tsource, typename Tchecker>
    template <typename Tc, typename dummy>
    bool Scanner<T, Tsource, Tchecker>::caller<Tc, dummy>::assigned(Tc callee)
    {
        return true;
    }

    template <typename T, typename Tsource, typename Tchecker>
    template <typename Tc, typename dummy>
    int Scanner<T, Tsource, Tchecker>::caller<Tc, dummy>::call(Tc callee)
    {
        return callee();
    }
} // namespace k_scanner
