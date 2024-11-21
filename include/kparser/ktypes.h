#pragma once

#include <memory>


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


    // token (sequence of characters, sort of string)
    //      T - type for character, like char or wchar_t
    template <typename T>
    struct TokenT
    {
        // construct empty token
        constexpr TokenT() noexcept :
            Text(nullptr),
            Length(0)
        {}

        // construct token from constant literal string
        template <size_t length>
        constexpr TokenT(const T(&text)[length]) noexcept :
            Text(text),
            Length(length - 1) // NOTE: C strings have implicit 0 at the end,
                               // it's not included in length
        {}

        // construct token from given text and length
        constexpr TokenT(const T *text, unsigned length) noexcept :
            Text(text),
            Length(length)
        {}

        // construct token from given source text and SourceToken
        constexpr TokenT(const T *source, const SourceToken &token) noexcept :
            Text(source + token.Position),
            Length(token.Length)
        {}

        // const iterators
        constexpr const T *begin() const noexcept { return Text; }
        constexpr const T *end() const noexcept { return Text + Length; }

        constexpr operator bool() const noexcept { return Length > 0; }

        constexpr bool operator==(const TokenT<T> &other) const noexcept
        {
            return
                Length == other.Length && (
                    Text == other.Text ||
                    memcmp(Text, other.Text, Length * sizeof(T)) == 0
                );
        }

        const T  *Text;   // pointer to first token character
        unsigned  Length; // character count
    };


    template <typename T>
    class TokenStringT
    {
    public:
        TokenStringT() :
            p_data(nullptr),
            p_length(0)
        {}

        TokenStringT(const TokenT<T> &source) :
            p_data(new T[source.Length]),
            p_length(source.Length)
        {
            memcpy(p_data, source.Text, p_length * sizeof(T));
        }

        TokenStringT(const TokenStringT<T> &source) :
            TokenStringT(TokenT<T>(source.p_data, source.p_length))
        {}

        TokenStringT(TokenStringT<T> &&source) :
            p_data(source.p_data),
            p_length(source.p_length)
        {
            source.p_data = nullptr;
            source.p_length = 0;
        }

        ~TokenStringT()
        {
            delete[] p_data;
        }


        TokenStringT<T> &operator=(const TokenStringT<T> &source)
        {
            auto copy = TokenStringT<T>(source.p_data);
            p_data = copy.p_data;
            p_length = copy.p_length;
            copy.p_data = nullptr;
            return *this;
        }

        TokenStringT<T> &operator=(TokenStringT<T> &&source)
        {
            p_data = source.p_data;
            p_length = source.p_length;
            source.p_data = nullptr;
            source.p_length = 0;
            return *this;
        }


        const T *data() const { return p_data; }
        size_t length() const { return p_length; }

        const T *begin() const { return p_data; }
        const T *end() const { return p_data + p_length; }

        T operator[](size_t index) const { return p_data[index]; }

        operator TokenT<T>() const { return { p_data, p_length }; }

    private:
        T        *p_data;
        unsigned  p_length;
    };
}
