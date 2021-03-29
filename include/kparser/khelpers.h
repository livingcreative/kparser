/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017 - 2021

    https://github.com/livingcreative/kparser

    khelpers.h
        common utility types and functions for parser
*/

#pragma once

namespace k_parser
{

    // handy template class for passing static/dynamic arrays with known fixed size
    //      T - type for array element
    template <typename T>
    class FixedArray
    {
    public:
        // initialize empty array
        constexpr FixedArray() noexcept :
            p_items(nullptr),
            p_count(0)
        {}

        // initialize from static C array with knwon length
        template <size_t length>
        constexpr FixedArray(T(&items)[length]) noexcept :
            p_items(items),
            p_count(length)
        {}

        // initialize from items pointer and given item count
        constexpr FixedArray(T *items, size_t count) noexcept :
            p_items(items),
            p_count(count)
        {}

        // access by index operators
        constexpr T& operator[](size_t index) noexcept { return p_items[index]; }
        constexpr T& operator[](size_t index) const noexcept { return p_items[index]; }
        // get item count
        constexpr size_t count() const noexcept { return p_count; }

        // iterators
        constexpr T* begin() noexcept { return p_items; }
        constexpr T* end() noexcept { return p_items + p_count; }

        // const iterators
        constexpr T* begin() const noexcept { return p_items; }
        constexpr T* end() const noexcept { return p_items + p_count; }

    private:
        T      *p_items; // pointer to first item
        size_t  p_count; // item count
    };


    // type for holding any character value
    // (should have enough range to hold values of different char types)
    typedef unsigned int CharType;


    // holds static helper functions
    class Helpers
    {
    public:
        // helper function to convert given char value to CharType value
        template <typename T>
        static constexpr CharType CharValue(T value) noexcept
        {
            return static_cast<CharType>(value);
        }

        // special case for signed char to prevent sign expansion
        template <>
        static constexpr CharType CharValue(char value) noexcept
        {
            return static_cast<CharType>(static_cast<unsigned char>(value));
        }
    };


    // character range structure
    // values have independent type and can hold any value of char, whcar_t, etc
    //      TODO: improve CharRange
    struct CharRange
    {
        CharType left;
        CharType right;
    };

    // simple class for holding set of character ranges
    class CharSet : public FixedArray<const CharRange>
    {
    public:
        // empty character set
        constexpr CharSet() noexcept :
            FixedArray<const CharRange>()
        {}

        // initialize from static C array with known length
        template <size_t length>
        constexpr CharSet(const CharRange(&items)[length]) noexcept :
            FixedArray<const CharRange>(items)
        {}

        // initialize from items pointer and given item count
        constexpr CharSet(const CharRange *items, size_t count) noexcept :
            FixedArray<const CharRange>(items, count)
        {}

        // check if given char value belongs to this set of characters
        template <typename T>
        constexpr bool in(T value) const noexcept;
    };


    template <typename T>
    constexpr bool CharSet::in(T value) const noexcept
    {
        auto val = Helpers::CharValue(value);
        for (auto &&r : *this) {
            if (val >= r.left && val <= r.right) {
                return true;
            }
        }

        return false;
    }

} // namespace k_scanner
