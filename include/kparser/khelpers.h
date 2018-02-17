/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017

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
        FixedArray() :
            p_items(nullptr),
            p_count(0)
        {}

        // initialize from static C array with knwon length
        template <size_t length>
        FixedArray(T(&items)[length]) :
            p_items(items),
            p_count(length)
        {}

        // initialize from items pointer and given item count
        FixedArray(T *items, size_t count) :
            p_items(items),
            p_count(count)
        {}

        // access by index operators
        T& operator[](size_t index) { return p_items[index]; }
        T& operator[](size_t index) const { return p_items[index]; }
        // get item count
        size_t count() const { return p_count; }

        // iterators
        T* begin() { return p_items; }
        T* end() { return p_items + p_count; }

        // const iterators
        T* begin() const { return p_items; }
        T* end() const { return p_items + p_count; }

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
        static CharType CharValue(T value)
        {
            return static_cast<CharType>(value);
        }

        // special case for signed char to prevent sign expansion
        template <>
        static CharType CharValue(char value)
        {
            return static_cast<CharType>(static_cast<unsigned char>(value));
        }

        // helper for comparison functions, trims comparison result to -1 0 +1
        //      NOTE: this is sort of copypasta from kcommon c_util.h, currently
        //            parser doesn't depend on other headers
        static int sign(int value)
        {
            if (value == 0) {
                return value;
            }

            return value > 0 ? +1 : -1;
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
        CharSet() :
            FixedArray<const CharRange>()
        {}

        // initialize from static C array with known length
        template <size_t length>
        CharSet(const CharRange(&items)[length]) :
            FixedArray<const CharRange>(items)
        {}

        // initialize from items pointer and given item count
        CharSet(const CharRange *items, size_t count) :
            FixedArray<const CharRange>(items, count)
        {}

        // check if given char value belongs to this set of characters
        template <typename T>
        inline bool in(T value) const;
    };


    template <typename T>
    bool CharSet::in(T value) const
    {
        // TODO: use binary search and force ordered ranges rule
        auto val = Helpers::CharValue(value);
        for (auto &&r : *this) {
            if (val >= r.left && val <= r.right) {
                return true;
            }
        }

        return false;
    }

} // namespace k_scanner
