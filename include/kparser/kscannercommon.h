/*
        KPARSER PROJECT

    Utilities library for parsers programming

    (c) livingcreative, 2017 - 2021

    https://github.com/livingcreative/kparser

    kscannercommon.h
        common scanner declaration
*/

#pragma once

#include "kscanner.h"


namespace k_parser
{
    class ScannerCommonDefs
    {
    protected:
        using CharRange = k_parser::CharRange;
        using TokenChar = TokenT<char>;

        static const CharRange p_numeric[1];     // numeric [0 - 9] characters set
        static const CharRange p_hexadecimal[3]; // hexadecimal [0 - 9, A - F, a - f] characters set
        static const CharRange p_alpha[4];       // alpha characters set (not exact, unicode range needs refinement)
        static const CharRange p_alphanum[5];    // alpha + numeric characters set
    };
}
