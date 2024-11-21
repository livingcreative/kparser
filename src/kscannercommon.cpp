#include "kparser/kscannercommon.h"

using namespace k_parser;


const CharRange ScannerCommonDefs::p_numeric[] = {
    { '0', '9' }
};

const CharRange ScannerCommonDefs::p_hexadecimal[] = {
    { '0', '9' },
    { 'A', 'F' },
    { 'a', 'f' }
};

const CharRange ScannerCommonDefs::p_alpha[] = {
    { '_', '_' },
    { 'A', 'Z' },
    { 'a', 'z' },
    // TODO: refine alpha range
    { L'\x0100', L'\xFFFF' }
};

const CharRange ScannerCommonDefs::p_alphanum[] = {
    { '_', '_' },
    { 'A', 'Z' },
    { 'a', 'z' },
    { L'\x0100', L'\xFFFF' },
    { '0', '9' }
};
