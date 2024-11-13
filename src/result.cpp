#include "result.h"
#include <cmath>
#include <string>

result result_of_string(const std::string& s) {
    result res = 0;
    for (int i = 0; i < 5; ++i) {
        if (s[4 - i] == 'G') {
            res += pow(3, i) * 2;
        } else if (s[4 - i] == 'Y') {
            res += pow(3, i);
        }
    }
    return res;
}

std::string result_to_string(result r) {
    std::string res = "XXXXX";
    for (int i = 0; i < 5; ++i) {
        if (r % 3 == 1) {
            res[4 - i] = 'Y';
        } else if (r % 3 == 2) {
            res[4 - i] = 'G';
        }
        r = r / 3;
    }
    return res;
}

