#ifndef RESULT_H
#define RESULT_H
#include <string>

#define ALL_GREEN (unsigned char) 242

// A result will be in the range [0, 243). In a Wordle game, if the answer is "earth"
// and you guess "plate", the result will be "XXYGY", which means that there is no "p"
// or "l" in "earth, there is an "a" and "e" in "earth" but not in the same position
// as in "plate," and the "t" is in the correct position.
// We assign these different possible results by interpreting them as base 3 numbers,
// where "X" = 0, "Y" = 1", and "G" = 2. So, this result would be represented as
// (3^0 * 1) + (3^1 * 2) + (3^2 * 1) + (3^3 * 0) + (3^4 * 0) = 18.
using result = unsigned char;

result result_of_string(const std::string& s);
std::string result_to_string(result r);

#endif

