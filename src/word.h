#ifndef WORD_H
#define WORD_H
#include "constants.h"
#include <string>


// TODO: Remove
#include <iostream>

/**
 * Represents one 5 letter word.
 *
 * Because this type is dealt with in great quantities and very frequently, we represent
 * it using an index into a file. Unless a human is inspecting a word, we never need to
 * know what the word actually is. For example, "aback" is word 0 because it is the
 * first word in the words list.
 *
 * One other quirk of this representation is that we store all answers first (so, a
 * Word that can be a possible answer will have a lower index than a word that you can
 * guess but cannot be a possible answer).
 */
class Word {
    public:
        Word() {};
        Word(unsigned short i) { this->index = i; }

        // Must exist in WORDS_FILE
        Word(const std::string& s);
        std::string to_string() const;
        bool can_be_answer() const { return this->index + 1 < NUM_ANSWERS; }
        unsigned short get_index() const { return this->index; }

        bool operator==(const Word other) const { return other.index == this->index; };

        // TODO: Remove
        friend std::ostream& operator<<(std::ostream& os, const Word& w) {
            os << w.to_string() << " (" << w.get_index() << ")";
            return os;
        };

    private:
        // index of word in guesses file
        unsigned short index;
};

#endif
