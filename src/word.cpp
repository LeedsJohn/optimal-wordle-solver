#include "constants.h"
#include "word.h"
#include <fstream>
#include <string>

Word::Word(const std::string& s) {
    unsigned short i = 0;
    std::string word;
    std::ifstream guesses;
    guesses.open(WORDS_FILE);
    this->index = -1;
    while (std::getline(guesses, word)) {
        if (word == s) {
            this->index = i;
            break;
        }
        ++i;
    }
    guesses.close();
}

std::string Word::to_string() const {
    std::ifstream guesses;
    std::string w;
    guesses.open(WORDS_FILE);
    // each line is 6 characters (5 characters + newline)
    guesses.seekg(this->index * 6);
    guesses >> w;
    guesses.close();
    return w;
}

