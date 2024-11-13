#include "answer_list.h"
#include "evaluator.h"
#include "result.h"
#include "word.h"
#include <array>
#include <vector>


#include <iostream>

/**
 * This class stores the possible answers as ranges of numbers. This is to save memory
 * so that we don't need to save 2,000+ entries for an answer list. While untested,
 * I also think that there will tend to be chunks of words that remain alive in any
 * answer list because WORDS_FILE is alphabetized.
 * 
 * If we create an answer list with the numbers 1, 5, 6, 7, 10, 11, 12, 13, 14, this
 * would be stored as [1, 1, 5, 7, 10, 14] - we have the numbers 1 through 1, 5 through
 * 7, and 10 through 14.
 */
Answer_list::Answer_list(bool all) {
    this->hash_computed = false;
    if (all) {
        this->size_ = NUM_ANSWERS;
        this->answers.push_back(0);
        this->answers.push_back(NUM_ANSWERS - 1);
        return;
    }
    this->size_ = 0;
}

void Answer_list::append(const Word w) {
    this->hash_computed = false;
    this->size_++;
    unsigned short i = w.get_index();
    if (this->answers.size() == 0 || this->answers.back() + 1 != i) {
        this->answers.push_back(i);
        this->answers.push_back(i);
        return;
    }
    this->answers.back()++;
}

Answer_list Answer_list::filter(const Word guess, const result res) const {
    Answer_list new_list = Answer_list();
    for (Word answer : *this)  {
        result answer_res = evaluator.evaluate(guess, answer);
        if (answer_res == res) {
            new_list.append(answer);
        }
    }
    return new_list;
}

size_t Answer_list::get_hash() const {
    if (this->hash_computed) {
        return this->hash;
    }
    std::hash<unsigned short> hasher;
    size_t combined_hash = 0;
    for (unsigned short n : this->answers) {
        combined_hash ^= hasher(n) + 0x9e3779b9 + (combined_hash << 6) + (combined_hash >> 2);
    }
    this->hash = combined_hash;
    this->hash_computed = true;
    return this->hash;
}

bool Answer_list::contains(const Word w) const {
    for (Word word : *this) {
        if (word == w) return true;
    }
    return false;
}
