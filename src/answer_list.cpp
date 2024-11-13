#include "answer_list.h"
#include "evaluator.h"
#include "result.h"
#include "word.h"
#include <array>
#include <vector>


#include <iostream>


// TODO: represent this as an array where the even indices say how many valid words are
// still alive in a row and the odd indices say how many eliminated words.
// So for example 0, 1, 7, 8, 9, 10 would be [2, 5, 4]
// TODO: I think this will basically just go with stack depth so I can assign this->idx
// to the number of currently created lists instead of looping through all the possibilities
// TODO: make a real iterator instead of this scuffed thing
Answer_list::Answer_list(bool all) {
    this->hash_computed = false;
    if (all) {
        this->length = NUM_ANSWERS;
        this->idx = 1;
        this->answers.push_back(0);
        this->answers.push_back(NUM_ANSWERS - 1);
        return;
    }
    this->idx = -1;
    this->length = 0;
}

void Answer_list::append(const Word w) {
    this->computed_hash = false;
    this->length++;
    unsigned short i = w.get_index();
    if (this->idx == -1 || this->answers[this->idx] + 1 != i) {
        this->answers.push_back(i);
        this->answers.push_back(i);
        this->idx += 2;
        return;
    }
    this->answers[this->idx]++;
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
    for (int i = 0; i <= this->idx; ++i) {
        combined_hash ^= hasher(this->answers[i]) + 0x9e3779b9 + (combined_hash << 6) + (combined_hash >> 2);
    }
    this->hash = combined_hash;
    this->hash_computed = true;
    return this->hash;
}

// TODO: Remove
bool Answer_list::contains(const Word w) const {
    for (Word word : *this) {
        if (word == w) return true;
    }
    return false;
}

// TODO: Remove
void Answer_list::print() const {
    for (Word word : *this) {
        std::cout << word << "\n";
    }
}

