#ifndef WORD_LIST_H
#define WORD_LIST_H
#include "constants.h"
#include "evaluator.h"
#include "result.h"
#include "word.h"
#include <array>
#include <vector>

// #define MAX_LENGTH 320

// Holds all words that are still possible answers.
class Answer_list {
    public:
        /** Default constructor
         *
         * If all is true, this means that all possible answers are still in play. This
         * is special cased so we don't need to store all numbers in [0, NUM_ANSWERS).
         *
         * @param all whether the answer list contains all possible answers.
         */
        Answer_list(bool all = false);

        void append(const Word w);

        /** Create a new filtered Answer_list based off of the results of a guess and 
         * the true answer.
         *
         * @param guess  The word that was guessed.
         * @param result The result of evaluating "guess" with the given answer.
         */
        Answer_list filter(const Word guess, const result res) const;

        void make_hash();

        int length;
        size_t hash;
        // std::array<unsigned short, MAX_LENGTH> answers;
        std::vector<unsigned short> answers;

        bool operator==(const Answer_list& other) const {
            if (this->hash != other.hash || this->answers.size() != other.answers.size()) {
                return false;
            }
            for (size_t i = 0; i < this->answers.size(); ++i) {
                if (this->answers[i] != other.answers[i]) {
                    return false;
                }
            }
            return true;

            // if (this->idx != other.idx) {
            //     return false;
            // }
            // for (int i = 0; i <= this->idx; ++i) {
            //     if (this->answers[i] != other.answers[i]) {
            //         return false;
            //     }
            // }
            // return true;
        }

        // TOOD: REMOVE
        bool contains(const Word w) const;
        void print() const;

        class Iterator {
            public:

                Iterator(const Answer_list& answers, int ptr, Word val) : ptr(ptr), val(val), answers(answers) {};

                const Word& operator*() const { return this->val; }
                Iterator& operator++() {
                    if (this->val.get_index() == this->answers.answers[this->ptr]) {
                        this->ptr += 2;
                        if (this->ptr > this->answers.idx) {
                            this->val = Word(~0);
                        } else {
                            this->val = Word(this->answers.answers[this->ptr - 1]);
                        }
                    } else {
                        this->val = Word(this->val.get_index() + 1);
                    }
                    return *this;
                }
                Iterator operator++(int) { Iterator tmp = *this; ++(*this); return tmp; }
                friend bool operator== (const Iterator& a, const Iterator& b) { return a.ptr == b.ptr && a.val == b.val; };
                friend bool operator!= (const Iterator& a, const Iterator& b) { return !(a == b); };  


            private:
                int ptr;
                Word val;
                const Answer_list& answers;
        };

        Iterator begin() const { return Iterator(*this, 1, Word(this->answers[0])); }
        Iterator end() const { return Iterator(*this, this->idx + 2, Word(~0)); }

    private:
        int idx;
};

#endif
