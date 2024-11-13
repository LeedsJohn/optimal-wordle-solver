#ifndef WORD_LIST_H
#define WORD_LIST_H
#include "constants.h"
#include "evaluator.h"
#include "result.h"
#include "word.h"
#include <array>
#include <vector>

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

        // The index of w MUST be greater than the previous greatest word index in
        // the answer list.
        void append(const Word w);

        /** Create a new filtered Answer_list based off of the results of a guess and 
         * the true answer.
         *
         * @param guess  The word that was guessed.
         * @param result The result of evaluating "guess" with the given answer.
         */
        Answer_list filter(const Word guess, const result res) const;

        // Return an arbitrary word from the Answer List.
        Word get() const { return this->answers[0]; }

        // Lazily initializes hash if necessary.
        size_t get_hash() const;

        size_t size() const { return this->size_; };

        std::vector<unsigned short> answers;

        bool operator==(const Answer_list& other) const {
            if (this->get_hash() != other.get_hash() || this->answers.size() != other.answers.size()) {
                return false;
            }
            return std::equal(this->answers.begin(), this->answers.end(), other.answers.begin());
        }

        friend std::ostream& operator<<(std::ostream & os, const Answer_list& answers) {
            if (answers.size() > 10) {
                os << "Answer list with length " << answers.size() << "\n";
            } else {
                for (Word w : answers) {
                    os << w << "\n";
                }
            }
            return os;
        }

        bool contains(const Word w) const;

        class Iterator {
            public:

                Iterator(const Answer_list& a, int p, Word v) : ptr(p), val(v), answers(a) {};

                const Word& operator*() const { return this->val; }
                Iterator& operator++() {
                    if (this->val.get_index() == this->answers.answers[this->ptr]) {
                        this->ptr += 2;
                        if (this->ptr >= this->answers.answers.size()) {
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
                size_t ptr;
                Word val;
                const Answer_list& answers;
        };

        Iterator begin() const { return Iterator(*this, 1, Word(this->answers[0])); }
        Iterator end() const { return Iterator(*this, this->answers.size() + 1, Word(~0)); }

    private:
        mutable size_t hash;
        mutable bool hash_computed;
        size_t size_;
};

#endif
