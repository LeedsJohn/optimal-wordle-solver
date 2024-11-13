#include "evaluator.h"
#include "constants.h"
#include "word.h"
#include <fstream>

static std::array<std::array<result, NUM_ANSWERS>, NUM_WORDS> evaluations_table;

Evaluator::Evaluator() {
    this->load_evaluations();
}

result Evaluator::evaluate(const Word guess, const Word answer) const {
    return evaluations_table[guess.get_index()][answer.get_index()];
}

// TODO: make sure this is only ever called once
void Evaluator::load_evaluations() {
    result r;
    std::ifstream evals;
    evals.open(EVALUATIONS_FILE, std::ios::binary);
    for (int guess = 0; guess < NUM_WORDS; ++guess) {
        for (int answer = 0; answer < NUM_ANSWERS; ++answer) {
            evals.read(reinterpret_cast<char*>(&r), sizeof(r));
            evaluations_table[guess][answer] = r;
        }
    }
    evals.close();
}

Evaluator evaluator;

