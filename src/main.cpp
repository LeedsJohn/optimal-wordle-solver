#include "answer_list.h"
#include "evaluator.h"
#include "make_evaluations.h"
#include "solver.h"
#include <iostream>
#include <string>
#include <tuple>


int play(const Word answer, bool verbose) {
    Answer_list answers(true);
    Word guess("salet");
    Answer_list stuff = answers.filter(guess, evaluator.evaluate(guess, answer));
    int i = 2;
    for (int j = 0; j < 6; ++j) {
        if (verbose) std::cout << "\n\nAnswers remaining: " << stuff.length << "\n";
        if (verbose && stuff.length <= 10) {
            stuff.print();
        }
        std::tuple<Word, int> res = get_guess(stuff, 5 - j);
        Word w = std::get<0>(res);
        if (w == answer) {
            if (verbose) std::cout << "Guessing " << w << "... got it in " << i << "\n";
            return i;
        }
        result r = evaluator.evaluate(w, answer);
        if (verbose) std::cout << "Guessing " << w << " Result: " << result_to_string(r) << "\n";
        stuff = stuff.filter(w, r);
        ++i;
    }
    std::cout << "FAILURE: " << answer << "\n";
    return 100;
}

void test_all() {
    int total = 0;
    for (unsigned short i = 0; i < NUM_ANSWERS; ++i) {
        if (i % 500 == 0) std::cout << i << "\n";
        total += play(Word(i), false);
    }
    std::cout << "Total: " << total << " Average: " << ((float) total) / ((float) NUM_ANSWERS) << "\n";
}

int main() {
    // TODO: CLI
    // make_evaluations();
    test_all();
}
