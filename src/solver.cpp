// TODO: :%s/double/float/g
#include "answer_list.h"
#include "constants.h"
#include "evaluator.h"
#include "solver.h"
#include <algorithm>
#include <array>
#include <limits>
#include <tuple>
#include <unordered_map>

#include <iostream>

#define EXPLORATION_RATE 80


template<>
struct std::hash<Answer_list> {
    size_t operator()(const Answer_list& l) const {
        return l.get_hash();
    }
};

Word bad_word(~((unsigned short) 0));

std::array<int, 243> results;

int total_words_eliminated(const Answer_list& answers, Word guess, int beta) {
    results.fill(0);
    int N = answers.size();
    int total = N * N;
    for (Word answer : answers) {
        if (total < beta) {
            return 0;
        }
        result r = evaluator.evaluate(guess, answer);
        total -= N;
        total -= results[r] * (N - results[r]);
        results[r]++;
        total += results[r] * (N - results[r]);
        if (guess == answer) {
            total += 1;
        }
    }
    return total;
}

struct compare {
    inline bool operator() (const std::tuple<Word, size_t>& v1, const std::tuple<Word, size_t>& v2) {
        return std::get<1>(v1) > std::get<1>(v2);
    }
};

size_t get_min_index(const std::array<std::tuple<Word, size_t>, EXPLORATION_RATE>& guesses_to_try) {
    size_t res = 0;
    for (size_t i = 1; i < EXPLORATION_RATE; ++i) {
        if (std::get<1>(guesses_to_try[i]) < std::get<1>(guesses_to_try[res])) {
            res = i;
            if (std::get<1>(guesses_to_try[res]) == 0) {
                break;
            }
        }
    }
    return res;
}

void get_guesses(std::array<std::tuple<Word, size_t>, EXPLORATION_RATE>& guesses_to_try, const Answer_list& answers) {
    guesses_to_try.fill({bad_word, 0});
    size_t min_i = 0;
    for (unsigned short i = 0; i < NUM_WORDS; ++i) {
        Word w(i);
        size_t words_eliminated = total_words_eliminated(answers, w, std::get<1>(guesses_to_try[min_i]));
        if (words_eliminated <= std::get<1>(guesses_to_try[min_i])) {
            continue;
        }
        guesses_to_try[min_i] = {w, words_eliminated};
        // TODO: (optimization) switch to using a priority queue
        if (words_eliminated == (answers.size() - 1) * (answers.size() - 1) + answers.size()) {
            break;
        }
        min_i = get_min_index(guesses_to_try);
    }
    std::sort(guesses_to_try.begin(), guesses_to_try.end(), compare());
}

std::unordered_map<Answer_list, std::tuple<Word, double>, std::hash<Answer_list>, std::equal_to<Answer_list>> cache;
std::tuple<Word, double> get_guess_aux(const Answer_list& answers, int max_guesses, double best_average_guesses) {
    if (max_guesses == 0) {
        return {bad_word, std::numeric_limits<double>::infinity()};
    } else if (answers.size() == 1) {
        return {answers.get(), 1.0};
    } else if (answers.size() == 2) {
        if (max_guesses > 1) {
            return {answers.get(), 1.5};
        } else {
            return {bad_word, std::numeric_limits<double>::infinity()};
        }
    }
    auto lookup = cache.find(answers);
    if (lookup != cache.end()) {
        return lookup->second;
    }
    Word best_word = bad_word;
    std::array<std::tuple<Word, size_t>, EXPLORATION_RATE> guesses_to_try;
    get_guesses(guesses_to_try, answers);
    double dl = ((double) 1.0) / ((double) answers.size());
    for (int i = 0; i < EXPLORATION_RATE; ++i) {
        if (std::get<1>(guesses_to_try[i]) == 0) {
            break;
        }
        Word guess(std::get<0>(guesses_to_try[i]));
        double total = 1.0;
        for (Word answer : answers) {
            // TODO: Early exit here
            if (guess == answer) {
                // total += dl;
                continue;
            }
            const result res = evaluator.evaluate(guess, answer);
            std::tuple<Word, double> best = get_guess_aux(answers.filter(guess, res), max_guesses - 1, std::numeric_limits<double>::infinity());
            total += dl * std::get<1>(best);
        }
        double ev = total;
        if (ev < best_average_guesses) {
            best_average_guesses = ev;
            best_word = guess;
        }
    }
    std::tuple<Word, double> res = {best_word, best_average_guesses};
    cache.insert({answers, res});
    return res;
}

std::tuple<Word, double> get_guess(const Answer_list& answers, int max_guesses) {
    return get_guess_aux(answers, max_guesses, std::numeric_limits<double>::infinity());
}
