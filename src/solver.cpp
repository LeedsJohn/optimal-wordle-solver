// TODO: :%s/double/float/g
#include "answer_list.h"
#include "constants.h"
#include "evaluator.h"
#include "solver.h"
#include <array>
#include <tuple>
#include <unordered_map>

#include <iostream>

#define MAX_TOTAL_GUESSES std::numeric_limits<double>::infinity()
#define EXPLORATION_RATE 80


template<>
struct std::hash<Answer_list> {
    size_t operator()(const Answer_list& l) const {
        return l.hash;
    }
};

Word bad_word(~((unsigned short) 0));

std::array<int, 243> results;

void show_guesses(std::array<std::tuple<Word, int>, EXPLORATION_RATE>& guesses_to_try) {
    for (int i = 0; i < EXPLORATION_RATE; ++i) {
        std::cout << i << ". " << std::get<0>(guesses_to_try[i]) << " - " << std::get<1>(guesses_to_try[i]) << "\n";
    }
}

int total_words_eliminated(const Answer_list& answers, Word guess, int beta) {
    results.fill(0);
    int N = answers.length;
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
    inline bool operator() (const std::tuple<Word, int>& v1, const std::tuple<Word, int>& v2) {
        return std::get<1>(v1) > std::get<1>(v2);
    }
};

void get_guesses(std::array<std::tuple<Word, int>, EXPLORATION_RATE>& guesses_to_try, const Answer_list& answers) {
    guesses_to_try.fill({bad_word, 0});
    for (unsigned short i = 0; i < NUM_WORDS; ++i) {
        Word w(i);

        int words_eliminated = total_words_eliminated(answers, w, std::get<1>(guesses_to_try[EXPLORATION_RATE - 1]));
        if (words_eliminated <= std::get<1>(guesses_to_try[EXPLORATION_RATE - 1])) {
            continue;
        }
        guesses_to_try[EXPLORATION_RATE - 1] = {w, words_eliminated};
        // TODO: (optimization) switch to using a priority queue
        std::sort(guesses_to_try.begin(), guesses_to_try.end(), compare());
        if (std::get<1>(guesses_to_try[0]) == (answers.length - 1) * (answers.length - 1) + answers.length) {
            break;
        }
    }
}

// std::unordered_map<Answer_list, std::tuple<Word, double>> cache;
std::unordered_map<Answer_list, std::tuple<Word, double>, std::hash<Answer_list>, std::equal_to<Answer_list>> cache;
std::tuple<Word, double> get_guess_aux(const Answer_list& answers, int max_guesses, double best_guess_count) {
    if (max_guesses == 0) {
        return {bad_word, std::numeric_limits<double>::infinity()};
    } else if (answers.length == 1) {
        return {answers.answers[0], 1.0};
    } else if (answers.length == 2) {
        if (max_guesses > 1) {
            return {answers.answers[0], 1.5};
        } else {
            return {bad_word, std::numeric_limits<double>::infinity()};
        }
    }
    auto lookup = cache.find(answers);
    if (lookup != cache.end()) {
        auto res = lookup->second;
        if (!(std::get<0>(res) == bad_word) && answers.length == 2 && !(answers.contains(std::get<0>(res)))) {
            std::cout << "Suspicious: " << std::get<0>(res) << " Answers:\n";
            answers.print();
        }
        return lookup->second;
    }
    Word best_word = bad_word;
    std::array<std::tuple<Word, int>, EXPLORATION_RATE> guesses_to_try;
    get_guesses(guesses_to_try, answers);
    double dl = ((double) 1.0) / ((double) answers.length);
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
            std::tuple<Word, double> best = get_guess_aux(answers.filter(guess, res), max_guesses - 1, MAX_TOTAL_GUESSES);
            // total += dl * (1.0 + std::get<1>(best));
            total += dl * std::get<1>(best);
        }
        // double ev = total / ((double) answers.length);
        double ev = total;
        if (ev < best_guess_count) {
            best_guess_count = ev;
            best_word = guess;
        }
    }
    std::tuple<Word, double> res = {best_word, best_guess_count};
    if (!(best_word == bad_word) && answers.length == 2 && !(answers.contains(best_word))) {
        std::cout << "Suspicious: " << best_word << " Answers:\n";
        answers.print();
    }
    cache.insert({answers, res});
    return res;
}

std::tuple<Word, double> get_guess(const Answer_list& answers, int max_guesses) {
    return get_guess_aux(answers, max_guesses, MAX_TOTAL_GUESSES);
}
