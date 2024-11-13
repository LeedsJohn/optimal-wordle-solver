#include "constants.h"
#include "evaluator.h"
#include "result.h"
#include <array>
#include <fstream>
#include <string>


int count_occurrences(const std::string& s, const char c) {
    int res = 0;
    for (int i = 0; i < 5; ++i) {
        if (s[i] == c) {
            res++;
        }
    }
    return res;
}

result evaluate(const std::string& guess, const std::string& answer) {
    std::string res_string = "XXXXX";
    int used_counts[26] = {0};
    for (int i = 0; i < 5; ++i) {
        if (guess[i] == answer[i]) {
            res_string[i] = 'G';
            used_counts[guess[i] - 'a']++;
        }
    }
    for (int i = 0; i < 5; ++i) {
        if (res_string[i] == 'X' &&
                used_counts[guess[i] - 'a'] < count_occurrences(answer, guess[i])) {
            res_string[i] = 'Y';
            used_counts[guess[i] - 'a']++;
        }
    }
    return result_of_string(res_string);
}

// Create two dimensional array and save it to EVALUATIONS_FILE. This will only be run
// if the answers list or guesses list changes.
void make_evaluations() {
    std::string guess, answer;
    std::ifstream answers(WORDS_FILE), guesses(WORDS_FILE);
    std::array<std::string, NUM_ANSWERS> answers_ar;
    for (int i = 0; i < NUM_ANSWERS; ++i) {
        std::getline(answers, answer);
        answers_ar[i] = answer;
    }
    answers.close();
    std::ofstream evaluations(EVALUATIONS_FILE, std::ios::binary);
    while (std::getline(guesses, guess)) {
        for (int i = 0; i < NUM_ANSWERS; ++i) {
            result res = evaluate(guess, answers_ar[i]);
            evaluations.write(reinterpret_cast<const char*>(&res), sizeof(res));
        }
    }
    guesses.close();
    evaluations.close();
}
