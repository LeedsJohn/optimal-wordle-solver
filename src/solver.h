#ifndef SOLVER_H
#define SOLVER_H
#include "answer_list.h"
#include "word.h"
#include <tuple>

// TODO: Remove
// int total_words_eliminated(const Answer_list& answers, Word guess, int beta);
// void get_guesses(const Answer_list& answers);
extern int times_first_word;
extern int not_first_word;
void show_guesses();

std::tuple<Word, double> get_guess(const Answer_list& answers, int max_guesses);


#endif
