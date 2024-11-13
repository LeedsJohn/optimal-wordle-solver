#ifndef MAKE_EVALUATIONS_H
#define MAKE_EVALUATIONS_H

#include "result.h"
#include <string>

// TODO: eventually should not be in header 
result evaluate(const std::string& guess, const std::string& answer);

// Create two dimensional array and save it to EVALUATIONS_FILE. This will only be run
// if the answers list or guesses list changes.
void make_evaluations();


#endif
