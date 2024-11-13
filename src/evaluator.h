#ifndef EVALUATOR_H
#define EVALUATOR_H
#include "result.h"
#include "word.h"
#include "constants.h"
#include <array>


class Evaluator {
    public:
        Evaluator();
        result evaluate(const Word guess, const Word answer) const;

    private:
        void load_evaluations();
};

extern Evaluator evaluator;

#endif 
