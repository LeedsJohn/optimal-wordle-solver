# Optimal Wordle Solver

This program can find an optimal guess for a game of Wordle given a list of possible
answers and possible guesses. In this case, "optimal" means minimizing the expected
number of guesses to determine the answer using the rules defined [here.](https://freshman.dev/wordle/leaderboard)
This solver takes 7,920 guesses, or 3.42 on average (the proven minimum) to solve every single answer.

## Building

Navigate to `build/` and run `cmake .. && make && ./WordleSolver`. There are no external
dependencies, so hopefully that works for people other than me...

## Algorithm

After understanding the problem, describing an algorithm to find an optimal Wordle guess
is quite simple. Imagine a function `solve` that takes a set of possible answers and
returns a best guess and the expected number of guesses the game will take to finish.

If the set of possible answers only contains one answer, this is a trivial problem.
`solve` will return the only remaining answer and this game will take `1` guesses to
finish.

If there are many possible answers remaining, we can determine an optimal guess by
trying every single possible guess and seeing which one leads to the best expected
score. For every single answer, we'll filter the list of answers to only answers that
could still be the answer after playing "guess" if "answer" is the answer. Then, we
recursively call `solve` with the new filtered answer list.

This described algorithm is prohibitively expensive, but luckily, it is faster to find
"good" guesses and only test a few promising guesses rather than every single possible
guess. I identify promising guesses by sorting the list of guesses by how many answers
they will eliminate on average.

Imagine we are playing Wordle where the only words are `admin`, `alpha`, and
and `woozy`. In this example, these words can all be possible answers and they all have
an equal probability of being the answer. In order to get the first guess, we will call
`solve([admin, alpha, woozy])`.

We begin by evaluating `admin` as a guess. If `admin` is the answer, we'll finish the
game in one guess.  If `alpha` is the answer, we'll call `solve([alpha])` because
we will be given enough information to eliminate `admin` and `woozy`. In this case,
we'll get `alpha` in two guesses. Finally, if `woozy` is the answer, we will also get
that on our second guess.  So, we expect to finish the game in `(1 + 2 + 2) / 3 = 1.67`
guesses.

Similar logic follows for evaluating `alpha` as a guess. If we guess `woozy`, we will
still finish the game in one guess one third of the time, but we still won't be able to
distinguish between `alpha` and `admin` for our second guess. `solve([alpha, admin])`
can return either word and will take `1.5` guesses on average to finish the game. So,
if our first guess is `woozy`, it will take `(1 + 2.5 + 2.5) / 3 = 2` guesses on average.

From this, it's clear that we want to either guess `admin` or `alpha` (and it doesn't 
matter which).

