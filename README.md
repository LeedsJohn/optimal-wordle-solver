# Wordle Solver

This program solves Wordle in an average of 3.42 guesses, which is proven to be the best possible average guess count using the New York Time's original list of allowed guesses and answers.

Note that this project does not *prove* that 3.42 is the optimal number of guesses and relies on prior knowledge from articles linked below
The main prior knowledge that I relied upon was that "salet" is the best starting word and that you can create an optimal decision without ever exceeding 5 guesses.

## Description of Strategy

### Game Setup

I am going to assume that you've heard of (and maybe even played) Wordle.
However, I still want to describe the game because there are a few intricacies.

First, Wordle has a list of possible five-letter words to guess, but not all of these words can be the answer.
For example, you can guess "aahed", but it can never be the answer.

Secondly, when evaluating a guess given an answer, every character in the guess is assigned a color - green (`g`) if `guess[i] = answer[i]`, yellow (`y`) if `guess[i]` is in `answer` but does not equal `guess[i]`, and otherwise, gray (`x`). 
If the answer is `lingo` and you guess `oinks`, the result will be `yggxx`.

One additional subtlety is that if you guess a word where a character occurs twice and the answer only has one of that character, one of the occurrences will be marked as `x`.
Consider the evalution of the guess `stuff` and the answer `fight` - we will be told that the first `f` is yellow but the second `f` is gray (so the result is `xyxyx`).

Additionally, green characters get priority.
This means that if the answer is `aloof` and we guess `stuff`, the second `f` will be marked as green and the first will be marked gray (so the result is `xxxxg` instead of `xxxyg`).

### Solving Algorithm

Given a list of allowed guesses and possible answers, you can create a function `f` to find the best possible guess at this point (meaning the guess that leads to the lowest expected number of guesses to find the answer).
You can model Wordle as a directed graph where each vertex represents your information about the answer and each edge represents what you could learn from making a guess.
Note that you do not know what vertex you will transition to before making a guess (but you *can* calculate the probability that you will end up in different states).

A naive strategy is to pick the guess that will eliminate the most possible answers on average, but this does not always lead to an optimal solution.
Instead, you must pick a guess based on the expected number of guesses after that guess.

This effectively involves exploring every possible guess which is very computationally expensive.
One important observation is that when evaluating a guess, you only need to consider every possible result you might be presented with rather than every possible answer.

For example, consider a Wordle dictionary consisting of the words `aaaaa`, `bbbbb`, and `ccccc`.
If you make the guess `aaaaa`, you only need to consider what your next guess will be if the result of evaluating your guess on the answer is `xxxxx`, not what will happen if the answer is `bbbbb` or `ccccc`.

Another way to trim the number of states you explore is to stop exploring after 5 guesses.
As seen in [Cyrus Freshman's Wordle leaderboard](https://freshman.dev/wordle/leaderboard), it is possible to achieve an optimal average number of guesses and not take 6 guesses to solve any puzzle.

One other way to trim the number of states is to only evaluate "good" guesses at each point.
I chose to find "good" guesses by calculating the expected number of answers remaining after making a guess.
This is cheaper than fully exploring the game tree.
The number of guesses explored at each state is referred to as the `exploration rate`.

Finally, it helps to avoid recomputing information (duh).
I've accomplished this by manually storing `salet` as the starting word and storing the best second guess given any possible result after starting with `salet`.
I intend to improve upon this strategy by caching based off of what you know about the game, such as "the third letter is `a` and there are at least two `m`s".

## Results

I was successfully able to create an optimal decision tree using an exploration rate of 100.  Creating the cache took about 30 minutes, but after this, finding an answer for every possible answer takes about 40 seconds.

## Usage

Coming soon

## Other Notes

There are several modules that I have left in here that are currently unused (`Dictionary` and `Information`).
I have not removed them yet because I think that I will reintroduce them soon.

## Interesting Links

* [Wordle-solving state of the art: all optimality results so far](https://www.poirrier.ca/notes/wordle-optimal/)

* [Wordle Solver Leaderboard](https://freshman.dev/wordle/leaderboard)
    * Contains list of the original [allowed guesses](https://gist.github.com/cfreshman/cdcdf777450c5b5301e439061d29694c)
    * And [allowed answers](https://gist.github.com/cfreshman/a03ef2cba789d8cf00c08f767e0fad7b)

* [Jonathan Olson on optimal Wordle solutions](https://jonathanolson.net/experiments/optimal-wordle-solutions)

* [Alex Selby on the best strategies for Wordle](https://sonorouschocolate.com/notes/index.php?title=The_best_strategies_for_Wordle)
