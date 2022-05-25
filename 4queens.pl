four_queens(Solution):-
    % initialize population
    random_permutation([1, 2, 3, 4], Configuration1),
    random_permutation([1, 2, 3, 4], Configuration2),
    % apply genetic algorithm
    genetic_algorithm(Configuration1, Configuration2, Solution, _), !.

genetic_algorithm([], [], Solution, Solution).
genetic_algorithm(Configuration1, Configuration2, Aux, Solution) :-
    (
         % if Configuration1 is safe then, the solution is Configuration1
         safe(Configuration1), valid(Configuration1) -> genetic_algorithm([], [], Aux, Configuration1);
         % else if Configuration2 is safe, then the solution is Configuration2
         safe(Configuration2), valid(Configuration2) -> genetic_algorithm([], [], Aux, Configuration2);
         % else
         (
               % make crossover step (generate TmpConfiguration crossing Configuration1 and Configuration2):
               uniform_crossover(Configuration1, Configuration2, TmpConfiguration),
               % make mutation step (generate Configuration3 mutating TmpConfiguration):
               mutation(TmpConfiguration, Configuration3),
               (
                   % if Configuration3 is safe and valid, then the solution is Configuration3
                   safe(Configuration3), valid(Configuration3) -> genetic_algorithm([], [], Aux, Configuration3);
                   % else compute the fitness value associated with Configuration1, Configuration2, Configuration3,
                   safe_queens(Configuration1, FitnessValue1),
                   safe_queens(Configuration2, FitnessValue2),
                   safe_queens(Configuration3, FitnessValue3),
                   % and make the selection step:
                   (
                        % if the fitness value associated with Configuration1
                        % is >= of the fitness value associated with Configuration2
                        % (Configuration1 is better than Configuration2), then
                        FitnessValue1 >= FitnessValue2 ->
                        (
                             % if the fitness value associated with Configuration2
                             % is >= of the fitness value associated with Configuration3
                             % (Configuration2 is better than Configuration3), then continue recursion on Configuration1 and Configuration2
                             FitnessValue2 >= FitnessValue3 -> genetic_algorithm(Configuration1, Configuration2, Aux, _);
                             % else (Configuration3 is better than Configuration2) continue recursion on Configuration1 and Configuration3
                             genetic_algorithm(Configuration1, Configuration3, Aux, _)
                        );
                   % else if the fitness value associated with Configuration1
                   % is >= of the fitness value associated with Configuration3
                   % (Configuration1 is better than Configuration3), then continue recursion on Configuration1 and Configuration2
                   FitnessValue1 >= FitnessValue3 ->  genetic_algorithm(Configuration1, Configuration2, Aux, _);
                   % else (Configuration3 is better than Configuration1), continue recursion on Configuration2 and Configuration3
                   genetic_algorithm(Configuration2, Configuration3, Aux, _)
                   )
              )
         )
    ).

% one-point crossover (deterministic, crossover point is in the middle)
one_point_crossover([A, B, C, D], [E, F, G, H], [A, B, G, H]).

% two-point crossover (deterministic)
two_point_crossover([A, B, C, D], [E, F, G, H], [A, F, G, D]).

% four-point crossover (deterministic)
four_point_crossover([A, B, C, D], [E, F, G, H], [A, F, C, H]).

% uniform crossover (stochastic)
uniform_crossover([], [], []).
uniform_crossover([Head1|Tail1], [Head2|Tail2], [Head3|Tail3]) :-
    % compute a random float number P
    random(0.0, 1.0, P),
    % choose current bit from either configuration with equal probability
    (
        P >= 0.5 -> Head3 is Head1;
        Head3 is Head2
    ),
    uniform_crossover(Tail1, Tail2, Tail3).


mutation([], []).
mutation([Head1|Tail1], [Head2|Tail2]) :-
    % compute a random integer valid number RandomInt
    random(1, 5, RandomInt),
    % compute a random float number P
    random(0.0, 1.0, P),
    % with probability 0.1 mutate current bit
    (
        % substitution of current bit with RandomInt
        P < 0.9 -> Head2 is Head1;
        % no substitution
        Head2 is RandomInt
    ),
    mutation(Tail1, Tail2).

% returns false if there are repetitions
valid([Head|Tail]) :- \+member(Head, Tail) -> valid(Tail).
valid([]).

% fitness function: counts how many queens are safe
safe_queens([], 0).
safe_queens([Queen|OtherQueens], SafeQueens) :-
    safe_queens(OtherQueens, CurrSafeQueens),
    noattack(Queen, OtherQueens, 1),
    SafeQueens is CurrSafeQueens + 1.
safe_queens([Queen|OtherQueens], SafeQueens) :-
    safe_queens(OtherQueens, SafeQueens),
    \+noattack(Queen, OtherQueens, 1).

safe([Queen|OtherQueens]) :-
    safe(OtherQueens),
    noattack(Queen, OtherQueens, 1).
safe([]).

noattack(_, [], _).
noattack(Q, [Q1|Qlist], Qdist) :-
    Q1 - Q =\= Qdist,            % check diagonal up
    Q - Q1 =\= Qdist,            % check diagonal down
    Dist1 is Qdist + 1,
    noattack(Q, Qlist, Dist1).

