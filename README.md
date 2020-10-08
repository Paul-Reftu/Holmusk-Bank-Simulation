# Quick Setup

Hello! If you're seeing this, you've probably been redirected from one of my project lists. The following is an 
explanation of this particular project and how to compile it. Thank you for taking the time to visit! 

Firstly, you will notice that the solution is split into multiple modules, rather than
being a single monolithic part - this is done to foster good software properties, such
as scalability, maintainability, understandability, and so on. Moreover, throughout
the code, you will find comments prefixed by `"Paul Reftu: "`, whose purpose is to
expedite your understanding of the program functionalities described in their corresponding
contexts.

Secondly, to briefly explain my actual solution to the Bank Simulation problem, I
propose an efficient streaming approach based on one of the best streaming libraries
for Haskell, namely, `Streamly`, whose performance is comparable to that of pure C. 
Furthermore, my specific approach exhibits `O(n)` time and space complexity (which I 
believe to be the most optimal, given the nature of our problem), where `n` represents 
the amount of time dedicated for a single simulation, specified in seconds.

Thirdly, to compile and run our program, simply execute the following command from the 
root directory of the project:

`stack build && stack exec Holmusk-Bank-Simulation-exe <simulation_time_in_seconds>`

On a recent generation, 2018 edition MacBook Pro, the simulation takes...
- `< 1s` for `simTime := 100 000`, which corresponds to simulating
about one day of customer activity for our bank.
- `~16s` for `simTime := 1 000 000`, which is about 12 days.
- and, lastly, `~3m` for `simTime := 10 000 000`, which is roughly 116 days of
simulation.

Beyond the last simulation time, I have not yet performed many tests, however, the
execution time does appear to be directly proportional to our parameter, which is only
natural, as we know our time complexity is linear. Hence, I would expect the simulation
for 1 billion seconds (or ~32 years) to only take about `300m ~= 5h`.

Statistically, it appears that the only type of customers for which it is
feasible for our bank to handle are the `Yellow` type ones, whose waiting
times average to about `1s`, regardless of the simulation time. On the other hand,
both `Red` and `Blue` type customers require so much processing time that
our queues become incredibly large. If our bank model is accurate, then we would
definitely need more tellers for the two latter types.


Finally, I would advise the following order of evaluation for the code, which I believe
would be most convenient and easy to understand:

`Auxiliary.hs => RuntimeException.hs => Customer.hs => BankSimulation.hs =>
Statistics.hs => StatisticsLogger.hs => Main.hs`
