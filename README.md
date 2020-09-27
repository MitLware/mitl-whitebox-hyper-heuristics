# mitl-whitebox-hyper-heuristics

Whitebox analog of the hyflex hyper-heuristic framework

This provides an elementary example of the approach described in ["A Re-characterization of Hyper-Heuristics"][1].

By way of a whitebox analog of the 'Hyflex' HH framework, this takes as input any problem domain (examples used are SAT, bin-packing, TSP, VRP)
generically described via the *[XCSP](http://www.xcsp.org/)* constraint programming format. 

It then uses heuristic pattern matching to determine if the problem constraints are isomorphic to the TSP: 
if so, then the problem is rewritten on the fly to *[TSPLib](http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/) format and the dedicated TSP solver ( *[Concorde](http://www.math.uwaterloo.ca/tsp/concorde.html)*'s 'LINKERN') is used, 
if not then the generic *[Choco Solver](https://choco-solver.org/)* is invoked instead.

[1]: Swan J., De Causmaecker P., Martin S., Özcan E. (2018) 
A Re-characterization of Hyper-Heuristics. 
In: Amodeo L., Talbi EG., Yalaoui F. (eds) Recent Developments in Metaheuristics. 
Operations Research/Computer Science Interfaces Series, vol 62. Springer, Cham. 
https://doi.org/10.1007/978-3-319-58253-5_5
