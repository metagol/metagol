:- use_module('../metagol').

%% tell metagol to use the BK
prim(p/2).

%% metarules
metarule([P,Q,X,Y],([P,X,A]:-[[Q,Y,A]])).

%% background knowledge
p(spongebob,laura).
p(patrick,amelia).


a:-
    learn([f(andy,laura),f(andy,amelia)],[]).