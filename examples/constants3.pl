:- use_module('../metagol').

%% background knowledge
p(spongebob,laura).
p(patrick,amelia).

%% tell metagol to use the BK
prim(p/2).

%% metarules
metarule([P,Q,X,Y],([P,X,A]:-[[Q,Y,A]])).

a:-
  learn([
    f(andy,laura),
    f(andy,amelia)],[],Prog),
  pprint(Prog).