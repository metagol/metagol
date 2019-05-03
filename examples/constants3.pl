:- use_module('../metagol').

%% tell metagol to use the BK
body_pred(p/2).

%% metarules
metarule([P,Q,X,Y], [P,X,A], [[Q,Y,A]]).

%% background knowledge
p(spongebob,laura).
p(patrick,amelia).


:-
    learn([f(andy,laura),f(andy,amelia)],[]).