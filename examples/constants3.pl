:- use_module('../metagol').

%% target theory
%% f(andy,laura):- p(spongebob,laura).
%% f(andy,amelia):- p(patrick,amelia).

metarule([P,Q,X,Y],([P,X,A]:-[[Q,Y,A]])).

prim(p/2).

p(spongebob,laura).
p(patrick,amelia).

a:-
  learn([
    f(andy,laura),
    f(andy,amelia)],[],H),
  pprint(H).