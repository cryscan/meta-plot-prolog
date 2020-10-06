likes(john, ann).
likes(ann, john).

date(X, Y) :-
    likes(X, Y),
    likes(Y, X). 