
grade(steve,boy,97).
grade(anne,girl,97).
grade(sally,girl,88).
grade(mike,boy,77).
grade(cathy,girl,81).

same(highest,largest).
same(largest,biggest).
same(biggest,best).
same(lowest,smallest).
same(smallest,worst).
same(worst,icky-est).
same(girl,girls).
same(boy,boys).
same(guys,boys).
same(men,boys).
same(women,girls).
same(students,people).
same(students,young-uns).
same(a,some).
same(many,count).
transitive_same(X,Y,Z) :- (same(X,Y);same(Y,X)), \+ member(Y,Z).
transitive_same(X,Y,Z) :- (same(X,Q);same(Q,X)), \+ member(Q,Z), transitive_same(Q,Y,[Q|Z]).
synonym(X,Y) :- X = Y; transitive_same(X,Y,[X]).

parse(Query,Result) :-
  splitter(Query,Noun,Adj,Restrictions),
  grade(Person,Gender,Grade),
  maplist(satisfies(Person,Gender,Grade),Restrictions),
  forall(
    maplist(satisfies(_,_,OtherGrade),Restrictions),
    ( synonym(Adj,highest), Grade >= OtherGrade;
      synonym(Adj,lowest),  Grade =< OtherGrade;
      synonym(Adj,some))
  ),
  ( synonym(Noun,who),  Result = Person;
    synonym(Noun,what), Result = Grade ;
    synonym(Noun,many), singularize(Query,Q2), aggregate_all(count,parse(Q2,_),Result)).

singularize([],[]).
singularize([H1|T1],[who|T2]) :- synonym(H1,many), singularize(T1,T2), !.
singularize([H1|T1],[H1|T2]) :- singularize(T1,T2).


% all restrictions are two words, but 'for A students' is 3 words so we remove the 'for'
.% for consistency
normalize_restrictions([],[]).
normalize_restrictions([for,X,students|T1],[X,students|T2]) :- normalize_restrictions(T1,T2), !.
normalize_restrictions([H|T1],[H|T2]) :- normalize_restrictions(T1,T2).

splitter([],Noun,Adj,[[]]) :- atom(Noun), atom(Adj). % see comment in satisfies/4 for double nested list
splitter([H|T],Noun,Adj,Restrictions) :- \+atom(Noun), is_subject(H), splitter(T,H,Adj,Restrictions), Noun = H, !.
splitter([H,Object|T],Noun,H,Restrictions) :- is_adj(H), synonym(Object,grade), splitter([Object|T],Noun,H,Restrictions), !.
splitter([Object,H1,H2|T],Noun,Adj,[[H1,H2]|Restrictions]) :- % first restriction clause
  synonym(Object,grade), is_restriction(H1,H2), splitter(T,Noun,Adj,Restrictions).
splitter([who,are,H1,H2|T],Noun,Adj,[[H1,H2]|Restrictions]) :- % all other restrictions clauses
  is_restriction(H1,H2), splitter(T,Noun,Adj,Restrictions), !.
splitter([_|T],Noun,Adj,Restrictions) :- splitter(T,Noun,Adj,Restrictions), !.


is_subject(X) :- member(X,[who,what,many,count]).
is_adj(X) :- synonym(X,lowest); synonym(X,highest); synonym(X,some).
is_gender(X) :- synonym(X,boys); synonym(X,girls).
is_restriction(X,Y) :- synonym(X,for), is_gender(Y).
is_restriction(X,Y) :- (synonym(X,below); synonym(X,above)), (grade(Y,_,_); number(Y)).
is_restriction(X,Y) :- synonym(Y,students), letter_grade(X,_,_).


letter_grade(a,90,101).
letter_grade(b,80,90).
letter_grade(c,70,80).
letter_grade(d,60,70).
letter_grade(f,0,60).

satisfies(Per,Gen,Gra,[]) :- grade(Per,Gen,Gra). % otherwise maplist doesn't bind the value
satisfies(Per,Gen,Gra,[For,Gender]) :- synonym(For,for), synonym(Gen,Gender), grade(Per,Gen,Gra).
satisfies(Per,Gen,Gra,[Prep,Clause]) :-
  grade(Per,Gen,Gra),
  (
    ( % 'for girls'
      synonym(Prep,for), synonym(Gen,Clause), grade(Per,Gen,_)
    );
    (
      ( number(Clause), Grade is Clause;  % 'above 87'
        grade(Clause,_,Grade)),           % 'below mike'
      ( synonym(Prep,above), Gra > Grade;
        synonym(Prep,below), Gra < Grade)
    );
    ( % 'b students'
      Clause = students,
      letter_grade(Prep,LowerBound,UpperBound),
      Gra >= LowerBound, Gra < UpperBound
    )
  ).

get_string(X) :- get_string_helper(Y), string_codes(X,Y).
get_string_helper(X) :- get_code(Y),(Y = 63,get_code(10),X = []; Y = 10,get_string_helper(X); get_string_helper(Z), X = [Y|Z]), !.
get_words(Q) :- get_string(Y), atomic_list_concat(X,' ',Y), maplist(downcase_atom,X,Z), maplist(numerize,Z,Q).

numerize(X,Y) :- atom_number(X,Y), !.
numerize(X,X).

writeln(X) :- write(X),write('\n').

do_nlp(Start) :-
  get_words(Words),
  (
    member(done,Words),!;
    (
      findall(Result,parse(Words,Result),Results),
      list_to_set(Results,UniqueResults),
      (
        length(Results,0),writeln(none);
        maplist(writeln,UniqueResults)
      ),
      do_nlp(Start)
    )
  ),!.




























