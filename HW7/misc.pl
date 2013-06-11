% CSE 130: Programming Assignment 4
% nano.ml
% Author: Daniel Shipps (A10239760)
% Date: 5/19/2013

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helpers

%isin(X,L) is true if X appears in L
isin(X,[X|_]).
isin(X,[_|T]) :- isin(X,T).

% zip(L1,L2,L3) is true if L3 is the result of interleaving L1 and L2
% e.g. zip([1,2],[3,4],[1,3,2,4])   is true
zip([],[],[]).
zip([H1|T1],[H2|T2],[H1,H2|T]) :- zip(T1,T2,T).

% assoc(L,K,V) is true if L is a list of 2-element lists and one of them is [K,V]
% e.g. assoc([[key1,value1],['a',1],[3,4]], 3, 4) is true
assoc([[X,Y]|_],X,Y).
assoc([_|T],X,Y) :- assoc(T,X,Y).

% remove_duplicates(L1,L2) is true if L2 is the result of removing all duplicate elements from L1.
% The remaining elements should be in the original order.
% e.g. remove_duplicates([1,1,2,2,3,3,4,4],[1,2,3,4]) is true
clean([],Soln,Y) :- reverse(Y,Soln).
clean([H|T],Soln,Y) :- isin(H,Y),!,clean(T,Soln,Y).
clean([H|T],Soln,Y) :- clean(T,Soln,[H|Y]).
remove_duplicates(L1,L2) :- clean(L1,L2,[]). 

% union(L1,L2,L3) is true if L3 is the set union of L1 and L2. 
% There should be no duplicates in L3.
% e.g. union([1,2,3],[2,3,4],[1,2,3,4]) is true
union(L1,L2,L3) :- append(L1,L2,L),remove_duplicates(L,L3). 

% intersection(L1,L2,L3) is true if L3 is the set intersection of L1 and L2.
% There should be no duplicates in L3.
% e.g. intersection([1,2,3],[2,3,4],[2,3]) is true
its([],_,X,Y) :- reverse(X,Y).
its([H|T],L,X,Y) :- isin(H,L),!,its(T,L,[H|X],Y).
its([_|T],L,X,Y) :- its(T,L,X,Y).
intersection(L1,L2,L3) :- its(L1,L2,[],L3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 1: Facts

cost(carne_asada,3).
cost(lengua,2).
cost(birria,2).
cost(carnitas,2).
cost(adobado,2).
cost(al_pastor,2).
cost(guacamole,1).
cost(rice,1).
cost(beans,1).
cost(salsa,1).
cost(cheese,1).
cost(sour_cream,1).
cost(taco,1).
cost(tortilla,1).
cost(sopa,1).


ingredients(carnitas_taco, [taco,carnitas, salsa, guacamole]).
ingredients(birria_taco, [taco,birria, salsa, guacamole]).
ingredients(al_pastor_taco, [taco,al_pastor, salsa, guacamole, cheese]).
ingredients(guacamole_taco, [taco,guacamole, salsa,sour_cream]).
ingredients(al_pastor_burrito, [tortilla,al_pastor, salsa]).
ingredients(carne_asada_burrito, [tortilla,carne_asada, guacamole, rice, beans]).
ingredients(adobado_burrito, [tortilla,adobado, guacamole, rice, beans]).
ingredients(carnitas_sopa, [sopa,carnitas, guacamole, salsa,sour_cream]).
ingredients(lengua_sopa, [sopa,lengua,beans,sour_cream]).
ingredients(combo_plate, [al_pastor, carne_asada,rice, tortilla, beans, salsa, guacamole, cheese]).
ingredients(adobado_plate, [adobado, guacamole, rice, tortilla, beans, cheese]).

taqueria(el_cuervo, [ana,juan,maria], 
        [carnitas_taco, combo_plate, al_pastor_taco, carne_asada_burrito]).

taqueria(la_posta, 
        [victor,maria,carla], [birria_taco, adobado_burrito, carnitas_sopa, combo_plate, adobado_plate]).

taqueria(robertos, [hector,carlos,miguel],
        [adobado_plate, guacamole_taco, al_pastor_burrito, carnitas_taco, carne_asada_burrito]).

taqueria(la_milpas_quatros, [jiminez, martin, antonio, miguel],  
        [lengua_sopa, adobado_plate, combo_plate]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 1: Rules



% available_at()
%  true if the item DISH is available at the taqueria SHOP

available_at(DISH,SHOP) :- taqueria(SHOP,_,ALLDISHES),isin(DISH,ALLDISHES).



% multi_available()
% true if the item DISH is available in more than one place.

% removes doubles
multi_available(DISH) :- bagof(ALLDISHES, multi_helper(ALLDISHES), LIST), remove_duplicates(LIST,RESULT), isin(DISH,RESULT).
% find items available at multiple locations
multi_helper(DISH) :- available_at(DISH,SHOP1), available_at(DISH,SHOP2), SHOP1\=SHOP2.



% overworked()
% true if the person WORKER works at more than one taqueria

% remove doubles
overworked(WORKER) :- bagof(ALLWORKERS, ow_helper(ALLWORKERS), LIST),remove_duplicates(LIST,RESULT),isin(WORKER,RESULT).
% find workers that dont work at the same shop
ow_helper(WORKER) :- taqueria(SHOP1,WORKERS1,_),taqueria(SHOP2,WORKERS2,_),isin(WORKER,WORKERS1),isin(WORKER,WORKERS2),SHOP1\=SHOP2.



% total_cost()
% true if the sum of the costs of the ingredients of item DISH is equal to PRICE 

total_cost(DISH,PRICE) :- ingredients(DISH,LIST), c_helper(LIST,PRICE).
 % base case, return 0 when list is empty
c_helper([],COST) :- COST is 0.
% get the cost of each item
c_helper([HEAD|TAIL],COST) :- cost(HEAD,TEMP), c_helper(TAIL,TEMP2), COST is TEMP + TEMP2.


 
% has_ingredients()
% true if the item DISH has all the ingredients listed in ING 

has_ingredients(DISH,ING) :- ingredients(DISH,ALL), ing_helper(ING,ALL).
% base case, return nothing when list is empty
ing_helper([],LIST).
% get the ingredients in each item
ing_helper([HEAD|TAIL],LIST) :- isin(HEAD,LIST),ing_helper(TAIL,LIST).



% avoids_ingredients()
% true if the item DISH does not have any of the ingredients listed in BADING 

avoids_ingredients(DISH,BADING) :- ingredients(DISH,ALL),a_helper(BADING,ALL). 
% base case, return nothing when list is empty
a_helper([],LIST).
% get the ingredients NOT in each item
a_helper([HEAD|TAIL],LIST) :- \+isin(HEAD,LIST), a_helper(TAIL,LIST).



% find_items()
% true if ITEMS is the list of all items that contain all the 
% ingredients in YESING and do not contain any of the ingredients in NOING.

% helper p1
p1(ITEMS1,YESING) :- bagof(LIST1,has_ingredients(LIST1,YESING),ITEMS1). 
% helper p2
p2(ITEMS2,NOING) :- bagof(LIST2,avoids_ingredients(LIST2,NOING),ITEMS2).
find_items(ITEMS,YESING,NOING) :- p1(ITEMS1,YESING),p2(ITEMS2,NOING),intersection(ITEMS1,ITEMS2,ITEMS).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
