% ===========================
% ====COMP30020 Project 1====
% ===========================
% author: Shiqi (Daniel) Lin <sllin2@student.unimelb.edu.au>
% purpose: select the best hand in a game of Cribbage

% ===========================
% ====Program Description====
% ===========================
% This program can be used to determine the Cribbage point value of a player's
% hand in the show stage in a game of Cribbage.
% It can also find the hand of 4 cards out of 5 or 6 cards dealt to the player
% that maximises the expected value of Cribbage points gained.
% This is achieved by generating combinations of all possible start cards with
% all possible selection of hand, and calculating their expected value to find
% the one with the highest expected value.
% All cards in the program are represented as card(Rank, Suit)
%         ^see is_card/1 for more description (line 300). 

% ===========================
% ======Main Predicates======
% ===========================

% select_hand(+Cards, ?Hand, ?CribCards)
% holds if Hand is the selection of 4 cards out of Cards that gives the highest
% expected value of Cribbage Points.
% Cards, Hand, CribCards are lists of cards.
% CribCards is the rest of the cards in Cards (discarded).
select_hand(Cards, Hand, CribCards):-
        % this 3 tuple is used to sort the possible combinations
        findall(ExpectedValue-Combination-Crib,
                (hand_combination(Cards, Combinations),
                 member(Combination, Combinations), 
                 subtract_list(Cards, Combination, Crib),
                 expected_value(Combination, ExpectedValue)
                ), 
                Possibilities),
        keysort(Possibilities, SortedPossibilities),
        last_element(SortedPossibilities, _-Hand-CribCards).

% hand_value(+Hand, +StartCard, ?Value)
% holds if Value is the total value of Hand when StartCard is the start card.
% Hand is a list of Cards, StartCard is a card, Value is an integer
hand_value(Hand, StartCard, Value):-
        % runs/2 requires the hand to be sorted
        sort_hand([StartCard|Hand], SortedHand),
        runs(SortedHand,RunsScore),
        fifteens(SortedHand, FifteensScore),
        pairs(SortedHand, PairsScore),
        flushes(Hand, StartCard, FlushesScore),
        nob(Hand, StartCard, NobScore),
        Value is RunsScore + FifteensScore + PairsScore + FlushesScore
                + NobScore.

% ===========================
% =====Helper Predicates=====
% ===========================

% ====Select_Hand====

% hand_combination(+InitialCards, -Hands)
% holds when Hands is the set of all combinations of 4 cards chosen from
% InitialCards (which contains 5 or 6 cards). 
% InitialCards is a list of cards, Hands is a list of list of cards.
hand_combination(InitialCards, Hands):-
        findall(Hand,
                % all hands must have 4 cards
                (length(Hand, 4),
                sublist(Hand, InitialCards)),
                Hands).

% expected_value(+Hand, ?ExpectedValue)
% holds when ExpectedValue is the expected value of Hand combined with all
% possible start cards.
% Hand is a list of Cards, ExpectedValue is a float
expected_value(Hand, ExpectedValue):-
        findall(Value, 
                (is_card(StartCard),
                \+member(StartCard, Hand),
                hand_value(Hand, StartCard, Value)),
                Values),
        length(Values, Length),
        sumlist(Values, Sum),
        ExpectedValue is Sum / Length.

% ====Hand_Value====

% ----Fifteens----

% fifteens(+Hand, ?Score)
% holds when Score is the total score of all the "fifteens" in Hand.
% (you get points for any distinct combination of cards that sum to 15.)
% Hand is a list of cards, Score is an integer.
fifteens(Hand, Score):-
        findall(Cards, 
                (sublist(Cards, Hand), 
                sum_is_fifteen(Cards)
                ), 
                Fifteens),
        length(Fifteens, Length), 
        % 2 points for every "fifteen"
        Score is Length * 2.

% sum_is_fifteen(+Cards)
% Cards is a list of cards
% holds if sum of all the cards in Cards is 15
sum_is_fifteen(Cards):-
        findall(RankValue,
                (member(Card, Cards), 
                % Royals are counted as 10 when considering fifteens
                capped_rank_value(Card, RankValue)
                ), 
                Values), 
        sumlist(Values, 15).

% ----Pairs----

% pairs(+Hand, ?Score)
% holds when Score is the total score of all the pairs in Hand.
% Hand is a list of cards, Score is an integer.
pairs(Hand, Score):-
        has_pairs(Hand, NumPairs),
        % any distinct pair generates 2 points
        Score is NumPairs * 2;
        \+has_pairs(Hand, Score), 
        Score = 0.

% has_pairs(+Hand, ?TotalNumPairs)
% holds when TotalNumPairs is the total number of distinct pairs in Hand.
% (a pair is 2 cards with the same rank.)
% Hand is a list of cards, TotalNumPairs is an integer.
has_pairs([H|T], TotalNumPairs):-
        T = []
        ->       TotalNumPairs = 0
        ;has_pairs(T, NumPairsTail),
        findall(H-X, 
                (member(X, T), 
                rank_value(H,Value), 
                rank_value(X,Value)
                ), 
                HeadTailPairs),
        length(HeadTailPairs, NumPairs),
        TotalNumPairs is NumPairs + NumPairsTail.

% ----Runs----

% runs(+Hand, ?Score)
% holds when Score is the total score of runs in Hand.
% This assumes hand is sorted!
% Hand is a list of cards, Score is an integer.
runs(Hand, Score):-
        findall(Run, is_run(Hand, Run), Runs),
        % to remove runs that is part of longer runs
        findall(Run, no_super_runs(Run, Runs), LongestRuns),
        maplist(length, LongestRuns, Subtotals),
        sumlist(Subtotals, Score).

% is_run(+Hand, ?Run)
% holds if Run is a valid run in Hand.
% This assumes Run and Hand are sorted
% Hand is a list of cards, Score is an integer.
is_run(Hand, Run):-
        sublist(Run, Hand),
        consecutive(Run),
        length(Run, Length),
        % all valid runs must have at least 3 cards
        Length >= 3.

% super_runs(+SubRun, +Runs)
% holds when SubRun has runs in Runs that contain SubRun(equal does not count), 
% this is used to remove duplicates
% this assumes SubRun and all runs in Runs are sorted
% SubRun is a list of cards, Runs is a list of list of cards.
super_runs(SubRun, Runs):-
        member(SubRun, Runs),
        member(Run2, Runs),
        SubRun \= Run2,
        sublist(SubRun, Run2).

% no_super_runs(+SubRun, +Runs)
% holds if SubRun is in Runs and Runs does not contain any super-runs of SubRun
% this is used to remove duplicates
% this assumes SubRun and all runs in Runs are sorted
% SubRun is a list of cards, Runs is a list of list of cards.
no_super_runs(SubRun, Runs):-
        member(SubRun, Runs),
        \+super_runs(SubRun,Runs).

% ----Flushes----

% flushes(+Hand, +StartCard, ?Score)
% holds when Score is the total score of flushes in Hand (flush or no flush)
% if StartCard is the same suit as the flush one further point is scored
% Hand is a list of cards, StartCard is a card, Score is an integer
flushes(Hand, StartCard, Score):-
        same_suit(Hand), 
        length(Hand, 4)
        ->       (Hand = [Card|_], 
                 same_suit([StartCard, Card])
                 ->        Score = 5
                 ;Score = 4
                 )
        ;Score = 0.

% same_suit(+Cards)
% holds if Cards is a list of cards that all have the same suit.
same_suit([Card1, Card2|T]):-
        Card1 = card(_,Suit1),
        Card2 = card(_, Suit2),
        Suit1 = Suit2,
        (T = [];
        same_suit([Card2|T])
        ).

% ----One-For-His-Nob----

% nob(+Hand, +StartCard, ?Score)
% holds when Score is the score of "one for his nob" of the combination of
% the Hand and StartCard.
% (Score is 1 if Hand contains the Jack of the same suit as the start card)
% Hand is a list of cards, StartCard is a card, Score is an integer
nob(Hand, StartCard, Score):-
        is_nob(Hand, StartCard),
        Score = 1;
        \+is_nob(Hand,StartCard),
        Score = 0.

% is_nob(+Hand, +StartCard)
% holds if this combination of Hand and StartCard satisfies "one for his nob"
% (if Hand contains the Jack of the same suit as the start card)
% Hand is a list of cards, StartCard is a card
is_nob(Hand, StartCard):-
        Jack = card(jack, _),
        same_suit([Jack, StartCard]),
        member(Jack, Hand).

% ====Utilities====

% ----Project Specific----

% rank_value(+Card, ?Value)
% holds when Value is the numerical value of the rank of the card.
% Card is a card, Value is an integer.
rank_value(card(Rank,_), Value):-
        (integer(Rank)
        ->       Value = Rank
        ;Rank = ace
        ->       Value = 1
        ;Rank = king
        ->       Value = 13
        ;Rank = queen
        ->       Value = 12
        ;Rank = jack
        ->       Value = 11
        ).

% capped_rank_value(+Card, ?Value)
% identical as rank_value but king, queen, jack are counted as 10.
% This is used when calculating "fifteens"
% Card is a card, Value is an integer.
capped_rank_value(Card, Value):-
        rank_value(Card, FaceValue),
        (FaceValue >= 10
        ->       Value = 10
        ;FaceValue < 10
        ->       Value = FaceValue
        ).

% sort_hand(+Hand, ?SortedHand)
% holds when SortedHand is the Hand sorted in ascending rank values.
% Hand, SortedHand are lists of cards.
sort_hand(Hand, SortedHand):-
        findall(Value-Card,
                (member(Card, Hand), 
                rank_value(Card, Value)), 
                LabelledCards),
        keysort(LabelledCards, SortedLabelledCards),
        list_second_items(SortedLabelledCards, SortedHand).

% consecutive(+Cards)
% holds if the cards in Cards are in consecutive, ascending order
% does not hold for descending order even if it is consecutive.
% Cards is a list of cards.
consecutive([_|[]]).
consecutive([H1,H2|T]):-
        rank_value(H1, V1),
        rank_value(H2, V2),
        V2 is V1 + 1, 
        consecutive([H2|T]).

% is_card(?Card)
% holds when Card is a valid variation of card
% Card is a card(Rank, Suit)
% Rank is one of (ace, 2 - 10, jack, queen, king)
% Suit is one of (diamonds, clubs, hearts, spades)
% This is used to generate all possible start cards when evaluating the 
% expected value of a hand.
is_card(card(Rank, Suit)):-
        (Rank = 2;
         Rank = 3;
         Rank = 4;
         Rank = 5;
         Rank = 6;
         Rank = 7;
         Rank = 8;
         Rank = 9;
         Rank = 10;
         Rank = king;
         Rank = queen;
         Rank = jack
        ),
        (Suit = clubs;
         Suit = diamonds;
         Suit = spades;
         Suit = hearts
        ).

% ----Generic----

% list_second_items(+ListOfTuples, ?ListOfSecondItems)
% ListOfTuples is a list of tuples, ListOfSecondItems is a list.
% this holds when ListOfSecondItems is a list made from the second items in the
% tuples in ListOfTuples.
list_second_items([],[]).
list_second_items([_-Item2|T1], [H2|T2]):-
        H2 = Item2,
        list_second_items(T1, T2).

% sublist(?List1, ?List2)
% holds when every element in List1 is in List2, and they appear in the same
% order in both lists.
% List1, List2 are lists.
sublist([], _).
sublist([H1|T1], [H2|T2]):-
        H1 = H2,
        sublist(T1, T2);
        sublist([H1|T1], T2).

% subtract_list(+List1, +List2, ?Diff)
% holds if Diff is the list of elements that appear in List1 but not in List2.
% note that this is different to append(List2, Diff, List1) as the elements
% in Diff do not have to be adjacent, same with List2.
% List1, List2, Diff are lists.
subtract_list(L1, L2, Result):-
        findall(X, 
                (member(X, L1), 
                \+member(X, L2)), 
                Result).

% last_element(?List, ?Last)
% holds when Last is the Last element in the list List.
last_element([H|[]], H).
last_element([_|T], Last):-
        last_element(T, Last).

% ====End Of Program====
