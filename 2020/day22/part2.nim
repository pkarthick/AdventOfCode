import strutils
import sequtils
import sets

let input="""Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"""

var decks = input.split("\n\n").mapIt(it.splitLines()[1..^1].mapIt(it.parseInt))

var game = 1

proc playRecursiveRound(d1: seq[int], d2: seq[int], g: int): (int, seq[int]) =

    var rounds1 = initHashSet[(int, seq[int])]()
    var rounds2 = initHashSet[(int, seq[int])]()

    var deck1 = d1
    var deck2 = d2
    var round = 1

    while deck1.len > 0 and deck2.len > 0:
            
        if ((g, deck1) in rounds1 or (g, deck2) in rounds2): 
            return (0, deck1)

        rounds1.incl (g, deck1)
        rounds2.incl (g, deck2)

        if deck1[0] < deck1.len and deck2[0] < deck2.len:

            inc game

            if playRecursiveRound(deck1[1 .. deck1[0]], deck2[1 .. deck2[0]], game)[0] == 0:
                deck1 = deck1[1..^1] & deck1[0] & deck2[0]
                deck2 = deck2[1..^1]
            else:
                deck2 = deck2[1..^1] & deck2[0] & deck1[0]
                deck1 = deck1[1..^1]

        else:

            if deck1[0] > deck2[0]:
                deck1 = deck1[1..^1] & deck1[0] & deck2[0]
                deck2 = deck2[1..^1]
            else:
                deck2 = deck2[1..^1] & deck2[0] & deck1[0]
                deck1 = deck1[1..^1]

        inc round


    if deck1.len == 0:
        return (1, deck2)

    if deck2.len == 0:
        return (0, deck1)
                    

var deck1 = decks[0]
var deck2 = decks[1]

let (_, winnerDeck) = playRecursiveRound(deck1, deck2, game)
# echo winnerDeck

echo (0 ..< winnerDeck.len).toSeq.mapIt((winnerDeck.len - it) * winnerDeck[it]).foldl(a + b, 0)

