import strutils
import sequtils
import algorithm

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

proc playRecursiveRound(deck1: var seq[int], deck2: var seq[int]) =

    while deck1.len > 0 and deck2.len > 0:

        if deck1[0] > deck2[0]:
            deck1 = deck1[1..^1] & deck1[0] & deck2[0]
            deck2 = deck2[1..^1]

            if deck2.len == 0:
                echo "Player 1 has won"
                
                echo (0 ..< deck1.len).toSeq.mapIt((deck1.len - it) * deck1[it]).foldl(a + b, 0)

        else:
            deck2 = deck2[1..^1] & deck2[0] & deck1[0]
            deck1 = deck1[1..^1]

            if deck1.len == 0:
                echo "Player 2 has won"
                echo (0 ..< deck2.len).toSeq.mapIt((deck2.len - it) * deck2[it]).foldl(a + b, 0)


playRecursiveRound(decks[0], decks[1])

