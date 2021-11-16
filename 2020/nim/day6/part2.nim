import strutils
import sets
import sequtils

let input="""abc

a
b
c

ab
ac

a
a
a
a

b"""


echo input.split("\n\n").foldl(
    a + len(
        b.split('\n').foldl(
            a * b.mapIt(it).toHashSet, ('a'..'z').toSeq.toHashSet
        )
    ), 0
)
