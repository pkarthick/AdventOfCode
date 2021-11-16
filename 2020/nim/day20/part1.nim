import strutils
import sequtils
import tables
import algorithm
import sugar
import options
import sets

let input="""Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."""


let segs = input.split("\n\n")

var tiles = segs.mapIt(it.split("\n")).map(x => (x[0].split({' ', ':'})[1].parseInt, x[1..^1])).toTable

proc getAllBorders(cell: int): seq[string] =
    let rows = tiles[cell].toSeq

    let sides = @[rows[0], rows[^1], rows.mapIt(it[0]).foldl(a & $b, ""), rows.mapIt(it[^1]).foldl(a & $b, "")]

    return sides

proc rev(s: string): string =
    s.reversed.foldl(a & $b, "")

var pairs = initHashSet[(int, int)]()

var corners = newSeq[int]()

var allCols = newSeq[int]()

for k, _ in tiles:
    allCols.add k

for i1, tile1 in tiles:

    let allSides = getAllBorders(i1)

    let l = allSides.filter(side => allCols.filterIt(it != i1).filter(k => side in getAllBorders(k) or side.rev() in getAllBorders(k)).len == 0).len 

    if l == 2:
        corners.add i1

echo corners.foldl(a * b, 1)
