import sequtils
import strutils

let input = """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"""

func countTrees(lines: seq[string], slope : tuple[right: int, down:int]): int =
    let 
        treeid = '#'
        cols = lines[0].len 
        
    var 
        col = 0
    
    for r in countup(slope.down, lines.len-1, slope.down):
        col += slope.right
        
        if col >= cols:
            col = col %% cols

        if lines[r][col] == treeid:
            inc result

let slopes = @ [(1,1), (3,1), (5,1), (7,1), (1,2)]

echo slopes.foldl(a * countTrees(input.splitLines, b), 1)