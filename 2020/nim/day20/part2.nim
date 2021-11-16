import strutils
import sequtils
import tables
import algorithm
import sugar

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

var tl = 0
var tr = 0
var tl1 = 0

let segs = input.split("\n\n")

var tiles = segs.mapIt(it.split("\n")).map(x => (x[0].split({' ', ':'})[1].parseInt, x[1..^1])).toTable

proc getBorders(cell: int): seq[string] =
    let rows = tiles[cell].toSeq
    let sides = @[rows.mapIt(it[0]).foldl(a & $b, ""), rows[0], rows.mapIt(it[^1]).foldl(a & $b, ""), rows[^1]]
    return sides

proc getBorders(tile: seq[string]): seq[string] =
    let rows = tile
    let sides = @[rows.mapIt(it[0]).foldl(a & $b, ""), rows[0], rows.mapIt(it[^1]).foldl(a & $b, ""), rows[^1]]
    return sides

proc rev(s: string): string =
    s.reversed.foldl(a & $b, "")

var neighbours = initTable[int, seq[int]]()

var corners = newSeq[int]()
var sides = newSeq[int]()

var allCols = newSeq[int]()

for id, _ in tiles:
    allCols.add id

proc getOverlappingIDs(id: int, edge: string): seq[int] =
    return allCols.filterIt(it != id).filter(k => edge in getBorders(k) or edge.rev() in getBorders(k))

proc isOdd(id: int, edge: string): bool =
    getOverlappingIDs(id, edge).len == 0

for id, tile1 in tiles:

    let allSides = getBorders(id)

    let ns = allSides.map(side => getOverlappingIDs(id, side))

    neighbours[id] = ns.concat()

    let l = ns.filterIt(it.len == 0).len

    if l == 2:
        corners.add id
    elif l == 1:
        sides.add id

# echo corners
# echo sides


var borderFounds = newSeq[int]()

proc findNextInBorder(nexts: seq[int]): seq[int] =

    let cell = nexts[^1]
    
    if nexts.len == 11:
        borderFounds.add neighbours[cell].filterIt(it in corners)[0]
        return nexts & neighbours[cell].filterIt(it in corners)
    else:
        let ns = neighbours[cell].filterIt(it notin borderFounds and it != cell and it in sides)[0]
        borderFounds.add ns
        return findNextInBorder(nexts & ns)

var borders = newSeq[seq[int]]()

for c in corners:
    
    let ns = neighbours[c].filterIt(it in sides and it notin borderFounds)

    for n in ns:
        borderFounds.add n
        borders.add findNextInBorder(@[c, n])

proc rotate(xs: seq[string]): seq[string] =

    result = newSeq[string]()
        
    for j in 0 ..< xs.len:

        var t = newSeq[char]()

        for i in 0 ..< xs.len:

            t.add xs[i][j]

        result.add t.reversed().foldl(a & $b, "")

proc flipVertical(tile: seq[string]): seq[string] =
    tile.reversed()

proc flipHorizontal(tile: seq[string]): seq[string] =

    result = newSeq[string]()
        
    for row in tile:
        result.add row.rev


proc rotateTimes(times: int, tile: seq[string]): seq[string] =
    if times == 0:
        return tile
    else:
        let res = rotate(tile)
        return rotateTimes(times-1, res)

proc displayPair(pair: (seq[string], seq[string])) =

    echo ""
    for i in 0 ..< pair[0].len:
        echo pair[0][i] & "     " & pair[1][i]
    echo ""

proc getMatchingIndices(cornerTile: seq[string], neighbour: seq[string]): (int, int, bool) =

    let sides1 = getBorders(cornerTile)
    let sides2 = getBorders(neighbour)

    for i1, side1 in sides1:
        for i2, side2 in sides2:
            
            if side1 == side2:
                return (i1, i2, true)
            elif side1.rev == side2:
                return (i1, i2, false)

var processedTiles = newSeq[int]()

proc arrangeBorderTile(tileid: int, fixedpos: int): seq[string] =
    var tile = tiles[tileid]
    var rbs = getBorders(tile)
    var rodd = (0 .. 3).toSeq.filterIt(isOdd(tileid, rbs[it]))[0]

    if (0 .. 3).toSeq.filterIt(isOdd(tileid, rbs[it])).len > 1:
        if tileid notin corners:
            quit "Too many options in arrangeBorderTile!"
        else:

            var rodds = (0 .. 3).toSeq.filterIt(isOdd(tileid, rbs[it]))

            if rodds == @[0,1]:

                tile = rotateTimes(3, tile)
                tiles[tileid] = tile

                rbs = getBorders(tile)

                rodds = (0 .. 3).toSeq.filterIt(isOdd(tileid, rbs[it]))

                if rodds != @[0,3]:
                    echo rodds
                    quit "Still!"

                return tile
            else:
                echo rodds
                quit "Handle this now!"

    if rodd < fixedpos:
        return rotateTimes(fixedpos - rodd, tile)
    elif rodd > fixedpos:
        return rotateTimes(fixedpos + 4 - rodd, tile)
    else:
        return tile

proc arrangeRightTile(leftid: int, topid: int, rightid: int, fixedpos: int): seq[string] =

    var right = tiles[rightid]

    var lb = getBorders(tiles[leftid])[2]
    var tb = getBorders(tiles[topid])[3]

    var rbs = getBorders(right)

    let tops = (0 .. 3).toSeq.filterIt(tb == rbs[it] or tb == rbs[it].rev)

    if tops.len == 0:
        displayPair (tiles[topid], tiles[rightid])
        quit "For top not aligning"

    let ti = tops[0]

    if ti < 1:
        right = rotateTimes(1, right)
    elif ti > 1:
        right = rotateTimes(5 - ti, right)

    if tb == right[0].rev:
        right = flipHorizontal(right)

    rbs = getBorders(right)

    if lb != rbs[0]:

        echo "top and right"
        echo ""
        displayPair (tiles[topid], right)
        echo ""


        echo tb
        echo lb
        echo rbs[0]
        displayPair (tiles[leftid], right)
        quit "Right not aligning!"
    
    return right

proc arrangeTopRow(row: seq[int])=
    for i in 2 .. row.len-2:
        tiles[row[i]] = arrangeBorderTile(row[i], 1)
        
        var left = getBorders(tiles[row[i-1]])
        var right = getBorders(tiles[row[i]])

        if left[2] == right[2]:
            tiles[row[i]] = flipHorizontal(tiles[row[i]])
        else:
            tiles[row[i]] = arrangeBorderTile(row[i], 1)

        right = getBorders(tiles[row[i]])

        if left[2] != right[0]:
            echo left[2]
            echo right[0]
            displayPair (tiles[row[i-1]], tiles[row[i]])
            quit "not aligning"

        processedTiles.add row[i]
    

proc fixTopLeft(row: seq[int]): bool =

    var corner = tiles[row[0]]
    var right = arrangeBorderTile(row[1], 1)

    var rbs = getBorders(right)
    var cbs = getBorders(corner)

    let odds = (0 .. 3).toSeq.filterIt(isOdd(row[0], cbs[it]))

    if odds == @[0, 3]:

        let rotated = rotate(corner)
        cbs = getBorders(rotated)

        if not (rbs[0] == cbs[2] or rbs[2] == cbs[2]):

            let flipped = flipVertical(corner)

            cbs = getBorders(flipped)

            if not (rbs[0] == cbs[2] or rbs[2] == cbs[2]):
                return false
            else:
                corner = flipped
        else:
            corner = rotated

    elif odds == @[0,1]:

        if not (rbs[0] == cbs[2] or rbs[2] == cbs[2]):

            let flipped = flipVertical(corner)
            echo ""
            echo "here again!"
            echo ""
            displayPair (flipped, right)
            echo ""

            cbs = getBorders(flipped)

            if not (rbs[0] == cbs[2] or rbs[2] == cbs[2]):
                return false
            else:
                corner = flipped
        else:
            right = flipHorizontal(right)

    else:
        quit "Different top left tile position!"

    cbs = getBorders(corner)
    let ns = neighbours[row[0]]

    var bottomid = ns[0]

    if ns[0] == row[1]:
        bottomid = ns[1]
 
    var bottom = arrangeBorderTile(bottomid, 0)

    if bottom[0] != cbs[3]:
        bottom = flipVertical(bottom)

    if bottom[0] == cbs[3]:
        tiles[row[0]] = corner
        tiles[row[1]] = right
        tiles[bottomid] = bottom
        processedTiles.add row[0]
        processedTiles.add row[1]
        processedTiles.add bottomid
        arrangeTopRow(row)
        return true

    return false


proc arrangeBorders() =

    tl = corners[0] 

    let matching = borders.filterIt(it[0] == tl or it[^1] == tl)

    let potentialTopRights = matching.mapIt(if it[0] == tl: it[^1] else: it[0])

    proc checkIfTopRight(corner: int): bool =

        let topRight = (0,3) 
        let ns = neighbours[corner]

        let (ci, _, _) = getMatchingIndices(tiles[corner],tiles[ns[0]])
        let (ci1, _, _) = getMatchingIndices(tiles[corner], tiles[ns[1]])
        (ci == topRight[0] and ci1 == topRight[1]) or (ci == topRight[1] and ci1 == topRight[0])

    let topRights = potentialTopRights.filterIt(checkIfTopRight(it))

    if topRights.len > 0:

        let row = borders.filterIt((it[0] == tl and it[^1] == topRights[0]) or (it[^1] == tl and it[0] == topRights[0]))[0]

        if row[0] == tl:
            if not fixTopLeft(row):
                quit "row is not checking out!"
            else:
                tl1 = row[1]
                tr = row[^1]
                return

        else:
            if not fixTopLeft(row.reversed()):
                quit "row is not checking out 2!"
            else:
                tl1 = row[^2]
                tr = row[0]
                return
            
arrangeBorders()

let leftColumn = borders.filterIt((it[0] == tl and it[^1] != tr) or (it[^1] == tl and it[0] != tr))[0]
var topRow = borders.filterIt((it[0] == tl and it[^1] == tr) or (it[^1] == tl and it[0] == tr))[0]

if topRow[0] != tl:
    topRow = topRow.reversed()

for col in 2 ..< leftColumn.len-1:

    var tile = arrangeBorderTile(leftColumn[col], 0)

    var bs = getBorders(tile)

    if col != leftColumn.len - 1 and (bs[1] != tiles[leftColumn[col-1]][^1]):
        tile = flipVertical(tile)

    bs = getBorders(tile)

    if bs[1] != tiles[leftColumn[col-1]][^1]:
        echo tiles[leftColumn[col-1]][^1]
        echo bs[1]
        displayPair (tiles[leftColumn[col-1]], tiles[leftColumn[col]])
        quit "border tile not aligning!"

    tiles[leftColumn[col]] = tile
    processedTiles.add leftColumn[col]

tiles[leftColumn[11]] = flipVertical(tiles[leftColumn[11]])

processedTiles.add leftColumn[11]

var remTileIds = newSeq[int]()

for id in leftColumn[1 .. ^1]:
    remTileIds.add id

tiles[topRow[11]] = flipVertical(rotate(tiles[topRow[11]]))

processedTiles.add topRow[11]

var ind = 0

while ind < remTileIds.len:
        
    let leftid = remTileIds[ind]

    for rightid in neighbours[leftid].filterIt(it notin processedTiles):

        if rightid notin processedTiles:

            remTileIds.add rightid

            let topid = neighbours[rightid].filterIt(it != leftid and (rightid in corners or it in processedTiles))[0]

            var right = tiles[rightid]

            right = arrangeRightTile(leftid, topid, rightid, 0)

            tiles[rightid] = right

            processedTiles.add rightid

    inc ind


var ignoreTiles = newSeq[int]()

for col in topRow:
    ignoreTiles.add col

var allRows = newSeq[seq[int]]()
allRows.add topRow

while allRows.len < leftColumn.len:

    var row = newSeq[int]()

    for col in allRows[^1]:

        let bottomids = neighbours[col].filterIt(it notin ignoreTiles)

        if bottomids.len > 0:

            row.add bottomids[0]
            ignoreTiles.add bottomids[0]

    allRows.add row

var picture = newSeq[string]()

for row in allRows:

    var rows = newSeq[string]()
    
    for _ in 0 ..< tiles[tl].len-2:
        rows.add ""

    for col in 0 ..< row.len:
        let tile = tiles[row[col]]

        for i, tr in tile[1 .. ^2].mapIt(it[1..^2]):
            rows[i] = rows[i] & tr 

    for r in rows:
        picture.add r


let monster="""                  # 
#    ##    ##    ###
 #  #  #  #  #  #   """

let offsets = monster.splitLines().map(l => (0 .. l.len-1).toSeq.filterIt(l[it] == '#')).toSeq
let offsetIndices = (0 ..< offsets.len).toSeq

var pic = picture


# for line in pic:
#     echo line

proc getCount(pic: seq[string]): int =

    var count = 0

    for r in 0 ..< pic.len - 3:
        for c in 0 ..< pic[0].len - 19:

            if offsetIndices.all(orow => offsets[orow].allIt(pic[r+orow][c+it] == '#')):
                inc count

    return count

var rotated = pic

var pics = @[ pic, flipHorizontal(pic), flipVertical(pic), flipVertical(flipHorizontal(pic)), flipHorizontal(flipVertical(pic)) ]

for pic1 in pics:

    rotated = pic1

    for rt in 0 .. 2:

        var count = getCount(rotated)

        if count > 0:

            let total = rotated.mapIt(it.filterIt(it == '#').len).foldl(a+b, 0)

            echo total - (count * 15)

            quit "Done!"

        rotated = rotate(rotated)


let total = picture.mapIt(it.filterIt(it == '#').len).foldl(a+b, 0)
echo total

        



