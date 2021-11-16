import strutils
import sequtils
import algorithm
import sets
import tables

let input="""mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"""

let lines = input.splitLines()

type 
    Food = (seq[string], seq[string])

proc getFood(line: string): Food =

    let segs = line.split("(contains")

    if segs.len == 1:
        return (line.splitWhitespace(), @[])
    else:
        return (segs[0].splitWhitespace, segs[1].split({' ', ',', ')'}).filterIt(it.len > 0))

var allergenTable = initTable[string, HashSet[string]]()

var foods = newSeq[Food]()

for line in lines:
    
    let (ingredients, allergens) = getFood(line)
    foods.add (ingredients, allergens)

    if allergens.len == 1:
        for a in allergens:
            allergenTable[a] = ingredients.toHashSet
    else:
        for a in allergens:
            allergenTable[a] = initHashSet[string]()

var foundIngredients = initTable[string, string]()
var foundAllergens = initTable[string, string]()

var probables = initTable[string, HashSet[string]]()

proc excludeFromProbable(al: string, ing: string) =
    foundAllergens[al] = ing
    foundIngredients[ing] = al

    for al1, probs in probables:
        var x = probs.toSeq.filterIt(it notin foundIngredients).toHashSet
        x.excl ing

        if x.len == 1:
            excludeFromProbable al1, x.toSeq[0]
        else:
            probables[al1] = x


for al, _ in allergenTable:

    let ings = foods.filterIt(al in it[1]).mapIt(it[0].filterIt(it notin foundIngredients).toHashSet).foldl(a * b)

    if ings.len == 1:
        excludeFromProbable al, ings.toSeq[0]
    else:
        probables[al] = ings.toSeq.filterIt(it notin foundIngredients).toHashSet

var canonical = newSeq[string]()

for al,_ in foundAllergens:
    canonical.add al

echo canonical.sorted().foldl(a & "," & foundAllergens[b], "")[1..^1]

