import strutils
import sequtils
import sugar

#[
    Trying out some nim features like recursive type
]#

type 
    ValidationKind = enum 
        Contains 
        InCharRanges 
        Integer 
        InRange 
        Length 
        HasItem 
        EndsWith 
        Group
        Or
        And
    
    Validation* = ref object 
        massageInput: proc(input:string): string
        case kind: ValidationKind 
        of Contains: entries: seq[string] 
        of InCharRanges: char_ranges: seq[HSlice[char, char]] 
        of Integer: val: string 
        of InRange: slice: HSlice[int, int] 
        of Length: length: int 
        of HasItem: item: tuple[index: int, ch: char] 
        of EndsWith: e: string 
        of Group: group: ValidationGroup
        of Or: orops: tuple[this: Validation, that: Validation]
        of And: andops: tuple[this: Validation, that: Validation]
        
    ValidationGroup* = seq[Validation]

proc validate*(v: Validation, strToValidate: string): bool = 

    var input = strToValidate

    if v.massageInput != nil:
        input = v.massageInput(strToValidate)

    case v.kind 
    of Contains: 
        input in v.entries
    of Length: 
        v.length == input.len 
    of HasItem: 
        input[v.item.index] == v.item.ch 
    of InRange: 
        input.parseInt in v.slice 
    of InCharRanges: 
        input.all(ch => v.char_ranges.anyIt(ch in it))
    of EndsWith: 
        input.endsWith(v.e) 
    of Group:
        v.group.allIt(it.validate(input))
    of Or:
        v.orops.this.validate(input) or v.orops.that.validate(input)
    of And:
        v.andops.this.validate(input) and v.andops.that.validate(input)
    of Integer: 
        try: 
            discard input.parseInt
        except: 
            return false 
        true

proc newContainsValidation*(entries: seq[string]): Validation =
    Validation(kind: Contains, entries: entries)

proc newLengthValidation*(length: int): Validation =
    Validation(kind: Length, length: length)

proc newHasItemValidation*(item: tuple[index: int, ch: char]): Validation =
    Validation(kind: HasItem, item: item)

proc newInCharRangesValidation*(ranges: seq[HSlice[char, char]], massageInput: proc(input:string): string): Validation =
    Validation(kind: InCharRanges, char_ranges: ranges, massageInput: massageInput) 

proc newGroupValidation*(group: ValidationGroup): Validation =
    Validation(kind: Group, group: group)

proc newEndsWithValidation*(ends: string): Validation =
    Validation(kind: EndsWith, e: ends)

proc newIntegerValidation*(massageInput: proc(input:string): string = nil): Validation =
    Validation(kind: Integer, massageInput: massageInput)

proc newInRangeValidation*(slice: HSlice[int, int], massageInput: proc(input:string): string = nil): Validation =
    Validation(kind: InRange, slice: slice, massageInput: massageInput)

proc newOrValidation*(this: Validation, that: Validation): Validation =
    Validation(kind: Or, orops: (this, that))

proc newAndValidation*(this: Validation, that: Validation): Validation =
    Validation(kind: And, andops: (this, that))

proc all*(group: ValidationGroup, val: string): bool = 
    group.allIt(it.validate(val))

proc any*(group: ValidationGroup, val: string): bool = 
    group.anyIt(it.validate(val))

proc `or`*(this: Validation, that: Validation, val: string): bool =
    this.validate(val) or that.validate(val)

proc `and`*(this: Validation, that: Validation, val: string): bool =
    this.validate(val) and that.validate(val)
