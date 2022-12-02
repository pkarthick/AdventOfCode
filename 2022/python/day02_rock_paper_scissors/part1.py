import sys

rock = 1
paper = 2
scissors = 3

draw = 3
win = 6
loss = 0

scores = {
    "A X": rock + draw,
    "A Y": paper + win,
    "A Z": scissors + loss,
    "B X": rock + loss,
    "B Y": paper + draw,
    "B Z": scissors + win,
    "C X": rock + win,
    "C Y": paper + loss,
    "C Z": scissors + draw,
}

print(sum(map(lambda k: scores[k.strip()], sys.stdin.readlines())))
