import sys

rock = 1
paper = 2
scissors = 3

draw = 3
win = 6
loss = 0

scores = {
    "A X": scissors + loss,
    "A Y": rock + draw,
    "A Z": paper + win,
    "B X": rock + loss,
    "B Y": paper + draw,
    "B Z": scissors + win,
    "C X": paper + loss,
    "C Y": scissors + draw,
    "C Z": rock + win,
}

print(sum(map(lambda k: scores[k.strip()], sys.stdin.readlines())))
