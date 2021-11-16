var subno = 7

var val = 1

var loopsize = 1

let publickey = 5764801

while true:

    val = subno * val

    val = val %% 20201227

    if val == publickey:
        break

    inc loopsize

echo loopsize

val = 1
subno = 17807724

for i in 1 .. loopsize:
    
    val = subno * val

    val = val %% 20201227

echo val
