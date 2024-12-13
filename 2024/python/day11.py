
from data.day11 import PUZZLE_INPUT, TEST_INPUT

input = PUZZLE_INPUT

nums = list(map(int, input.split()))

class Node:

    def __init__(self, num):
        self.next = None
        self.num = num

i = 0

cache = {}
nodes_cache = {}
count_cache = {}

def get(n, t):

    if n in nodes_cache and (t, n) in count_cache:
        return (nodes_cache[n], count_cache[(t, n)])

    root = Node(n)

    count = 1

    for x in range(1, t+1):

        current = root

        while current:

            nxt = current.next

            if current.num == 0:
                current.num = 1

            else:

                if current.num in cache:
                    first, second = cache[current.num]

                    if second is None:
                        current.num = first
                    else:

                        current.num = first
                        current.next = Node(second)
                        current.next.next = nxt
                        count += 1

                else:
                    s = str(current.num)
                    l = len(s)
                    if l % 2 == 0:

                        first = int(s[0:l//2])
                        second = int(s[l//2:])

                        cache[current.num] = first, second
                            
                        current.num = first
                        current.next = Node(second)
                        current.next.next = nxt
                        count += 1
                    else:
                        m = current.num * 2024
                        cache[current.num] = m, None
                        current.num = m

            
            current = nxt

    nodes_cache[n] = root
    count_cache[(t, n)] = count

    return (root, count)

def get_stones_count(n, base):

    (current1, _) = get(n, base)

    total = 0

    while current1:
        count4 = 0

        if (base*4, current1.num) in count_cache:
            total += count_cache[(base*4, current1.num)]
            count4 += count_cache[(base*4, current1.num)]
            current1 = current1.next
            continue

        (current2, _) = get(current1.num, base)

        while current2:
            count3 = 0

            if (base*3, current2.num) in count_cache:
                total += count_cache[(base*3, current2.num)]
                count3 += count_cache[(base*3, current2.num)]
                count4 += count_cache[(base*3, current2.num)]
                current2 = current2.next
                continue            

            (current3, _) = get(current2.num, base)

            while current3:
                count2 = 0

                if (base*2, current3.num) in count_cache:
                    total += count_cache[(base*2, current3.num)]
                    count2 += count_cache[(base*2, current3.num)]
                    count3 += count_cache[(base*2, current3.num)]
                    count4 += count_cache[(base*2, current3.num)]
                    current3 = current3.next
                    continue            


                (current4, _) = get(current3.num, base)

                while current4:
                    (current5, count) = get(current4.num, base)

                    total += count

                    count4 += count
                    count3 += count
                    count2 += count
                    
                    current4 = current4.next

                count_cache[(base*2, current3.num)] = count2

                current3 = current3.next
            
            count_cache[(base*3, current2.num)] = count3

            current2 = current2.next

        count_cache[(base*4, current1.num)] = count4

        current1 = current1.next

        # print(total)

    return total

def get_blinking_count(times):

    total = 0
    i = 0

    while i < len(nums):
        
        count = get_stones_count(nums[i], times)

        total += count
        
        # print(i+1, len(nums), total)
        i += 1

    return total

print(get_blinking_count(5)) #part1

cache = {}
nodes_cache.clear()
count_cache.clear()

print(get_blinking_count(15)) #part2
