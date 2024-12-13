
from data.day11 import PUZZLE_INPUT, TEST_INPUT

input = PUZZLE_INPUT

nums = list(map(int, input.split()))

next_nums_cache = {}
nodes_cache = {}
count_cache = {}

def get(n, t):

    if n in nodes_cache and (1, n) in count_cache:
        return (nodes_cache[n], count_cache[(1, n)])
    
    nums = [n]

    count = 1

    for x in range(1, t+1):

        i = 0
        res = []

        for num in nums:

            if num in next_nums_cache:
                res.extend(next_nums_cache[num])
            else:
                
                if num == 0:
                    res.append(1)
                    next_nums_cache[num] = [1]

                else:
                    s = str(num)
                    l = len(s)
                    if l % 2 == 0:

                        first = int(s[0:l//2])
                        second = int(s[l//2:])

                        res.append(first)
                        res.append(second)

                        next_nums_cache[num] = (first, second)
                            
                        count += 1
                    else:
                        res.append(num * 2024)
                        next_nums_cache[num] = [num * 2024]
                    
        nums = res

    nodes_cache[n] = nums
    count_cache[(1, n)] = len(nums)

    return (nums, len(nums))

def get_stones_count(n, base, times):

    if (times, n) in count_cache:
        return count_cache[(times, n)]

    if times == 1:
        (_, total) = get(n, base)
        return total
    
    else:

        (current1, _) = get(n, base)

        total = 0

        for n1 in current1:
            count = get_stones_count(n1, base, times-1)
            total += count

        count_cache[(times, n)] = total

        return total

print(sum([get_stones_count(num, 5, 5) for num in nums])) #part1

nodes_cache.clear()
count_cache.clear()

print(sum([get_stones_count(num, 15, 5) for num in nums])) #part1
