
from functools import cache
from data.day11 import PUZZLE_INPUT, TEST_INPUT

input = PUZZLE_INPUT

nums = list(map(int, input.split()))

next_nums_cache = {}
nodes_cache = {}
count_cache = {}

@cache
def get(n, t):
    
    nums = [n]

    count = 1

    for _ in range(1, t+1):

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

    return (nums, len(nums))

@cache
def transform_stones(n, base, times):

    if times == 1:
        (_, total) = get(n, base)
        return total
    
    else:

        (stones, _) = get(n, base)

        total = 0

        for stone in stones:
            count = transform_stones(stone, base, times-1)
            total += count

        # count_cache[(times, n)] = total

        return total

print(sum([transform_stones(num, 5, 5) for num in nums])) #part1

nodes_cache.clear()
count_cache.clear()

print(sum([transform_stones(num, 15, 5) for num in nums])) #part1
