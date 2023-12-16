public class Day9 {


    // 0 3 6 9 12 
    // 3 6 9 12 15

    string[] lines = File.ReadAllLines("../input/day09");

//     string[] lines = @"0 3 6 9 12 15
// 1 3 6 10 15 21
// 10 13 16 21 30 45".Split("\n").ToArray();

    public int findExtraPolatedValue(String line) {

        var nums = line.Split(" ", StringSplitOptions.RemoveEmptyEntries).Select(w => Int32.Parse(w)).ToList();
        List<List<int>> lss = new();

        while (!nums.All(x => x == 0)) {
            lss.Add(nums);
            nums = nums.Zip(nums.Skip(1)).Select(tup => tup.Second - tup.First).ToList();
        }

        lss.Reverse();

        int last = 0;
                
        foreach (var ls in lss) {

            last = (ls.Last() + last);

        }


        // Console.WriteLine(last);

        return last;


    }

    public int findExtraPolatedValue2(String line) {

        var nums = line.Split(" ", StringSplitOptions.RemoveEmptyEntries).Select(w => Int32.Parse(w)).ToList();
        
        List<List<int>> lss = new();

        while (!nums.All(x => x == 0)) {
            lss.Add(nums);
            nums = nums.Zip(nums.Skip(1)).Select(tup => tup.Second - tup.First).ToList();
        }

        lss.Reverse();

        int first = lss.Aggregate(0, (a, ls) => ls.First() - a);

        var nums1 = Enumerable.Range(1, 10);

        var total = nums1.Aggregate(1, (total, n) => total * n);

        // int first = 0;
                
        // foreach (var ls in lss) {
        //     first = (ls.First() - first);
        // }

        // Console.WriteLine();

        // Console.WriteLine(last);

        return first;


    }

    public void part1() {
        var res = lines.Select(findExtraPolatedValue).Sum();
        
        Console.Write("Answer for Day 9 Part 1: ");
        Console.WriteLine(res);
    }

    public void part2() {
        var res = lines.Select(findExtraPolatedValue2).Sum();
        Console.Write("Answer for Dayy 9 Part 2: ");
        Console.WriteLine(res);
    }
}