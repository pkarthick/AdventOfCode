using System.Net.Http.Headers;

public record Node(String NodeName, String Left, String Right);

public record DesertMap(char[] Directions, Node[] Nodes)
{
    internal Dictionary<string, Node> NodesDict = Nodes.ToDictionary(node => node.NodeName);

}

public class State
{

    public long Steps { get; set; } = 0;

    public int DirIndex { get; set; } = 0;

    public string NodeName { get; set; }

    public DesertMap DesertMap { get; init; }

    public static Dictionary<(string, long), (string, long)> countsDict = new();

    internal void StepsRequired()
    {
        var current = NodeName;


        var dirs = DesertMap.Directions;
        var dirsCount = dirs.Length;


        while (current != "ZZZ")
        {

            var node = DesertMap.NodesDict[current];

            if (dirs[DirIndex] == 'L')
            {
                current = node.Left;
            }
            else
            {
                current = node.Right;
            }

            DirIndex++;
            if (DirIndex == dirsCount) DirIndex = 0;
            Steps++;

        }

    }

    public State(DesertMap desertMap, string nodeName)
    {
        this.DesertMap = desertMap;
        this.NodeName = nodeName;

    }

    internal void StepsRequiredEndingWithZ(List<String> NodesEndingWithA, long targetCount)
    {

        var dirs = DesertMap.Directions;
        var dirsCount = dirs.Length;

        var initialName = NodeName;
        var initialDirIndex = DirIndex;

        var initialSteps = Steps;
        List<(String, long, int)> listNodeNames = new();

        while (Steps == 0 || Steps < targetCount)
        {
            UpdateDictionary(targetCount);

            while (Steps == 0 || Steps < targetCount || !NodeName.EndsWith("Z"))
            {
                var node = DesertMap.NodesDict[NodeName];

                if (dirs[DirIndex] == 'L')
                {
                    NodeName = node.Left;
                }
                else
                {
                    NodeName = node.Right;
                }

                DirIndex++;
                if (DirIndex == dirsCount) DirIndex = 0;
                Steps++;

            }

            if (!countsDict.ContainsKey((initialName, DirIndex)))
                countsDict.Add((initialName, DirIndex), (NodeName, Steps - initialSteps));

        }

    }

    private void UpdateDictionary(long targetCount)
    {
        if (countsDict.TryGetValue((NodeName, DirIndex), out var value))
        {
            if (Steps < targetCount)
            {
                NodeName = value.Item1;
                var count = value.Item2;
                var diff = targetCount - Steps;
                var times = diff / count;
                if (diff % count > 0)
                {
                    times++;
                }
                Steps += times * count;
                Console.WriteLine(Steps);
            }
        }

    }
}

public class SimultaneousTraversal(DesertMap DesertMap)
{

    List<State> NodesEndingWithA = DesertMap.Nodes.Where(n => n.NodeName.EndsWith("A"))
            .Select(nodeEndingWithA => new State(DesertMap, nodeEndingWithA.NodeName)).ToList();

    List<string> NodeNamesEndingWithA = DesertMap.Nodes.Where(n => n.NodeName.EndsWith("A")).Select(n => n.NodeName).ToList();

    internal bool AllCountAreSame(int count)
    {
        var all = NodesEndingWithA.Take(count).Select(state => state.Steps).ToHashSet();
        return all.Count == 1 && all.FirstOrDefault() > 0;
    }

    internal long StepsRequired()
    {


        long maxCount = NodesEndingWithA.Select(state => state.Steps).Max();

        for (int i = 0; i < NodesEndingWithA.Count; i++)
        {
            NodesEndingWithA[i].StepsRequiredEndingWithZ(NodeNamesEndingWithA, maxCount);
        }


        long lcmOfNums = lcm(NodesEndingWithA.Select(n => n.Steps).ToList());

        return lcmOfNums;

    }

    private long lcm(List<long> nums)
    {
        List<long> factors = new();

        long f = 2;

        while (f <= nums.Max())
        {
            bool x = true;

            for (int i = 0; i < nums.Count; i++)
            {
                if (factors.Any(f => nums[i] % f == 0))
                    continue;

                if (nums[i] % f == 0)
                {
                    nums[i] /= f;
                    factors.Add(f);
                    x = false;
                }

            }

            if (nums.Select(x => x).ToHashSet().Count == 1) {
                factors.Add(nums[0]);
                break;
            }

            if (x)
            {
                f++;
            }

        }

        long prod = 1;

        foreach (var num in factors)
        {
            prod *= num;
        }

        return prod;


    }



}



public class Day8
{

    String[] lines = File.ReadAllLines("../input/day08");

    public void part1() {
        var dirs = lines[0];

        var nodes = lines.Skip(2).Select(l =>
        {
            var xs = l.Split(new char[] { ' ', '=', '(', ')', ',' }, StringSplitOptions.RemoveEmptyEntries);
            return new Node(xs[0], xs[1], xs[2]);

        }).ToArray();

        DesertMap desertMap = new(dirs.ToCharArray(), nodes);
        
        State state = new(desertMap, "AAA");
        state.StepsRequired();

        long steps = state.Steps;
        Console.Write("Answer for Day 8 Part 1: ");
        Console.WriteLine(steps);

    }

    public void part2()
    {
        var dirs = lines[0];

        var nodes = lines.Skip(2).Select(l =>
        {
            var xs = l.Split(new char[] { ' ', '=', '(', ')', ',' }, StringSplitOptions.RemoveEmptyEntries);
            return new Node(xs[0], xs[1], xs[2]);

        }).ToArray();

        DesertMap desertMap = new(dirs.ToCharArray(), nodes);

        long steps = new SimultaneousTraversal(desertMap).StepsRequired();
        Console.Write("Answer for Day 8 Part 2: ");
        Console.WriteLine(steps);
    }

}