using System.Text;

namespace Solution;

enum ActivityKind { Move, Open };

record Valve(string Name, int Pressure, string[] ConnectionNames, List<Valve> Connections)
{
    public static Valve From(string line)
    {
        var words = line.Trim().Split([' ', '=', ';', ',']).Where(w => !string.IsNullOrEmpty(w)).ToList();
        return new Valve(words[1], Convert.ToInt32(words[5]), words.Skip(10).ToArray(), []);
    }

    public bool Openable { get; } = Pressure > 0;

}

record Data(Dictionary<string, Valve> Valves)
{
    public int OpenableCount { get; init; } = Valves.Values.Where(v => v.Pressure > 0).Count();
}

record Activity(Data Data)
{

    public Dictionary<string, Activity> Activities { get; init; } = [];

    public Valve Valve { get; init; } = Data.Valves["AA"];

    public int Minutes { get; init; } = 0;

    public Activity? PreviousActivity { get; init; } = null;

    public int TotalPressureReleased { get; init; } = 0;

    public ActivityKind Kind { get; init; } = ActivityKind.Move;

    public HashSet<string> OpenValves { get; init; } = [];

    private string GetHash(HashSet<String> set)
    {

        StringBuilder sb = new();

        foreach (var item in set.Order())
        {
            sb.Append(item);
            sb.Append(' ');
        }

        return sb.ToString();

    }

    public int MaxPressure()
    {
        if (Minutes == 30)
        {
            return TotalPressureReleased;
        }
        else
        {

            int maxPressure = TotalPressureReleased;

            if (!OpenValves.Contains(Valve.Name) && Valve.Openable)
            {
                var openActivity = OpenActivity();
                if (openActivity.OpenValves.Count == Data.OpenableCount)
                {
                    return openActivity.TotalPressureReleased;
                }
                else
                {
                    var key = GetHash(openActivity.OpenValves);
                    if (!Activities.ContainsKey(key))
                    {
                        Activities.Add(key, openActivity);
                        maxPressure = openActivity.MaxPressure();
                    }
                    else
                    {
                        var cachedActivity = Activities[key];
                        if (cachedActivity.TotalPressureReleased > openActivity.TotalPressureReleased || (cachedActivity.TotalPressureReleased == openActivity.TotalPressureReleased && openActivity.Minutes >= cachedActivity.Minutes))
                            return cachedActivity.TotalPressureReleased;
                        Activities[key] = openActivity;
                        maxPressure = openActivity.MaxPressure();
                    }
                }
            }

            foreach (var conn in Valve.Connections)
            {
                if (!InLoop(conn.Name))
                {
                    int pressure = MoveActivity(conn).MaxPressure();
                    if (pressure > maxPressure)
                    {
                        maxPressure = pressure;
                    }
                }
            }

            return maxPressure;

        }
    }

    private bool InLoop(string connectedValveName)
    {

        var activity = this;

        while (activity != null)
        {
            if (activity.Kind == ActivityKind.Open)
            {
                return false;
            }
            else if (activity.Valve.Name == connectedValveName)
            {
                return true;
            }

            activity = activity.PreviousActivity;
        }

        return false;

    }

    private Activity MoveActivity(Valve connectedValve)
    {
        return this with
        {
            PreviousActivity = this,
            Valve = connectedValve,
            Minutes = Minutes + 1,
            Kind = ActivityKind.Move,
            OpenValves = OpenValves,
        };
    }

    private Activity OpenActivity()
    {

        var isOpen = OpenValves.Select(x => x).ToHashSet();
        isOpen.Add(Valve.Name);

        return this with
        {
            PreviousActivity = this,
            Minutes = Minutes + 1,
            TotalPressureReleased = TotalPressureReleased + ((29 - Minutes) * Valve.Pressure),
            Kind = ActivityKind.Open,
            OpenValves = isOpen,
        };
    }
}

class Solution
{
    public static void Main(string[] args)
    {

        var start = DateTime.Now;

        var input = @"Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        Valve BB has flow rate=13; tunnels lead to valves CC, AA
        Valve CC has flow rate=2; tunnels lead to valves DD, BB
        Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
        Valve EE has flow rate=3; tunnels lead to valves FF, DD
        Valve FF has flow rate=0; tunnels lead to valves EE, GG
        Valve GG has flow rate=0; tunnels lead to valves FF, HH
        Valve HH has flow rate=22; tunnel leads to valve GG
        Valve II has flow rate=0; tunnels lead to valves AA, JJ
        Valve JJ has flow rate=21; tunnel leads to valve II";

        var input1 = @"Valve WT has flow rate=0; tunnels lead to valves BD, FQ
Valve UG has flow rate=0; tunnels lead to valves FQ, YB
Valve FN has flow rate=0; tunnels lead to valves TV, GA
Valve RU has flow rate=11; tunnels lead to valves YZ, QS, BL, BT, WJ
Valve RH has flow rate=0; tunnels lead to valves AS, II
Valve FL has flow rate=0; tunnels lead to valves HR, PQ
Valve KQ has flow rate=18; tunnels lead to valves FR, BN
Valve PM has flow rate=25; tunnels lead to valves YZ, FR
Valve RQ has flow rate=0; tunnels lead to valves FQ, MW
Valve BL has flow rate=0; tunnels lead to valves RU, IR
Valve FF has flow rate=0; tunnels lead to valves QS, ED
Valve KP has flow rate=0; tunnels lead to valves QM, MA
Valve YB has flow rate=0; tunnels lead to valves UG, HR
Valve TV has flow rate=17; tunnels lead to valves BD, MT, FN
Valve HY has flow rate=0; tunnels lead to valves DW, IU
Valve KF has flow rate=0; tunnels lead to valves AA, HR
Valve YC has flow rate=0; tunnels lead to valves II, MA
Valve EE has flow rate=0; tunnels lead to valves AA, CD
Valve ED has flow rate=9; tunnels lead to valves HG, FF
Valve SA has flow rate=0; tunnels lead to valves MW, LS
Valve II has flow rate=20; tunnels lead to valves YC, CY, QP, RH
Valve BN has flow rate=0; tunnels lead to valves BT, KQ
Valve MO has flow rate=0; tunnels lead to valves XO, VI
Valve YZ has flow rate=0; tunnels lead to valves RU, PM
Valve WJ has flow rate=0; tunnels lead to valves RU, QP
Valve AW has flow rate=0; tunnels lead to valves HR, DW
Valve MJ has flow rate=0; tunnels lead to valves BP, AA
Valve DW has flow rate=4; tunnels lead to valves AU, CB, HY, GL, AW
Valve QM has flow rate=0; tunnels lead to valves KP, FQ
Valve LF has flow rate=5; tunnels lead to valves LS, QN, AU, BP, ZY
Valve QS has flow rate=0; tunnels lead to valves FF, RU
Valve BT has flow rate=0; tunnels lead to valves BN, RU
Valve VI has flow rate=22; tunnel leads to valve MO
Valve LS has flow rate=0; tunnels lead to valves LF, SA
Valve QD has flow rate=0; tunnels lead to valves HR, ZY
Valve HG has flow rate=0; tunnels lead to valves AS, ED
Valve BD has flow rate=0; tunnels lead to valves WT, TV
Valve CD has flow rate=0; tunnels lead to valves EE, MW
Valve QP has flow rate=0; tunnels lead to valves II, WJ
Valve MW has flow rate=7; tunnels lead to valves PQ, SA, CB, CD, RQ
Valve AU has flow rate=0; tunnels lead to valves DW, LF
Valve RR has flow rate=0; tunnels lead to valves AS, MA
Valve GA has flow rate=0; tunnels lead to valves FN, MA
Valve MT has flow rate=0; tunnels lead to valves CY, TV
Valve HR has flow rate=14; tunnels lead to valves KF, YB, QD, AW, FL
Valve AS has flow rate=16; tunnels lead to valves RR, RH, HG, IR
Valve CY has flow rate=0; tunnels lead to valves MT, II
Valve AA has flow rate=0; tunnels lead to valves OX, KF, GL, MJ, EE
Valve IU has flow rate=0; tunnels lead to valves XO, HY
Valve XO has flow rate=23; tunnels lead to valves IU, MO
Valve FR has flow rate=0; tunnels lead to valves KQ, PM
Valve CB has flow rate=0; tunnels lead to valves MW, DW
Valve ZY has flow rate=0; tunnels lead to valves QD, LF
Valve BP has flow rate=0; tunnels lead to valves LF, MJ
Valve QN has flow rate=0; tunnels lead to valves LF, FQ
Valve IR has flow rate=0; tunnels lead to valves AS, BL
Valve PQ has flow rate=0; tunnels lead to valves FL, MW
Valve GL has flow rate=0; tunnels lead to valves AA, DW
Valve OX has flow rate=0; tunnels lead to valves MA, AA
Valve MA has flow rate=10; tunnels lead to valves RR, YC, GA, OX, KP
Valve FQ has flow rate=12; tunnels lead to valves QN, WT, UG, RQ, QM";

        int Pressure = ComputeMaxPressure(input);
        Console.WriteLine($"Maximum pressure released: {Pressure}");

        int Pressure1 = ComputeMaxPressure(input1);
        Console.WriteLine($"Maximum pressure released: {Pressure1}");

        var end = DateTime.Now;

        Console.WriteLine($"{(end - start).TotalMilliseconds}");
    }

    private static int ComputeMaxPressure(string input)
    {
        var valves = input.Split(['\r', '\n']).Where(l => !string.IsNullOrEmpty(l)).Select(Valve.From).ToDictionary(v => v.Name);

        foreach (var valve in valves.Values)
        {
            foreach (var name in valve.ConnectionNames)
            {
                valve.Connections.Add(valves[name]);
            }
        }

        Data data = new(valves);
        Activity activity = new(data);
        int Pressure = activity.MaxPressure();
        return Pressure;
    }
}




