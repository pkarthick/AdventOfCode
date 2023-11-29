package main

import (
	"fmt"
	"slices"
	"strconv"
	"strings"
)

func create_key(keys []int) string {

	var b strings.Builder
	for _, key := range keys {
		fmt.Fprintf(&b, "%d ", key)
	}
	return b.String()
}

type Activity struct {
	valve                   *Valve
	prev_activity           *Activity
	minutes                 int
	total_pressure_released int
	openable_count          int
	open_valves             []int
	// max_activities          map[string]*Activity
	pressure_to_be_released int
	max_pressure            *int
}

var max_activities map[string]*Activity = make(map[string]*Activity)

func newActivity(valve *Valve, pressure_to_be_released int, openable_count int, max_pressure *int) Activity {
	return Activity{
		valve,
		nil,
		0,
		0,
		openable_count,
		[]int{},
		// max_activities,
		pressure_to_be_released,
		max_pressure,
	}
}

func (activity *Activity) inLoop(valve *Valve) bool {
	for current := activity; current != nil; current = current.prev_activity {
		if current.valve.name == valve.name {
			return true
		}
	}

	return false
}

func (activity *Activity) moveTo(valve *Valve) int {
	if activity.minutes >= 29 {
		return activity.total_pressure_released
	}
	a := &Activity{
		valve,
		activity,
		activity.minutes + 1,
		activity.total_pressure_released,
		activity.openable_count,
		slices.Clone(activity.open_valves),
		// activity.max_activities,
		activity.pressure_to_be_released,
		activity.max_pressure,
	}

	if *a.max_pressure > 0 {
		if a.minutes < 29 {
			avg := (*a.max_pressure - a.total_pressure_released) / (29 - a.minutes)
			if avg > a.pressure_to_be_released {
				return 0
			}
		}
	}

	return a.calculateReleasedPressure()
}

func (activity *Activity) openValve() int {
	if activity.minutes >= 29 {
		return activity.total_pressure_released
	}

	// max_activities := maps.Clone(activity.max_activities)

	open_valves := slices.Clone(activity.open_valves)
	open_valves = append(open_valves, activity.valve.id)
	slices.Sort(open_valves)

	open_activity := &Activity{
		activity.valve,
		nil,
		activity.minutes + 1,
		activity.total_pressure_released + (29-activity.minutes)*activity.valve.rate,
		activity.openable_count,
		open_valves,
		// activity.max_activities,
		activity.pressure_to_be_released - activity.valve.rate,
		activity.max_pressure,
	}

	if *open_activity.max_pressure > 0 {
		if open_activity.minutes < 29 {
			avg := (*open_activity.max_pressure - open_activity.total_pressure_released) / (29 - open_activity.minutes)
			if avg > open_activity.pressure_to_be_released {
				return 0
			}
		}
	}

	key := create_key(open_valves)
	previous_max_activity, exists := max_activities[key]

	if exists {
		if open_activity.total_pressure_released > previous_max_activity.total_pressure_released {
			max_activities[key] = open_activity
		} else if open_activity.total_pressure_released == previous_max_activity.total_pressure_released && open_activity.minutes < previous_max_activity.minutes {
			max_activities[key] = open_activity
		} else {
			return 0
		}

	} else {
		max_activities[key] = open_activity
	}

	if len(open_activity.open_valves) == open_activity.openable_count {
		return open_activity.total_pressure_released
	} else {
		if open_activity.minutes >= 29 {
			return open_activity.total_pressure_released
		} else {
			return open_activity.visitConnections()
		}
	}
}

type Valve struct {
	id          int
	name        string
	rate        int
	connections []*Valve
	is_openable bool
}

func newValve(line string, id int) (*Valve, []string) {
	validate_token := func(c rune) bool { return (c == ';' || c == ' ' || c == ',' || c == '=') }
	tokens := strings.FieldsFunc(line, validate_token)
	name := tokens[1]
	rate, _ := strconv.Atoi(tokens[5])
	connections := tokens[10:]
	valve := &Valve{id, name, int(rate), []*Valve{}, rate > 0}
	return valve, connections
}

func parseInput(input string) (int, *Valve, int) {
	lines := strings.Split(input, "\n")

	connections_map := map[*Valve][]string{}
	names_map := map[string]*Valve{}
	openable_count := 0
	pressure_to_be_released := 0

	for id, line := range lines {
		valve, connections := newValve(line, id)
		pressure_to_be_released += valve.rate
		if valve.rate > 0 {
			openable_count += 1
		}

		connections_map[valve] = connections
		names_map[valve.name] = valve
	}

	for valve, connections := range connections_map {

		for _, conn_id := range connections {
			connected_valve := names_map[conn_id]
			valve.connections = append(valve.connections, connected_valve)
		}

		// slices.SortFunc(valve.connections, func(v1 *Valve, v2 *Valve) int {
		// 	return int(v2.rate - v1.rate)
		// })

	}

	return openable_count, names_map["AA"], pressure_to_be_released
}

func (a *Activity) visitConnections() int {
	max_pressure := int(0)

	for _, connected_valve := range a.valve.connections {
		if !a.inLoop(connected_valve) {

			moved_pressure := a.moveTo(connected_valve)

			if moved_pressure > max_pressure {
				max_pressure = moved_pressure
			}

		}
	}

	return max_pressure
}

func (activity *Activity) calculateReleasedPressure() int {
	if activity.minutes == 30 {
		return activity.total_pressure_released
	} else if activity.valve.is_openable && !slices.Contains(activity.open_valves, activity.valve.id) {

		open := activity.openValve()
		move := activity.visitConnections()

		if open > move {
			if open > *activity.max_pressure {
				*activity.max_pressure = open
			}
			return open
		} else {
			if move > *activity.max_pressure {
				*activity.max_pressure = move
			}
			return move
		}

	} else {
		return activity.visitConnections()
	}
}

func main() {
	input := `Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
	Valve BB has flow rate=13; tunnels lead to valves CC, AA
	Valve CC has flow rate=2; tunnels lead to valves DD, BB
	Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
	Valve EE has flow rate=3; tunnels lead to valves FF, DD
	Valve FF has flow rate=0; tunnels lead to valves EE, GG
	Valve GG has flow rate=0; tunnels lead to valves FF, HH
	Valve HH has flow rate=22; tunnel leads to valve GG
	Valve II has flow rate=0; tunnels lead to valves AA, JJ
	Valve JJ has flow rate=21; tunnel leads to valve II`

	openable_count, valve, pressure_to_be_released := parseInput(input)

	max_pressure := 0

	activity := newActivity(valve, pressure_to_be_released, openable_count, &max_pressure)
	pressure := activity.calculateReleasedPressure()

	fmt.Println(pressure)

	input1 := `Valve WT has flow rate=0; tunnels lead to valves BD, FQ
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
	Valve FQ has flow rate=12; tunnels lead to valves QN, WT, UG, RQ, QM`

	max_pressure1 := 0

	openable_count1, valve1, pressure_to_be_released1 := parseInput(input1)

	activity1 := newActivity(valve1, pressure_to_be_released1, openable_count1, &max_pressure1)
	pressure1 := activity1.calculateReleasedPressure()

	fmt.Println(pressure1)
}
