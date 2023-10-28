module main

enum Action {
	move
}

struct Valve {
	
	name        string   [required]
	rate        int      [required]
	connections []string [required]
}

[heap]
struct Activity {
	valve                   Valve
	valves 					map[string]Valve
	
	mut: 
	previous_activity       ?&Activity
	minutes                 int
	total_pressure_released int
	openable_count          int
	open_valves             map[string]int
	max_activities          map[string]Activity
}

fn new_activity(valves map[string]Valve, openable_count int, max_activities map[string]Activity) Activity {

	valve := valves["AA"] or { panic("Unexpected valve!") }

	return Activity{
		&valve,
		valves,
		none,
		0,
		0,
		openable_count,
		map[string]int{},
		max_activities,
	}
}

fn (activity Activity) in_loop(valve &Valve) bool {

	mut current := activity

	for {
		if current.valve.name == valve.name {
			return true
		}

		if current.previous_activity != none {
			mut current1 := current.previous_activity or { return false}
			return current1.in_loop(valve)
		} else {
			return false
		}

	}

	return false
}

fn (mut activity Activity) move_to(valve &Valve) int {

	if activity.minutes >= 29 {
		return activity.total_pressure_released
	}

	mut a := Activity{
		...activity
		valve: valve,
		previous_activity: activity,
		minutes: activity.minutes + 1,
		
	}

	return a.calculate_released_pressure()

}

fn (mut activity Activity) open_valve() int {

	if activity.minutes >= 29 {
		return activity.total_pressure_released
	}

	mut open_valves := activity.open_valves.clone()
	open_valves[activity.valve.name] = activity.minutes + 1

	// max_activities := maps.Clone(activity.max_activities)

	mut open_activity := Activity{
		...activity,
		previous_activity: none,
		minutes: activity.minutes + 1,
		total_pressure_released: activity.total_pressure_released + (29-activity.minutes)*activity.valve.rate,
		open_valves: open_valves,
	}

	key := create_key(open_valves)
	
	if key in open_activity.max_activities {

		previous_max_activity := open_activity.max_activities[key] or { return 0 }

		if open_activity.total_pressure_released > previous_max_activity.total_pressure_released {
			open_activity.max_activities[key] = open_activity
			// fmt.Println(open_activity.total_pressure_released, " > ", previous_max_activity.total_pressure_released, open_activity.minutes, previous_max_activity.minutes, open_activity.open_valves)
		} else if open_activity.total_pressure_released < previous_max_activity.total_pressure_released {
			return 0
		} else {
			if open_activity.minutes < previous_max_activity.minutes {
				open_activity.max_activities[key] = open_activity
			} else {
				return 0
			}
		}
	} else {
		open_activity.max_activities[key] = open_activity
	}

	if open_activity.open_valves.len == open_activity.openable_count {
		return open_activity.total_pressure_released
	} else {
		if open_activity.minutes >= 29 {
			return open_activity.total_pressure_released
		} else {
			return open_activity.visit_connections()
		}
	}

}


fn (mut a Activity) visit_connections() int {

	if a.minutes >= 29 {
		return a.total_pressure_released
	}

	mut max_pressure := 0

	for _, connected_valve in a.valve.connections {
		valve := a.valves[connected_valve] or { return 0 }

		if !a.in_loop(valve) {

			moved_pressure := a.move_to(valve)

			if moved_pressure > max_pressure {
				max_pressure = moved_pressure
			}

		}
	}

	return max_pressure
}

fn (mut activity Activity) calculate_released_pressure() int {

	if activity.minutes >= 29 {
		return activity.total_pressure_released
	} else if activity.valve.rate > 0 && !(activity.valve.name in activity.open_valves) {
		
		open := activity.open_valve()
		move := activity.visit_connections()

		if open > move {
			return open
		} else {
			return move
		}

	} else {
		return activity.visit_connections()
	}

}

fn create_key[V](m map[string]V) string {
	mut keys := m.keys()
	keys.sort()
	return keys.str()
}


fn (v Valve) to_string() string {
	return 'name: ${v.name}'
}

fn parse(line string) Valve {
	xs := line.split_any(';=, ')

	return Valve{
		name: xs[1]
		rate: xs[5].int()
		connections: xs[11..].filter(!it.is_blank())
	}
}

fn main() {

	input := 'Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II'

	lines := input.split_into_lines()


	mut valves := map[string]Valve{}
	mut openable_count := 0

	for v in lines.map(parse) {
		name := v.name
		valves[name] = v
		if v.rate > 0 {
			openable_count += 1
		}
	}

	mut activity := new_activity(valves, openable_count, map[string]Activity{})

	pressure := activity.calculate_released_pressure()

	println(pressure)
}
