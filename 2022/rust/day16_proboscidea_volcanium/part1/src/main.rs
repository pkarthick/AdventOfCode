use std::{collections::HashMap, rc::Rc, usize};

#[derive(Debug, Clone)]
struct MoveActivity<'a> {
    data: Rc<Data>,
    previous_move: Option<&'a MoveActivity<'a>>,
    minutes_elapsed: usize,
    valve_id: usize,
    open: Option<&'a OpenActivity>,
}

#[derive(Debug, Clone)]
struct OpenActivity {
    data: Rc<Data>,
    valve_id: usize,
    released_pressure: usize,
    minutes_elapsed: usize,
    open_valves: OpenValves,
    remaining_total_pressure: usize,
}

#[derive(Debug, Clone)]
struct State {
    max_pressures: HashMap<String, Rc<OpenActivity>>,
    max_pressure: usize,
    count: usize,
}

#[derive(Debug, Clone)]
struct OpenValves {
    opened: Vec<usize>,
}

impl OpenValves {
    fn new(valve_id: usize) -> Self {
        OpenValves {
            opened: vec![valve_id],
        }
    }

    fn add_valve(&self, open_valve_id: usize) -> Self {
        let mut opened = self.opened.clone();
        opened.push(open_valve_id);
        opened.sort();
        OpenValves { opened }
    }
}

impl ToString for OpenValves {
    fn to_string(&self) -> String {
        let mut s = String::new();
        for x in self.opened.iter() {
            s.push_str(x.to_string().as_str());
            s.push(' ');
        }
        s
    }
}

impl<'a> MoveActivity<'a> {
    fn new(data: Rc<Data>) -> Self {
        MoveActivity {
            valve_id: data.start_valve,
            data,
            previous_move: None,
            open: None,
            minutes_elapsed: 0,
        }
    }

    fn create_open_activity(&'a self, state: &mut State) -> Option<Rc<OpenActivity>> {
        state.count += 1;
        if let Some(open_activity) = self.open {
            let open_valves = open_activity.open_valves.add_valve(self.valve_id);
            let oa = OpenActivity {
                valve_id: self.valve_id,
                data: self.data.clone(),
                minutes_elapsed: self.minutes_elapsed + 1,
                released_pressure: open_activity.released_pressure
                    + (self.data.available_minutes - 1 - self.minutes_elapsed)
                        * self.data.pressures[self.valve_id],
                remaining_total_pressure: open_activity.remaining_total_pressure
                    - self.data.pressures[self.valve_id],
                open_valves,
            };

            let key = oa.open_valves.to_string();
            if let Some(state) = state.max_pressures.get(&key) {
                if oa.released_pressure <= state.released_pressure
                    && oa.minutes_elapsed >= state.minutes_elapsed
                {
                    return None;
                }
            }
            if oa.released_pressure > state.max_pressure {
                state.max_pressure = oa.released_pressure;
            }
            let rc = Rc::new(oa.clone());
            state.max_pressures.insert(key, rc.clone());
            Some(rc)
        } else {
            Some(Rc::new(OpenActivity {
                valve_id: self.valve_id,
                data: self.data.clone(),
                minutes_elapsed: self.minutes_elapsed + 1,
                released_pressure: (self.data.available_minutes - 1 - self.minutes_elapsed)
                    * self.data.pressures[self.valve_id],
                remaining_total_pressure: self.data.total_pressure
                    - self.data.pressures[self.valve_id],
                open_valves: OpenValves::new(self.valve_id),
            }))
        }
    }

    fn can_release_max_pressure(&self, state: &mut State) -> bool {
        if let Some(oa) = self.open {
            if self.minutes_elapsed < 30 {
                let mins = 30 - self.minutes_elapsed - 1; // it will take atleast take 1 minute to open the next valve

                if mins > 0 && state.max_pressure > oa.released_pressure {
                    let avg = (state.max_pressure - oa.released_pressure) / mins;
                    if avg > oa.remaining_total_pressure {
                        return false;
                    }
                }
            }
        }
        true
    }

    fn valve_can_be_opened(&self, valve_id: usize) -> bool {
        if self.data.openable[valve_id] {
            if let Some(open_activity) = self.open {
                !open_activity.open_valves.opened.contains(&valve_id)
            } else {
                true
            }
        } else {
            false
        }
    }

    fn in_loop(&self, valve_id: usize) -> bool {
        let mut activity = Some(self);
        while let Some(ma) = activity {
            if ma.valve_id == valve_id {
                return true;
            } else {
                activity = ma.previous_move;
            }
        }
        false
    }

    fn visit_connections(&'a self, state: &mut State) -> usize {
        let mut max_pressure = 0;
        if self.minutes_elapsed < self.data.available_minutes {
            for conn_valve_id in self.data.connections[self.valve_id].iter() {
                if !self.in_loop(*conn_valve_id) {
                    state.count += 1;
                    let move_activity = self.create_move_activity(*conn_valve_id);
                    if !move_activity.can_release_max_pressure(state) {
                        continue;
                    }
                    let pressure = move_activity.compute_maximum_pressure_released(state);
                    if pressure > max_pressure {
                        max_pressure = pressure;
                    }
                }
            }
        } 
        max_pressure
    }

    fn create_move_activity(&'a self, valve_id: usize) -> MoveActivity {
        MoveActivity {
            valve_id,
            data: self.data.clone(),
            previous_move: Some(self),
            minutes_elapsed: self.minutes_elapsed + 1,
            open: self.open,
        }
    }

    fn compute_maximum_pressure_released(&self, state: &mut State) -> usize {
        if self.minutes_elapsed == self.data.available_minutes - 1 {
            if let Some(open) = self.open {
                open.released_pressure
            } else {
                0
            }
        } else {
            let mut max_pressure = 0;
            if self.valve_can_be_opened(self.valve_id) {
                if let Some(open_activity) = self.create_open_activity(state) {
                    max_pressure = open_activity.visit_connections(state);
                }
            }
            let move_pressure = self.visit_connections(state);
            move_pressure.max(max_pressure)
        }
    }
}

impl OpenActivity {
    fn create_move_activity(&self, valve_id: usize) -> MoveActivity {
        MoveActivity {
            valve_id,
            data: self.data.clone(),
            previous_move: None,
            minutes_elapsed: self.minutes_elapsed + 1,
            open: Some(self),
        }
    }

    fn visit_connections(&self, state: &mut State) -> usize {
        self.data.connections[self.valve_id]
            .iter()
            .filter_map(|conn_valve_id| {
                let move_activity = self.create_move_activity(*conn_valve_id);
                if !move_activity.can_release_max_pressure(state) {
                    return None;
                }
                Some(move_activity.compute_maximum_pressure_released(state))
            })
            .max()
            .unwrap_or(0)
    }
}

#[derive(Debug, Clone)]
struct Data {
    pressures: Vec<usize>,
    openable: Vec<bool>,
    total_pressure: usize,
    connections: Vec<Vec<usize>>,
    start_valve: usize,
    available_minutes: usize,
}

impl Data {
    fn new(lines: Vec<String>) -> Data {
        let mut connection_names_vec = vec![];
        let mut connection_indices: HashMap<String, usize> = HashMap::new();

        let mut pressures = vec![];
        let mut openable = vec![];
        let mut connections = vec![];
        let mut start_valve = 0_usize;
        let mut total_pressure = 0;

        for (index, line) in lines.iter().enumerate() {
            let words: Vec<&str> = line.trim().split(&[' ', '=', ';', ',']).collect();
            let name: String = words[1].into();

            if name == "AA" {
                start_valve = index;
            }

            let pressure = words[5].parse::<usize>().unwrap();
            total_pressure += pressure;
            pressures.push(pressure);

            openable.push(pressure > 0);

            let connection_names: Vec<String> = words
                .iter()
                .skip(11)
                .filter_map(|s| {
                    if s.is_empty() {
                        None
                    } else {
                        Some(s.to_string())
                    }
                })
                .collect();

            connection_names_vec.push(connection_names);
            connection_indices.insert(name, index);
        }

        for connection_names in connection_names_vec {
            let mut indices = vec![];
            for conn in connection_names {
                indices.push(*connection_indices.get(&conn).unwrap());
            }
            connections.push(indices);
        }

        Data {
            connections,
            total_pressure,
            pressures,
            openable,
            start_valve,
            available_minutes: 30,
        }
    }
}

fn main() {
    let lines: Vec<String> = std::io::stdin()
        .lines()
        .into_iter()
        .map(|res| res.unwrap())
        .collect();

    compute_maximum_pressure_released(lines);
}

fn compute_maximum_pressure_released(lines: Vec<String>) {

    let data = Data::new(lines);

    let max_pressures: HashMap<String, Rc<OpenActivity>> = HashMap::new();
    let max_pressure: usize = 0;
    let count: usize = 0;

    let mut state = State {
        max_pressures,
        max_pressure,
        count,
    };

    let maximum_pressure =
        MoveActivity::new(Rc::new(data)).compute_maximum_pressure_released(&mut state);

    println!("\nActivities created: {}", state.count);
    println!("Max Pressure Released: {0}\n", maximum_pressure);
}
