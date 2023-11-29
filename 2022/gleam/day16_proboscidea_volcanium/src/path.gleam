import gleam/list
import gleam/map
import gleam/int
import gleam/string
import valve
import data

pub opaque type Path {
  Path(
    data: data.Data,
    activity: valve.Activity,
    previous_path: Result(Path, Int),
    remaining_total_pressure: Int,
    releasable_pressure: Int,
    released_pressure: Int,
    elapsed_minutes: Int,
    opened_valves: List(String),
    open_valve_count: Int,
    valve: valve.Valve,
  )
}

pub fn new(valves_data: data.Data) -> Path {
  let assert Ok(valve) =
    valves_data.valves
    |> map.get("AA")
  Path(
    data: valves_data,
    activity: valve.Move(valve),
    remaining_total_pressure: map.values(valves_data.valves)
    |> list.map(fn(v: valve.Valve) { v.rate })
    |> int.sum,
    previous_path: Error(0),
    releasable_pressure: 0,
    released_pressure: 0,
    elapsed_minutes: 0,
    open_valve_count: 0,
    opened_valves: [],
    valve: valve,
  )
}

fn may_release_max_pressure(
  activity: Path,
  max_pressure: Int,
  minutes_to_open: Int,
) {
  case max_pressure == 0 || activity.released_pressure == 0 {
    True -> True
    False -> {
      let mins = 30 - activity.elapsed_minutes - minutes_to_open
      let diff = max_pressure - activity.released_pressure
      // io.print(
      //   mins
      //   |> int.to_string,
      // )
      // io.print(" ")
      // io.print(
      //   diff
      //   |> int.to_string,
      // )
      // io.print(" ")
      case mins > 0 {
        True -> {
          let avg = diff / mins
          // io.print(
          //   avg
          //   |> int.to_string,
          // )
          // io.print(" ")
          // io.println("")
          avg <= activity.remaining_total_pressure
        }
        False -> {
          // io.println("")
          False
        }
      }
    }
  }
}

fn process_connections(
  path: Path,
  connections: List(String),
  valve: valve.Valve,
  max_pressure: Int,
  max_pressure_paths: map.Map(List(String), Path),
) -> #(Int, map.Map(List(String), Path)) {
  case connections {
    [] -> #(max_pressure, max_pressure_paths)
    [conn_id, ..rest] -> {
      case in_loop(path, conn_id) {
        True ->
          process_connections(
            path,
            rest,
            valve,
            max_pressure,
            max_pressure_paths,
          )
        False -> {
          let assert Ok(conn_valve) =
            path.data.valves
            |> map.get(conn_id)
          let move_activity = add_move_activity(path, conn_valve)

          case may_release_max_pressure(move_activity, max_pressure, 1) {
            True -> {
              let #(max_pressure, max_pressure_paths) =
                compute_max_pressure_released(
                  move_activity,
                  max_pressure_paths,
                  max_pressure,
                )

              process_connections(
                path,
                rest,
                valve,
                max_pressure,
                max_pressure_paths,
              )
            }
            False -> {
              process_connections(
                path,
                rest,
                valve,
                max_pressure,
                max_pressure_paths,
              )
            }
          }
        }
      }
    }
  }
}

pub fn compute_max_pressure_released(
  path: Path,
  max_pressure_paths: map.Map(List(String), Path),
  max_pressure: Int,
) -> #(Int, map.Map(List(String), Path)) {
  case path.elapsed_minutes {
    30 -> #(max_pressure, max_pressure_paths)
    _ -> {
      case path.activity {
        valve.Move(valve) -> {
          case
            valve.openable && !list.contains(path.opened_valves, valve.name)
          {
            True -> {
              let open_activity = add_open_activity(path, valve)

              let #(process, max_pressure_paths) = case
                map.get(max_pressure_paths, open_activity.opened_valves)
              {
                Ok(existing_path) -> {
                  case
                    open_activity.released_pressure < existing_path.released_pressure && open_activity.elapsed_minutes >= existing_path.elapsed_minutes
                  {
                    True -> #(False, max_pressure_paths)
                    False -> {
                      case
                        open_activity.released_pressure > max_pressure || {
                          open_activity.released_pressure == max_pressure && open_activity.elapsed_minutes < existing_path.elapsed_minutes
                        } || open_activity.elapsed_minutes < existing_path.elapsed_minutes
                      {
                        True -> #(
                          True,
                          map.insert(
                            max_pressure_paths,
                            open_activity.opened_valves,
                            open_activity,
                          ),
                        )
                        False -> {
                          #(
                            may_release_max_pressure(
                              open_activity,
                              max_pressure,
                              2,
                            ),
                            max_pressure_paths,
                          )
                        }
                      }
                    }
                  }
                }

                Error(_) -> {
                  #(
                    True,
                    map.insert(
                      max_pressure_paths,
                      open_activity.opened_valves,
                      open_activity,
                    ),
                  )
                }
              }

              case process {
                False -> {
                  #(max_pressure, max_pressure_paths)
                }
                True -> {
                  case
                    open_activity.open_valve_count == open_activity.data.open_valve_count
                  {
                    True -> {
                      #(
                        open_activity.released_pressure + {
                          open_activity.releasable_pressure * {
                            30 - open_activity.elapsed_minutes
                          }
                        },
                        max_pressure_paths,
                      )
                    }

                    False -> {
                      let #(new_max_pressure, max_pressure_paths) =
                        compute_max_pressure_released(
                          open_activity,
                          max_pressure_paths,
                          max_pressure,
                        )

                      process_connections(
                        path,
                        valve.connections,
                        valve,
                        new_max_pressure,
                        max_pressure_paths,
                      )
                    }
                  }
                }
              }
            }

            False ->
              process_connections(
                path,
                valve.connections,
                valve,
                max_pressure,
                max_pressure_paths,
              )
          }
        }

        valve.Open(valve) -> {
          process_connections(
            path,
            valve.connections,
            valve,
            max_pressure,
            max_pressure_paths,
          )
        }
      }
    }
  }
}

fn add_open_activity(path: Path, valve: valve.Valve) -> Path {
  let opened_valves =
    list.sort([valve.name, ..path.opened_valves], by: string.compare)

  Path(
    ..path,
    previous_path: Ok(path),
    activity: valve.Open(valve),
    releasable_pressure: path.releasable_pressure + valve.rate,
    remaining_total_pressure: path.remaining_total_pressure - valve.rate,
    released_pressure: path.released_pressure + path.releasable_pressure,
    elapsed_minutes: path.elapsed_minutes + 1,
    opened_valves: opened_valves,
    open_valve_count: path.open_valve_count + 1,
  )
}

fn add_move_activity(path: Path, valve: valve.Valve) -> Path {
  Path(
    ..path,
    previous_path: Ok(path),
    activity: valve.Move(valve),
    released_pressure: path.released_pressure + path.releasable_pressure,
    elapsed_minutes: path.elapsed_minutes + 1,
  )
}

fn in_loop(path: Path, id: String) -> Bool {
  case path.activity {
    valve.Move(valve) ->
      case valve.name == id {
        True -> True
        False -> {
          case path.previous_path {
            Ok(path) -> in_loop(path, id)
            Error(_) -> False
          }
        }
      }
    valve.Open(_) -> False
  }
}
