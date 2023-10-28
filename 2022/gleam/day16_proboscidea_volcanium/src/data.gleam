import gleam/string
import gleam/list
import gleam/result
import valve
import gleam/set
import gleam/map
import gleam/int

pub type Data {
  Data(
    valves: map.Map(String, valve.Valve),
    openable_valves: set.Set(String),
    open_valve_count: Int,
  )
}

pub fn parse(input: String) -> Data {
  let valves =
    input
    |> string.split(on: "\n")
    |> list.map(fn(s) {
      let [_, name, _, _, "rate=" <> rate, _, _, _, _, ..rest] =
        string.split(s, " ")

      let pressure =
        rate
        |> string.drop_right(1)
        |> int.parse
        |> result.unwrap(0)

      let conns =
        rest
        |> list.map(fn(x) {
          case string.ends_with(x, ",") {
            True -> string.drop_right(x, 1)
            False -> x
          }
        })

      #(name, valve.Valve(name, pressure, conns, [], pressure > 0))
    })
    |> map.from_list

  valves
  |> map.map_values(fn(_, valve) {
    valve.Valve(
      ..valve,
      connected_valves: valve.connections
      |> list.map(fn(conn) {
        let assert Ok(v) = map.get(valves, conn)
        v
      }),
    )
  })

  // let name_to_index_map =
  //   records
  //   |> list.map(fn(tuple) { #(tuple.1, tuple.0) })
  //   |> map.from_list

  // let valves =
  //   records
  //   |> list.map(fn(tuple) {
  //     valve.Valve(
  //       id: tuple.0,
  //       name: tuple.1,
  //       rate: tuple.2,
  //       connections: tuple.3
  //       |> list.map(fn(conn) {
  //         name_to_index_map
  //         |> map.get(conn)
  //         |> result.unwrap(0)
  //       }),
  //     )
  //   })

  let openable_valve_count =
    valves
    |> map.filter(fn(_, valve) { valve.rate > 0 })
    |> map.size

  let openable_valves =
    valves
    |> map.filter(fn(_, valve) { valve.rate > 0 })
    |> map.values
    |> list.map(fn(valve) { valve.name })
    |> set.from_list

  Data(valves, openable_valves, openable_valve_count)
}
// pub fn get_valve(data: Data, valve_id: Int) -> valve.Valve {
//   let assert Ok(valve) =
//     data.valves
//     |> list.at(valve_id)
//   valve
// }

// pub fn get_valve_pressure(data: Data, valve_id: Int) -> Int {
//   let assert Ok(valve) =
//     data.valves
//     |> list.at(valve_id)
//   valve.rate
// }
