pub type Valve {
  Valve(
    name: String,
    rate: Int,
    connections: List(String),
    connected_valves: List(Valve),
    openable: Bool,
  )
}

pub type Activity {
  Move(Valve)
  Open(Valve)
}

pub fn get_id(va: Activity) -> Valve {
  case va {
    Move(id) -> id
    Open(id) -> id
  }
}
