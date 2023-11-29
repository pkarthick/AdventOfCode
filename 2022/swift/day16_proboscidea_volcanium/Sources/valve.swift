//
//  valve.swift
//  Day16
//
//  Created by Karthick Pachiappan on 22/11/23.
//

class Valve {
    let name: String
    let pressure: Int
    let connections: [String]
    var connectedValves: [Valve]
    
    let openable: Bool
    
    init(name: String, pressure: Int, connections: [String], connectedValves: [Valve]) {
        self.name = name
        self.pressure = pressure
        self.connections = connections
        self.connectedValves = connectedValves
        self.openable = pressure > 0
    }
    
    func populateConnectedValves(_ nameToValve: Dictionary<String, Valve>) {
        for connection in connections {
            connectedValves.append(nameToValve[connection]!)
        }
    }
    
}

func newValve(input: String) -> Valve {
    let tokens: [Substring] = input.split(whereSeparator: {[" ", ";", ",", "="].contains($0)})
    return Valve(name: String(tokens[1]), pressure: Int(tokens[5])!, connections: tokens.dropFirst(10).map({s in String(s)}), connectedValves: [])
}
