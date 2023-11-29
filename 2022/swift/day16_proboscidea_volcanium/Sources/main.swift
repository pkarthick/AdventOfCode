//
//  main.swift
//  Day16
//
//  Created by Karthick Pachiappan on 22/11/23.
//

import Foundation

var nameToValve: Dictionary<String, Valve> = File(fileName: "puzzle").readDictionary(mapper: newValve, key:{valve in valve.name})!

// let nameToValve = Dictionary(uniqueKeysWithValues: valves.map({($0.name, $0)}))

for valve in nameToValve.values {
    valve.populateConnectedValves(nameToValve)
}

let valve = nameToValve["AA"]!
let openableCount = nameToValve.values.filter({$0.openable}).count

let activity = Activity(minutes: 0, previousActivity: nil, openValves: [], kind: ActivityKind.move, valve: valve, openableCount: openableCount, totalPressureReleased: 0)

let pressure = activity.findMaxPressure()

print(pressure)

