//
//  activity.swift
//  Day16
//
//  Created by Karthick Pachiappan on 23/11/23.
//

import Foundation

enum ActivityKind {
    case move, openValve
}

func createKey(_ openValves: [String]) -> String {
    var result = ""       
    openValves.forEach({ov in result += (ov + " ")})
    return result
}

func performMoveActivity(activity: Activity, connectedValve: Valve) -> Activity {
    return Activity(minutes: activity.minutes + 1, previousActivity: activity, openValves: activity.openValves, kind: ActivityKind.move, valve: connectedValve, openableCount: activity.openableCount, totalPressureReleased: activity.totalPressureReleased)
}

class Activity
{
    static var activities: Dictionary<[String], Activity> = [:]
    let minutes: Int
    let previousActivity: Activity?
    var openValves: [String]
    let kind: ActivityKind
        
    let valve: Valve
    let openableCount: Int
    let totalPressureReleased: Int
    

    init(minutes: Int, previousActivity: Activity?, openValves: [String], kind: ActivityKind, valve: Valve, openableCount: Int, totalPressureReleased: Int) {
        self.minutes = minutes
        self.previousActivity = previousActivity
        self.openValves = openValves
        self.kind = kind
        self.valve = valve
        self.openableCount = openableCount
        self.totalPressureReleased = totalPressureReleased
        
    }
    
    func valveCanBeOpened() -> Bool {
        !openValves.contains(valve.name) && valve.openable
    }

    func allValvesOpen() -> Bool {
        openValves.count == openableCount
    }

    func findMaxPressure() -> Int {
        
        if minutes == 30 {
            return totalPressureReleased
        } else {
            var maxPressure = totalPressureReleased
            if valveCanBeOpened() {
                let openAct = openActivity();
                if openAct.allValvesOpen() {
                    return openAct.totalPressureReleased
                } else {

                    if let cachedAct = Activity.activities[openAct.openValves] {
                        if cachedAct.totalPressureReleased > openAct.totalPressureReleased || (cachedAct.totalPressureReleased == openAct.totalPressureReleased && openAct.minutes >= cachedAct.minutes) {
                            return cachedAct.totalPressureReleased
                        }
                    }
                    Activity.activities[openAct.openValves] = openAct
                    maxPressure = openAct.findMaxPressure()
                }
            }
            
            for connectedValve in valve.connectedValves {
                if !inLoop(valveName: connectedValve.name) {
                    let pressure = moveActivity(connectedValve: connectedValve).findMaxPressure()
                    if pressure > maxPressure {
                        maxPressure = pressure
                    }
                }
            }
            
            
            return maxPressure
        }
        
    }
    
    func inLoop(valveName: String) -> Bool {
        
        var activityOpt: Activity? = self;
        
        while let activity = activityOpt {
            
            if activity.kind == ActivityKind.openValve {
                return false
            } else if activity.valve.name == valveName {
                return true
            }
            
            activityOpt = activity.previousActivity;
        }
        
        return false
    }
    
    func moveActivity(connectedValve: Valve) -> Activity
    {
       return Activity(minutes: minutes + 1, previousActivity: self, openValves: openValves, kind: ActivityKind.move, valve: connectedValve, openableCount: openableCount, totalPressureReleased: totalPressureReleased)
    }
    
    func openActivity() -> Activity
    {
        var openValves = openValves;
        
        if let position = openValves.firstIndex(where: {$0 > valve.name}){
            openValves.insert(valve.name, at: position);
        } else {
            openValves.append(valve.name);
        }
        
        let totalPressureReleased = totalPressureReleased + ((29 - minutes) * valve.pressure)
        
        return Activity(minutes: minutes + 1, previousActivity: self, openValves: openValves, kind: ActivityKind.openValve, valve: valve, openableCount: openableCount, totalPressureReleased: totalPressureReleased)
        
    }
}
