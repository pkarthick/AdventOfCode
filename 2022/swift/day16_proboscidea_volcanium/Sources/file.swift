//
//  File.swift
//  
//
//  Created by Karthick Pachiappan on 23/11/23.
//

import Foundation

class File {
    
    let fileName: String
    
    init(fileName: String) {
        self.fileName = fileName
    }
    
    func readDictionary<K, V>(mapper: (String) -> V, key: (V)-> K) -> Dictionary<K,V>? {
        var items: Dictionary<K,V> = [:]
                
        do {
            let contents = try String(contentsOfFile: "/Users/pkarthick/github/AdventOfCode/2022/swift/day16_proboscidea_volcanium/" + self.fileName)
            let lines = contents.split(separator: "\n", omittingEmptySubsequences: true)
                            lines.forEach({line in
                                let item = mapper(String(line))
                                let key = key(item)
                                items[key] = item
                            })
                            return items
        } catch {}
                
        return nil
    }
    
}
