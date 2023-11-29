class Console {

    static func readLines() -> [String] {

        var lines: [String] = []

        while let input = readLine() {
            guard input != "" else {
                continue
            }
            lines.append(input)
        }

        return lines
    }

    static func readArray<T>(mapper: (String) -> T) -> [T] {
        var items: [T] = []

        while let input = readLine() {
            guard input != "" else {
                continue
            }
            items.append(mapper(input))
        }

        return items
    }

    static func readDictionary<K, V>(mapper: (String) -> V, key: (V)-> K) -> Dictionary<K,V> {
        var items: Dictionary<K,V> = [:]

        while let input = readLine() {
            
            let item = mapper(input)
            let key = key(item)
            items[key] = item
        }

        return items
    }

}
