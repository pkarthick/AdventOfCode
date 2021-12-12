require "../day.cr"

class Graph
  property edges = Hash(String, Set(String)).new

  def addPath(s, e)
    edges[s] = Set(String).new unless edges.keys.includes?(s)
    edges[e] = Set(String).new unless edges.keys.includes?(e)

    edges[s] << e unless e == "start"
    edges[e] << s unless s == "start"
  end

  def all_paths_to_end(path : String, paths : Array(String), found_twice : Bool, all_paths : Array(Nil))
    if path == "end"
      all_paths << nil
    else
      edges[path]
        .select { |id| id.chars.all?(&.ascii_uppercase?) || !paths.includes?(id) || !found_twice }
        .each { |id|
          found_twice1 = found_twice || (id.chars.all?(&.ascii_lowercase?) && paths.includes?(id))
          all_paths_to_end(id, paths.clone << id, found_twice1, all_paths)
        }
      paths
    end
  end
end

puts Day.new(12, 2).execute { |input|
  
  graph = Graph.new

  input
    .lines
    .each { |l|
      s, e = l.split('-')
      graph.addPath(s, e)
    }

  all_paths = Array(Nil).new
  graph.all_paths_to_end("start", Array(String).new, false, all_paths)
  all_paths.size.to_s
}
