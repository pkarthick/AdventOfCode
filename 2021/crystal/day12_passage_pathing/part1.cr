require "../day.cr"

class Graph
  @edges = Hash(String, Set(String)).new

  def new
  end

  def edges
    @edges
  end

  def addPath(s, e)

    edges[s] = Set(String).new unless self.edges.keys.includes?(s)
    edges[e] = Set(String).new unless self.edges.keys.includes?(e)

    edges[s] << e unless e == "start"
    edges[e] << s unless s == "start"
  end

  def get_connected_caves(id)
    @edges[id]
  end

  def all_paths_to_end(path : String, paths : Array(String), all_paths : Array(Nil))
    if path == "end"
      all_paths << nil
    else

      get_connected_caves(path)
        .select { |id| id.chars.all?(&.ascii_uppercase?) || !paths.includes?(id) }
        .each { |id| all_paths_to_end(id, paths.clone << path, all_paths) }
    end
  end
end

puts Day.new(12, 1).execute { |input|

  graph = Graph.new()

  input
    .lines
    .each { |l|
      s, e = l.split('-')
      graph.addPath(s, e)
    }

  all_paths = Array(Nil).new
  graph.all_paths_to_end("start", [] of String, all_paths)

  all_paths.size.to_s
}
