let input = [
      [1; 0; 0; 0; 0; 0];
      [0; 1; 0; 1; 1; 1];
      [0; 0; 1; 0; 1; 0];
      [1; 1; 0; 0; 1; 0];
      [1; 0; 1; 1; 0; 0];
      [1; 0; 0; 0; 0; 1];
]

let int_of_pos (x, y) =
      if x < 0 || x > 5 || y < 0 || y > 5
      then -1
      else x + y * 6

module FlatGraph = struct
      type index = int

      type t =
            | End
            | Node of index * index list * t

      let empty = Node (-1, [], End)

      let rec add parent child = function
            | End -> Node (parent, [child], End)
            | Node (value, children, next) -> (
                  match value - parent with
                  | 0 -> Node (value, child :: children, next)
                  | diff when diff < 0 -> Node (value, children, add parent child next)
                  | _ -> Node (parent, [child], Node(value, children, next))
            )
end

module TreeGraph = struct
      type t = Node of index * t list

      let empty = Node (-2, [])


      (*
       * let has_key ...
       *
      let rec add parent child (Node(key, next_list)) =
            if key = parent
            then Node (key, Node(child, []))
            else (
                  if 
            )
      *)
end

