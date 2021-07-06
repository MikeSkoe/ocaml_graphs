let input = [
      [1; 0; 0; 0; 0; 0];
      [0; 1; 0; 1; 1; 1];
      [0; 0; 1; 0; 1; 0];
      [1; 1; 0; 0; 1; 0];
      [1; 0; 1; 1; 0; 0];
      [1; 0; 0; 0; 0; 1];
]

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
      type index = int
      type t = Node of index * t list

      let empty = Node (-2, [])

      let rec has_key key (Node (parent, children)) =
            if key = parent
            then true
            else List.exists (has_key key) children

      let rec add parent child = function
            | Node (key, children) when key = parent -> Node (key, Node(child, []) :: children) 
            | Node (key, children) ->
                  if List.exists (has_key key) children
                  then Node (key, List.map (add parent child) children)
                  else Node (key, Node(parent, [Node(child, [])]) :: children)
end

let rec tree_of_flat tree = function
      | FlatGraph.End -> tree
      | FlatGraph.Node(parent, children, next) -> 
            let sub_tree =
                  children
                  |> List.fold_left (fun acc child -> TreeGraph.(add parent child acc)) tree
            in
            tree_of_flat sub_tree next

let int_of_pos (x, y) =
      if x < 0 || x > 5 || y < 0 || y > 5
      then -1
      else x + y * 6

type value =
      | Outer
      | White
      | Black

let value_of_pos (x, y) =
      try 
            let row = List.nth input x in
            match List.nth row y with
            | 0 -> White
            | 1 -> Black
            | _ -> Outer
      with
            | Failure _ -> Outer
            | Invalid_argument _ -> Outer

let test =
      let xs = List.init (7 * 7) (fun x -> x mod 7) in
      let ys = List.init (7 * 7) (fun x -> x / 7) in

      List.fold_left2 (fun acc x y ->
            let left_pos = (x-1, y) in
            let curr_pos = (x, y) in
            let upper_pos = (x, y-1) in

            print_endline @@ Printf.sprintf "(%d, %d)" x y;

            let tuple_of_points pos_a pos_b =
                  match value_of_pos pos_a, value_of_pos pos_b with
                  | (Outer, Black)
                  | (Black, Black) -> [(int_of_pos pos_a, int_of_pos pos_b)]
                  | (Black, Outer) -> [(int_of_pos pos_b, int_of_pos pos_a)]
                  | _ -> []
            in
            acc
            @ tuple_of_points left_pos curr_pos
            @ tuple_of_points curr_pos upper_pos
      ) [] xs ys


(* list of all coords *)

