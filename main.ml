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
      type t =
            | End
            | Node of index * t * t

      let empty = Node (-1, End, End)

      let rec has_key search_key = function 
            | End -> false
            | Node(key, inner, next) ->
                  if key = search_key
                  then true
                  else has_key key inner || has_key key next

      let rec add parent child = function
            | End -> Node (parent, Node(child, End, End), End)

            | Node (key, inner, next) when key = parent ->
                  Node (key, Node(child, inner, End), next) 

            | Node (key, inner, next) ->
                  if has_key parent inner
                  then Node (key, (add parent child inner), next)
                  else Node (key, inner, (add parent child next))
end

let int_of_pos (x, y) =
      if x < 0 || x > 5 || y < 0 || y > 5
      then -1
      else x + y * 6

let pos_of_int = function
      | -1 -> (-1, -1)
      | num -> (num mod 6, num / 6)

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

let flat_relation_graph =
      let xs = List.init (7 * 7) (fun x -> x mod 7) in
      let ys = List.init (7 * 7) (fun x -> x / 7) in

      List.fold_left2 (fun acc x y ->
            let left_pos = (x-1, y) in
            let curr_pos = (x, y) in
            let upper_pos = (x, y-1) in

            let tuple_of_points pos_a pos_b graph =
                  match value_of_pos pos_a, value_of_pos pos_b with
                  | (Outer, Black)
                  | (Black, Black) -> FlatGraph.add (int_of_pos pos_a) (int_of_pos pos_b) graph
                  | (Black, Outer) -> FlatGraph.add (int_of_pos pos_b) (int_of_pos pos_a) graph
                  | _ -> graph
            in
            acc
            |> tuple_of_points left_pos curr_pos
            |> tuple_of_points curr_pos upper_pos
      ) FlatGraph.empty xs ys

let rec tree_of_flat tree = function
      | FlatGraph.End -> tree
      | FlatGraph.Node(parent, children, next) -> 
            let sub_tree =
                  children
                  |> List.fold_left (fun acc child -> TreeGraph.(add parent child acc)) tree
            in
            tree_of_flat sub_tree next

let tree_relation_graph = tree_of_flat TreeGraph.empty flat_relation_graph

(*
let rec ger_result input = function
      | TreeGraph.End -> input
      | TreeGraph.Node (key, inner, next) ->
*)

(* convert tree graph to board OR override input board *)

