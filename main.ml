let input = [
      [1; 0; 0; 0; 0; 0];
      [0; 1; 0; 1; 1; 1];
      [0; 0; 1; 0; 1; 0];
      [1; 1; 0; 0; 1; 0];
      [1; 0; 1; 1; 0; 0];
      [1; 0; 0; 0; 0; 1];
]

let add_unique lst value =
      if List.mem value lst
      then lst
      else value :: lst

let merge_lists = List.fold_left add_unique

module FlatGraph = struct
      type index = int

      type t =
            | End
            | Node of index * index list * t

      let root = 999

      let empty = Node (1, [root], End)

      let rec add parent child = function
            | End -> Node (parent, [child], End)
            | Node (value, children, next) -> (
                  match value - parent with
                  | 0 -> Node (value, add_unique children child, next)
                  | diff when diff > 0 -> Node (value, children, add parent child next)
                  | _ -> Node (parent, [child], Node(value, children, next))
            )

      let rec list_of_graph acc = function
            | End -> acc
            | Node (value, children, next) ->
                  let acc =
                        if List.mem value acc
                        then merge_lists List.(sort (fun a b -> b - a) children) acc
                        else acc
                  in
                  list_of_graph acc next
end

let int_of_pos (x, y) =
      if x < 0 || x > 5 || y < 0 || y > 5
      then FlatGraph.root
      else x + y * 6

let pos_of_int = function
      | num when num = FlatGraph.root -> FlatGraph.(root, root)
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

let res_list = FlatGraph.list_of_graph [FlatGraph.root] flat_relation_graph

let result = 
      let (<&&>) = List.init in
      6 <&&> fun x ->
      6 <&&> fun y ->
            let result_value = function
                  | int_pos when List.mem int_pos res_list -> 1
                  | _ -> 0
            in
            (x, y)
            |> int_of_pos
            |> result_value

(*
let rec ger_result input = function
      | TreeGraph.End -> input
      | TreeGraph.Node (key, inner, next) ->
*)

(* convert tree graph to board OR override input board *)

