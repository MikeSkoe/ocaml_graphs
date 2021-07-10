(* https://www.youtube.com/watch?v=4tYoVx0QoN0&t=201s *)

let input = [
      [1; 0; 0; 0; 0; 0];
      [0; 1; 0; 1; 1; 1];
      [0; 0; 1; 0; 1; 0];
      [1; 1; 0; 0; 1; 0];
      [1; 0; 1; 1; 0; 0];
      [1; 0; 0; 0; 0; 1];
]

let size = 6
let size_ = 7
let bound = 5

module ListGraph = struct
      type t =
            | Node of int list * t
            | End

      let empty = Node ([], End)

      let tuple_mem (a, b) list =
            (List.mem a list) || (List.mem b list)

      let rec add (left, right) acc = function
            | End -> Node (left :: right :: acc, End)
            | Node (items, next) ->
                  if tuple_mem (left, right) items
                  then add (left, right) (items @ acc) next
                  else Node (items, add (left, right) acc next)

      let id = function
            | Node (items, next) -> Node (items, next)
            | End -> End

      let rec last_items = function
            | End -> []
            | Node (items, End) -> items
            | Node (_items, next) -> last_items next
end

module Pos = struct
      type t =
            | Black
            | White
            | Outer

      let int_of_pos (x, y) =
            if x < 0 || x > bound || y < 0 || y > bound
            then -1
            else x + y * size

      let left (x, y) = (x - 1, y)
      let top (x, y) = (x, y - 1)

      let t_of_pos input (x, y) =
            try
                  let row = List.nth input y in
                  match List.nth row x with
                  | 1 -> Black
                  | 0 -> White
                  | _ -> White
            with
                  | _ -> Outer
end

let connections_graph input =
      let xs = List.init (size_ * size_) (fun index -> index mod size_) in
      let ys = List.init (size_ * size_) (fun index -> index / size_) in
      let t_of_pos = Pos.t_of_pos input in
      
      List.fold_left2 (fun graph x y ->
            let cur = (x, y) in
            let left = (x, y) |> Pos.left in
            let top = (x, y) |> Pos.top in

            let _ = cur |> Pos.int_of_pos |> string_of_int |> print_endline in

            let conn a b = match (t_of_pos a, t_of_pos b) with
                  | (Outer, Black) -> ListGraph.add Pos.(int_of_pos a, int_of_pos b) []
                  | (Black, Black)
                  | (Black, Outer) -> ListGraph.add Pos.(int_of_pos b, int_of_pos a) []
                  | _ -> ListGraph.id
            in
            graph
            |> conn left cur
            |> conn top cur

      ) ListGraph.empty xs ys

let last_connections = ListGraph.last_items @@ connections_graph input

let result =
      List.init size (fun y ->
      List.init size (fun x ->
            if List.mem (Pos.int_of_pos (x, y)) last_connections
            then 1
            else 0
      ))

