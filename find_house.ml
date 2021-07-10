(* https://www.youtube.com/watch?v=rw4s4M3hFfs *)

module Block = struct 
      type t = {
            gym: bool;
            school: bool;
            store: bool;
      }

      let make gym school store = { gym; school; store }

      let has_gym item = item.gym = true
      let has_school item = item.school = true
      let has_store item = item.store = true
end

module BlockValue = struct
      type t = {
            gym: int;
            school: int;
            store: int;
      }

      let make gym school store = { gym; school; store }

      let max_value item =
            item.gym
            |> max item.school
            |> max item.store
end

module Indexer = struct
      type t = int list

      let id a = a

      let get_indexes check list =
            let (_index, result) = List.fold_left
                  (fun (index, acc) item ->
                        if check item
                        then (index + 1, index :: acc)
                        else (index + 1, acc)
                  )
                  (0, [])
                  list
            in
            result

      let shortest_dist index =
            List.fold_left
                  (fun acc cur ->
                        let dist = abs @@ index - cur in
                        if dist < acc
                        then dist
                        else acc
                  )
                  max_int
end

let input = Block.[
      make false true false;
      make true false false;
      make true true false;
      make false true true;
]

let gym_indexes = Indexer.get_indexes Block.has_gym input
let school_indexes = Indexer.get_indexes Block.has_school input
let store_indexes = Indexer.get_indexes Block.has_store input

let blocks = List.(init (length input) Indexer.id)

let block_values = List.map
      (fun block_index ->
            let shortest_dist = Indexer.shortest_dist block_index in
            BlockValue.make
                  (shortest_dist gym_indexes)
                  (shortest_dist school_indexes)
                  (shortest_dist store_indexes)
      )
      blocks

let max_values = List.map BlockValue.max_value block_values

