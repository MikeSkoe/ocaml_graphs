let input = [
      [true; true; true; false; false];
      [true; true; true; true; false];
      [true; true; true; true; true; true; false; false; false];
      [true; false; false; false; false; false];
      [true; true; true; true; true; true; true; true; true; true; true; true; false];
      [true; false];
      [true; true; true; true; false; false];
]

let get_ratio =
      let rec iter (green, red) = function 
            | [] -> (green, red)
            | head :: tail -> (match head with
                  | true -> iter (green + 1, red) tail
                  | false -> iter (green, red + 1) tail
            )
      in
      iter (0, 0)

let get_percentage (green, red) =
      let green = float_of_int green in
      let red = float_of_int red in
      green /. (green +. red) *. 100.

let min_of_list =
      List.fold_left
      (fun acc cur ->
            if cur < acc
            then cur
            else acc
      )
      max_float

let min_ratio =
      input
      |> List.map get_ratio
      |> List.map get_percentage
      |> min_of_list

