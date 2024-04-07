include Map.Make (Player.Id)

let difference m1 m2 =
  merge
    (fun _player l r ->
      match l, r with
      | Some x, None -> Some x
      | _ -> None)
    m1
    m2
;;

let values m = bindings m |> List.map snd
