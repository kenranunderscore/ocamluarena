include Map.Make (struct
    type t = Player.meta

    let compare (meta1 : Player.meta) (meta2 : Player.meta) =
      String.compare meta1.name meta2.name
    ;;
  end)

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
