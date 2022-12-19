type t = {
  edges : Int_set.t Int_map.t;
}

let empty = {
  edges = Int_map.empty;
}

let add_directed_edge x y t =
  let s =
    match Int_map.find_opt x t.edges with
    | None -> Int_set.empty
    | Some s -> Int_set.add y s
  in
  let edges = Int_map.add x (Int_set.add y s) t.edges in
  { edges }

let add_edge x y t =
  t
  |> add_directed_edge x y
  |> add_directed_edge y x

let find_path (x : int) (y : int) (g : t) : int list option =
  let rec reconstruct_paths (rings : Int_set.t list) (from : Int_set.t) : int list Seq.t =
    match rings with
    | [] -> Seq.return []
    | r :: rs ->
      Int_set.to_seq from
      |> Seq.flat_map (fun x ->
          reconstruct_paths rs (Int_set.inter r (Int_map.find y g.edges))
          |> Seq.map (fun l -> x :: l)
        )
  in
  let rec aux
      (overall_explored : Int_set.t)
      (explored_rings : Int_set.t list)
      (cur_ring : Int_set.t)
    =
    if Int_set.is_empty cur_ring then
      None
    else
    if Int_set.mem y cur_ring then (
      match reconstruct_paths explored_rings Int_set.(add y empty) () with
      | Seq.Nil -> failwith "Unexpected failure in path reconstruction"
      | Seq.Cons (l, _) -> Some (List.rev l)
    ) else (
      let next_ring =
        Int_set.to_seq cur_ring
        |> Seq.map (fun x -> Int_map.find x g.edges)
        |> Seq.fold_left Int_set.union Int_set.empty
        |> (fun s -> Int_set.diff s overall_explored)
      in
      aux
        (Int_set.union cur_ring overall_explored)
        (cur_ring :: explored_rings)
        next_ring
    )
  in
  aux Int_set.empty [] Int_set.(add x empty)
