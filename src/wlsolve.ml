let db = List.fold_left (fun db word ->
    Db.add_word word db)
    Db.empty
    Word_list.l

let g = List.fold_left (fun g word ->
    let id = Option.get @@ Db.lookup_int_of_string word db in
    Db.adjacent_words id db
    |> Int_set.to_seq
    |> Seq.fold_left (fun g x ->
        Graph.add_edge id x g)
      g
  )
    Graph.empty
