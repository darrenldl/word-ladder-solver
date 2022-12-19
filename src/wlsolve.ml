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
    Word_list.l

let args = ref []

let add_to_args x =
  args := x :: !args

let speclist = []

let usage_msg = ""

let () =
  Arg.parse speclist add_to_args usage_msg;
  let src, dst =
    match List.rev !args with
    | [x; y] -> x, y
    | _ -> (
        Printf.printf "Error: Too few arguments\n";
        exit 1
      )
  in
  match Db.lookup_int_of_string src db with
  | None ->
    Printf.printf "Error: %s is not a valid word\n" src;
    exit 1
  | Some id_x ->
    match Db.lookup_int_of_string dst db with
    | None ->
      Printf.printf "Error: %s is not a valid word\n" dst;
      exit 1
    | Some id_y ->
      Graph.find_path id_x id_y g
      |> Option.get
      |> List.iter (fun x ->
          Printf.printf "%s\n" (Option.get @@ Db.lookup_string_of_int x db);
        )
