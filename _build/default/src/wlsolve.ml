let db = List.fold_left (fun db word ->
  Db.add_word word db)
Db.empty
Word_list.l

