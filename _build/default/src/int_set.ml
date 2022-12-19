include Set.Make (struct
    type t = int

    let compare = Int.compare
  end)
