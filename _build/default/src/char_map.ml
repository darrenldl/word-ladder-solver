include Map.Make (struct
    type t = char

    let compare = Char.compare
  end)
