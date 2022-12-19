type t = {
  count : int;
  lookup_string_of_int : string Int_map.t;
  lookup_int_of_string : int String_map.t;
  c0 : Int_set.t Char_map.t;
  c1 : Int_set.t Char_map.t;
  c2 : Int_set.t Char_map.t;
  c3 : Int_set.t Char_map.t;
}

let empty =
  {
    count = 0;
    lookup_string_of_int = Int_map.empty;
    lookup_int_of_string = String_map.empty;
    c0 = Char_map.empty;
    c1 = Char_map.empty;
    c2 = Char_map.empty;
    c3 = Char_map.empty;
  }

let add_to_char_map ~id c (m : Int_set.t Char_map.t) =
  let s =
    match Char_map.find_opt c m with
    | None -> Int_set.empty
    | Some s -> s
  in
  Char_map.add c (Int_set.add id s) m

let add_word (word : string) (t : t) : t =
  assert (String.length word = 4);
  match String_map.find_opt word t.lookup_int_of_string with
  | Some _ -> t
  | None -> (
      let id = t.count in
      let count = t.count + 1 in
      let lookup_string_of_int = Int_map.add id word t.lookup_string_of_int in
      let lookup_int_of_string = String_map.add word id t.lookup_int_of_string in
      let c0 = add_to_char_map ~id word.[0] t.c0 in
      let c1 = add_to_char_map ~id word.[1] t.c1 in
      let c2 = add_to_char_map ~id word.[2] t.c2 in
      let c3 = add_to_char_map ~id word.[3] t.c3 in
      { count;
        lookup_string_of_int;
        lookup_int_of_string;
        c0;
        c1;
        c2;
        c3;
      }
    )
