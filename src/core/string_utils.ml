type chunk = Str of string | Num of string

let chunk_string s =
  let len = String.length s in
  let rec loop i acc_chunks current_chunk is_num =
    if i = len then
      let final_chunk =
        if is_num then Num current_chunk else Str current_chunk
      in
      List.rev (final_chunk :: acc_chunks)
    else
      let c = s.[i] in
      let c_is_num = match c with '0' .. '9' -> true | _ -> false in
      if current_chunk = "" then
        loop (i + 1) acc_chunks (String.make 1 c) c_is_num
      else if c_is_num = is_num then
        loop (i + 1) acc_chunks (current_chunk ^ String.make 1 c) is_num
      else
        let new_chunk =
          if is_num then Num current_chunk else Str current_chunk
        in
        loop (i + 1) (new_chunk :: acc_chunks) (String.make 1 c) c_is_num
  in
  if len = 0 then [] else loop 0 [] "" false

let compare_numeric_strings s1 s2 =
  let strip_zeros s =
    let rec aux i =
      if i < String.length s && s.[i] = '0' then aux (i + 1) else i
    in
    let start = aux 0 in
    if start = String.length s then "0"
    else String.sub s start (String.length s - start)
  in
  let s1' = strip_zeros s1 in
  let s2' = strip_zeros s2 in
  let len_cmp = compare (String.length s1') (String.length s2') in
  if len_cmp <> 0 then len_cmp else String.compare s1' s2'

let rec compare_chunks c1 c2 =
  match (c1, c2) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | hd1 :: tl1, hd2 :: tl2 ->
      let cmp =
        match (hd1, hd2) with
        | Str s1, Str s2 -> String.compare s1 s2
        | Num n1, Num n2 -> compare_numeric_strings n1 n2
        | Str _, Num _ -> 1 (* Numbers come before letters *)
        | Num _, Str _ -> -1
      in
      if cmp <> 0 then cmp else compare_chunks tl1 tl2

let natural_compare s1 s2 = compare_chunks (chunk_string s1) (chunk_string s2)
