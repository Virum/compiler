let fprintf = Format.fprintf


let parenthesise f =
  fprintf f "(%a)"


let make_format ~binding ~format_naive =
  let rec format ?(precedence=0) f node =
    let inner_precedence, left, right = match binding node with
      | `Left  n -> n, n, n + 1
      | `Right n -> n, n + 1, n
      | `None  n -> n, n, n
    in
    let format_rec f =
      format_naive f
        (format ~precedence:left)
        (format ~precedence:right)
    in
    if precedence > inner_precedence
      then parenthesise f format_rec node
      else format_rec f node
  in format
