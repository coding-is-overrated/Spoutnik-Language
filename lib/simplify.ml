open Types

let is_inv u v =
  match (u, v) with u, UPow (v, -1) | UPow (u, -1), v -> v = u | _ -> false

(** Fonction de simplification avec passes d'optimisations
   donc recherche de point fixe (et potentiellement inexacte) *)
let simplify2 un =
  let rec break_pow u j =
    match j with
    | 0 -> UOne
    | -1 -> UPow (u, -1)
    | i ->
        if i > 0 then UProd (u, break_pow u (i - 1))
        else UProd (UPow (u, -1), break_pow u (i + 1))
  in
  let remove_div = function
    | UDiv (u1, u2) -> UProd (u1, UPow (u2, -1))
    | any -> any
  in
  let remove_pow = function
    | UPow (UPow (u, a), b) -> UPow (u, a * b)
    | any -> any
  in
  let pow_to_prod = function UPow (u, i) -> break_pow u i | any -> any in
  let flatten_pow = function
    | UPow (UProd (u1, u2), i) -> UProd (UPow (u1, i), UPow (u2, i))
    | any -> any
  in
  let prod_shift = function
    | UProd (UProd (u11, u12), u2) -> UProd (u11, UProd (u12, u2))
    | any -> any
  in
  let eliminate_ones = function
    | UProd (u, UOne) | UProd (UOne, u) -> u
    | UPow (UOne, -1) -> UOne
    | UPow (u, 1) -> u
    | any -> any
  in
  let search_inv =
    let rec aux v = function
      | UProd (u1, u2) ->
          if is_inv u1 v then (true, u2)
          else if is_inv u2 v then (true, u1)
          else
            let success, u2 = aux v u2 in
            (success, UProd (u1, u2))
      | any -> (false, any)
    in
    function
    | UProd (v, u) ->
        if is_inv v u then UOne
        else
          let success, u = aux v u in
          if success then u else UProd (v, u)
    | any -> any
  in
  let rec transform f = function
    | UDiv (a, b) -> f (UDiv (transform f a, transform f b))
    | UProd (a, b) -> f (UProd (transform f a, transform f b))
    | UPow (u, i) -> f (UPow (transform f u, i))
    | any -> any
  in

  let one_passes = [ ("remove_div", remove_div) ] in
  let passes =
    [
      ("remove_pow", remove_pow);
      ("pow_to_prod", pow_to_prod);
      ("flatten_pow", flatten_pow);
      ("prod_shift", prod_shift);
      ("eliminate_ones", eliminate_ones);
      ("search_inv", search_inv);
    ]
  in
  let first_pass =
    List.fold_left
      (fun u (_n, p) ->
        (*Printf.printf "%a\nwith %s: %!" Types.print_unit u _n;*)
        transform p u)
      un one_passes
  in
  let rec attractor init = function
    | [] -> init
    | (_n, p) :: q ->
        let neuf =
          (*Printf.printf "%a\nwith %s: %!" Types.print_unit init _n;*)
          transform p init
        in
        if neuf = init then attractor neuf q else attractor neuf passes
  in
  attractor first_pass passes

(** Fonction de simplification par enregistrement des exposants
   et recomposition en produit de puissances dont les unités
   sont ordonnées par ordre alphabétique *)
let simplify u =
  let map = Hashtbl.create 5 in
  let rec aux i = function
    | UBase s ->
        let exp = Option.value ~default:0 (Hashtbl.find_opt map s) in
        Hashtbl.replace map s (exp + i)
    | UVar x ->
        let s = "$" ^ x in
        let exp = Option.value ~default:0 (Hashtbl.find_opt map s) in
        Hashtbl.replace map s (exp + i)
    | UOne -> ()
    | UPow (u, n) -> aux (i * n) u
    | UDiv (a, b) ->
        let () = aux i a in
        aux (-i) b
    | UProd (a, b) ->
        let () = aux i a in
        aux i b
  in
  let () = aux 1 u in
  let lst = List.of_seq (Hashtbl.to_seq map) in
  let lst = List.sort (fun (x, _) (y, _) -> -String.compare x y) lst in
  let lst = List.filter (fun (_, i) -> i <> 0) lst in
  let leaf x =
    if String.starts_with ~prefix:"$" x then
      UVar (String.sub x 1 (String.length x - 1))
    else UBase x
  in
  match lst with
  | [] -> UOne
  | (v, i) :: q ->
      let init = if i = 1 then leaf v else UPow (leaf v, i) in
      List.fold_left
        (fun acc (v, i) ->
          UProd ((if i = 1 then leaf v else UPow (leaf v, i)), acc))
        init q
