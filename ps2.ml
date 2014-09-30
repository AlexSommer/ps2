
(* PART 1: EXPRESSION TREES *)
type 'a exprTree = 
  | Val of 'a
  | Unop of ('a -> 'a) * 'a exprTree 
  | Binop of ('a -> 'a -> 'a) * 'a exprTree * 'a exprTree 

(* precondition: takes a valid expression tree
   postcondition: returns the number of function app in an exprTree*)
let rec count_ops (et : 'a exprTree) : int = 
  match et with 
 | Val x -> 0
 | Unop (f , t) -> 1 + count_ops(t)
 | Binop (f , t1 , t2) -> 1 + count_ops(t1) + count_ops(t2)
  
(* precondition: takes a positive integer n. If n is 
	negative, failure
   postcondition: returns an exprTree representing the execution of fact n (as defined in the
  	write-up for exercise 5)*)
let rec make_fact_tree (n : int) : int exprTree =
  if (n > 1 ) then
  Binop ( ( * ) , Val n , make_fact_tree (n-1) )
  else if (n = 1 || n = 0) then Val n
  else failwith "Failure"
  
(*precontition: takes a valid expression tree
  postcondition: computes the expression represented by [et]*)
let rec eval (et : 'a exprTree) : 'a =
  match et with 
  | Val x -> x
  | Unop (f , x) -> f (eval x) 
  | Binop (f , t1 , t2) -> f (eval t1) (eval t2) 

(* PART 2: FOLDING*)

(* precondition: takes a float list
   postcondition: returns a float representing to product of
   all the floats in the list *)
let product (lst : float list) : float =
  List.fold_left ( *. ) 1.0 lst

(* precondition: takes a list of strings
   postcondition: returns all strings concatonated together 
   starting from the left*)
let concat_left (lst : string list) : string = 
  List.fold_left (^) "" lst

(* precondition: takes a list of strings
   postcondition: returns all strings concatonated together 
   starting from the right*)
let concat_right (lst : string list) : string = 
  List.fold_right (^) lst ""

(* precondition: takes a function that takes an integer and an 'a and
   returns 'b,  and a list of 'a
   postcondition: returns a list of type 'b that is f applied to each element
	of lst with its index*)
let mapi_lst (f: (int -> 'a -> 'b)) (lst: 'a list) : 'b list =
  let mapi_list_helper (l,index) (arg:'a)=
    ((l @ [(f index arg)]), index + 1) in
  let (result,junk)= List.fold_left (mapi_list_helper) ([],0) (lst) in
  result

(* precondition: takes a list of strings
   postcondition: returns a lit of strings, numbered in
   outline forman *)
let outline (lst: string list) : string list =
  let outline_helper (index:int) (l:string) : string =
    (string_of_int (index + 1)) ^ (". ") ^ (l) in
  mapi_lst outline_helper lst

(* precondition: taks a function a list and an accumulator of the 
	types specified below
   postcondition: returns a list  representingthe accumulator at 
	every step of the fold starting from the right*)
let scan_right (f: 'a -> 'b -> 'b) (lst: 'a list) (acc: 'b) : 'b list =
  let scan_right_helper (ele:'a) (last,builder) =
    ((f ele last), (builder @ [(f ele last)])) in
  let (junk,result) = List.fold_right scan_right_helper (lst) (acc,[acc]) in
  result

(* precondition: taks a function a list and an accumulator of the 
	types specified below
   postcondition: returns a list  representingthe accumulator at 
	every step of the fold starting from the left *)
let scan_left (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
  let scan_left_helper (builder,last) (ele:'b) =
    (builder @ [(f last ele)], (f last ele)) in 
  let (result,junk) = (List.fold_left (scan_left_helper) ([acc], acc) (lst)) in
  result

(* requires: n >= 1 
   returns: the list [1;2;...;n] *)
let countup (n:int) : int list =
  (* tail-recursive helper function for countup:  
       accumulate the answer in l, 
       starting from n and working down *)
  let rec countup' i l =
    if i<=0 then l
    else countup' (i-1) (i::l)
  in countup' n []

(* precondition: values n are >= 1 
	postcondition: returns a list of all factorials 1 through n*)
let fact_list (n: int) : int list =  
  match (scan_left ( * ) (1) (countup n)) with
  | [] -> [1]
  | h::t -> t


(* PART 3: MATRICES *)

type vector = int list
type matrix = vector list

exception MatrixFailure of string

(* precondition: takes a vector list 
   postcondition: returns if that list is a valid matrix*)
let is_valid_matrix mtrx =
  let valid_helper value row =
    if (value = 0) then (List.length row) 
    else if (value != (List.length row)) then 
      raise (MatrixFailure "Invalid Input")
    else
      value
  in
  let result =  List.fold_left valid_helper 0 mtrx in
  if result > -1 then ()

   
(* precondition: matrix m is valid or will raise matrix failure
	postcondition: prints out the matrix *)
let show (m : matrix) : unit = 
  let () = is_valid_matrix m in
  let show_helper lst =
    print_string (String.concat " " (List.map string_of_int lst));
    print_newline ();
  in
  List.iter show_helper m

(* precondition: takes a valid matrix and a vector c that is the same 
   depth as m. if m is not valid or c is not the same depth, will
   raise matrix failure
   postcondition: return the matrix with the vector inserted as the left most
   column *)
let insert_col (m : matrix) (c : vector) : matrix = 
  let () = is_valid_matrix m in
  if (List.length c != List.length m) 
    then raise (MatrixFailure ("Not valid input") )else
  let f (index, mtrx) (row : vector) = 
    ((index + 1) , (mtrx @ [row @ [(List.nth c index)]])) in
  let (i , newMatrix ) = List.fold_left f (0, []) m in
  newMatrix

(* precondition: m is valid or else raises matrix failure
   postcondition:returns the matrix which is the transpose of m*)
let transpose (m : matrix) : matrix = 
  let () = is_valid_matrix m in
  let row_to_matrix r = 
    let f i lst = [i] :: lst in
    List.fold_right f r [] in
  let insert_col_modify mtrx row = 
    match mtrx with 
    | [] -> row_to_matrix row
    | _ -> insert_col mtrx row
  in
  List.fold_left insert_col_modify [] m 

(* precondition: m1 and m2 are valid or else matrix failure 
   postcondition: returns the sum of the matrices (using rules
   of regular matrix addition) *)
let add_matrices (m1 : matrix) (m2 : matrix) : matrix = 
  let () = is_valid_matrix m1 in
  let () = is_valid_matrix m2 in
  if (List.length m1 != List.length m2 
    || List.length (List.nth m1 0) != List.length (List.nth m2 0) ) then
    raise (MatrixFailure("Invalid Input")) 
  else 
    let add_rows rowm1 rowm2 = 
      let f (indx, lst) (ele : int) = 
        ((indx + 1) , lst @ [(ele + List.nth rowm2 indx)] ) in
      let (junk, row) = List.fold_left f (0, []) rowm1 in
      row in
    let add_helper (index, new_mtrx) row_m1 = 
       ((index+1), (new_mtrx @ [add_rows (row_m1) (List.nth m2 index)])) in
    let (junk, result) = List.fold_left add_helper ( 0 , []) m1 in
    result

(* precondition: length of list1 and list2 are equal *)
let multiply_and_add_lists (acc,row) list2 =
  let helper (acc,index) ele =
    match acc with
    | [x] -> ([x + (ele * (List.nth list2 index))],index+1)
    | _ -> ([0],0) in
  let (result,index) = List.fold_left helper ([0],0) row in
  (acc @ result,row)

(* precondition: m1 and m2 are valid or else matrix failure 
   postcondition: returns the product of the matrices (using rules
   of regular matrix multiplication) *)
let multiply_matrices (m1 : matrix) (m2 : matrix) : matrix = 
  let () = is_valid_matrix m1 in
  let () = is_valid_matrix m2 in
  if ((List.length (List.nth m1 0)) != (List.length m2)) then
    raise (MatrixFailure "Invalid Input")
  else
    let transposed = (transpose m2) in
    let multiply_helper acc rows =
      let (result,junk) = 
        List.fold_left multiply_and_add_lists ([],rows) transposed in 
      (acc @ [result]) in
    List.fold_left multiply_helper [] m1



(* PART 4: PATTERN MATCHING PROBLEMS*)
type pat =
  | WCPat (*short for "wildcard pattern"; equivalent to an underscore*)
  | VarPat of string
  | UnitPat
  | ConstPat of int
  | TuplePat of pat list
  | StructorPat of string * (pat option) (*Short for "constructor pattern"*)

type value = 
  | ConstVal of int
  | UnitVal
  | TupleVal of value list
  | StructorVal of string * (value option)

type bindings = (string * value) list option

(*1. *************************************************************************)

(* a counting function where f1 computes the assigned value for
   WCPat and f2 assigns values to variable patterns*)
let rec z f1 f2 p =
  let r = z f1 f2 in
    match p with
    | WCPat -> f1 ()
    | VarPat x -> f2 x
    | TuplePat ps -> List.fold_left (fun acc e -> (r e) + acc) 0 ps
    | StructorPat (_,Some p) -> r p
    | _ -> 0

(* precondition: takes a p of type pat
   postcondition: counts the number of wildcards that occur in a pattern*)
let count_wcs (p: pat) : int = 
  z (fun () -> 1) (fun x -> 0 ) p


(* precondition: takes a p of type pat
   postconditions: counts the number of wildcards in a pattern, 
   and adds that quantity to the sum of the lengths of the variable 
   names used in the pattern*)
let count_wcs_and_var_lengths (p: pat) : int = 
  z (fun () -> 1) (fun x -> String.length x) p


(*precondition: takes a string variable name and a pattern
 postcondition: counts how oftern a variable occurs in a pattern*)
let count_var (var_name: string) (p: pat) : int = 
  z (fun () -> 0) (fun x -> if (x = var_name) then 1 else 0) p
  

(*2. *************************************************************************)

(* precondition: takes a pattern
   postcondition: returns a list of variable names that
   occur within the pattern *)
let rec extract_names (p: pat) : string list = 
  let f lst ptrn = 
    lst @ extract_names(ptrn) in
  match p with 
  | VarPat x -> [x]
  | TuplePat ps -> List.fold_left f [] ps
  | StructorPat (_, Some x) -> extract_names x
  | _ -> []


(* precondition: takes a list 
   postcondition: returns true if the list has duplicates, 
   false if all elements are unique*)
let has_dups (l: 'a list) : bool = 
  let compare (count,head) ele =
    if (head = ele) then (~-1,head)
    else if (count = ~-1) then (~-1,head)
    else
      (0,head) in
  let rec dups_helper lst =
    match lst with
    | [] -> false
    | [h] -> false
    | hd::tl -> 
        let (value,junk) = List.fold_left compare (0,hd) tl in
        if (value = ~-1) then true
        else 
          (dups_helper tl) 
  in (dups_helper l)


(* precondition: takes a pattern 
   postcondition: returns true if all variable names in the 
   pattern are unique, false if there are duplicates *)
let all_vars_unique (p: pat) : bool = 
  let lst = extract_names p in
  has_dups( lst ) = false
  
(*3. *************************************************************************)

exception NoneReached 

(* precondition: takes a function and list of the types defined below
   postcondition: returns an 'b list option, where the list contains every function
    evaluated call. If any of the calls returned none, all_answers will return None*)
let all_answers (f: 'a -> 'b list option) (l: 'a list) : 'b list option =

  try
  (let apply_and_append (lst: 'b list) (ele : 'a) = 
    match f(ele) with
    | Some x -> x @ lst
    | None -> raise NoneReached in

  Some (List.fold_left apply_and_append [] l ))
   with NoneReached -> None

(*4. ************************************************************************)

(* precondition: takes a touple of value * pattern
   postcondition: returns Some list of bindings, if any, produced by the 
   match or None if they don't match *)
let rec match_pat (v,p) : bindings =

  let rec lst_bindings l vl pt = 

    match pt with 
    | WCPat -> l
    | VarPat str -> (str, vl) :: l
    | UnitPat -> if (vl = UnitVal) then l else raise NoneReached
    | ConstPat i -> if (vl = ConstVal i) then l  else raise NoneReached 
    | TuplePat lpat -> 
      (match v with 
      | TupleVal lval when List.length lpat = List.length lpat ->  

        List.fold_left2 lst_bindings l lval lpat
      |_ -> raise NoneReached )
    | StructorPat (str, Some ptrn) -> 
      (match vl with 
      | StructorVal (str, Some vlue) -> lst_bindings l vlue ptrn 
      |_ -> raise NoneReached )
    |StructorPat (str, None) -> 
      (match vl with 
      | StructorVal (str, None) -> []
      | _ -> raise NoneReached )
    in

    try (Some (lst_bindings [] v p ))
    with NoneReached -> None


(*5. *************************************************************************)
exception NoAnswer

(* precondition: takes a  function 'a -> 'b oprion and a 'a list 
   postcondition: returns the first succesful function call on the list.
   if none are succesful, returns None*)
let rec first_answer (f: 'a -> 'b option) (l: 'a list) =
  match l with  
  | [] -> raise NoAnswer
  | h::t -> 
    match f h with 
    | Some v -> v
    | None -> first_answer f t

(*6. *************************************************************************)

(* precondition: takes a value and a list of patterns
   postcondition: returns a list of any binding produced by the first pattern
   the value matches. If the value matches none of the bindings, returns 0 *)
let match_pats ((v: value), (ps: pat list)) : bindings =
  let f acc ptrn = 
    if List.length acc > 0 then acc
    else 
     match match_pat (v, ptrn) with 
     |Some lst -> acc @ lst
     |None -> acc 
    in 

  let bind_list = List.fold_left f [] ps in
   match bind_list with
   | [] -> None
   | x -> Some x
