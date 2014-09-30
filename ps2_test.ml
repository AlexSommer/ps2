open Assertions
open Ps2

(*DO ANY OF THESE NEED FAIL WITHS OR ANYTHING LIKE THAT?*)
(* HOW TO TEST THE FACT TREE CORRECTLY? *)
(*what to do about invalid/valid matrices*)



(* tests for count_ops *)
TEST_UNIT "count_ops_test1" = 
    assert_true (count_ops (Val 3) = 0)
TEST_UNIT "count_ops_test2" = 
    assert_true (count_ops (Unop ((~-), Val 3)) = 1 )
TEST_UNIT "count_ops_test3" = 
    assert_true (count_ops (Binop ((+) , Val 3 , Val 4)) = 1)
TEST_UNIT "count_ops_test4" = 
    assert_true (count_ops (Binop((+), Val 3 , Unop((~-), Val 3))) = 2)
TEST_UNIT "count_ops_test5" = assert_true 
    (count_ops (Binop ((+),Val 3 ,Unop ((~-),Binop((/), Val 5, Val 2)) ))=3)

(*tests for make_fact_tree *)
(* TEST_UNIT "make_fact_tree" = assert_true ((make_fact_tree 3) = (Binop (( * ), Val 3, Binop (( * ), Val 2, Val 1)))) *)

(*NEED TO CHECK THIS FOR ARITHMETIC EXCEPTIONS?? DO WE NEED TO FAIL WITH??*)
(* test for eval *)
TEST_UNIT "eval_test1" = assert_true (eval (Val 3) = 3)
TEST_UNIT "eval_test2" = assert_true (eval (Unop ((~-), Val 5)) = ~-5)
TEST_UNIT "eval_test3" = assert_true (eval (Binop ((+) , Val 3 , Val 2)) = 5) 
(* TEST_UNIT "eval_test4" = 
    try (eval (Binop ((/) , Val 1, Val 0)))
    with Failure x -> assert_true (x = "Division_by_zero") *)

TEST_UNIT "eval_test5" = assert_true 
    (eval (Binop ((+.) , Val 3. , Unop ((~-.), 
      Binop((/.), Val 5., Val 2.)) )) = (1.0/.2.0))
TEST_UNIT "eval_test6" = assert_true (eval (Binop ((^) , Val "zar ", 
      Val "doz")) = ("zar doz"))
(* TEST_UNIT "eval_test7" = assert_true ((Binop ((&&), Val true, (Binop ((||) , (Unop ((not) , Val true)), Val true) ) ) ) = (true)) *)

(* tests for product *)
TEST_UNIT "product_test1" =
    assert_true ((product  [777.5; 4.]) = 3110.)
TEST_UNIT "product_test2" =
    assert_true ((product  []) = 1.)
TEST_UNIT "product_test3" =
    assert_true ((product  [3.]) = 3.)

(* tests for concat_left *)
TEST_UNIT "concat_left_test1" = 
    assert_true ((concat_left ["cs"; "3110"]) = "cs3110")
TEST_UNIT "concat_left_test2" = 
    assert_true ((concat_left []) = "")

(* tests for concat_right *)
TEST_UNIT "concat_right_test1" = 
    assert_true ((concat_right ["cs"; "3110"]) = "cs3110")
TEST_UNIT "concat_right_test2" = 
    assert_true ((concat_right []) = "")

(* tests for mapi_lst *)
TEST_UNIT "mapi_list_test1" =
    assert_true ((mapi_lst (+) [3;0;(-1);(-3)]) = [3;1;1;0])
TEST_UNIT "mapi_list_test2" =
    assert_true ((mapi_lst (-) [1;2;3;4]) = [(-1);(-1);(-1);(-1)])
TEST_UNIT "mapi_list_test3" =
    assert_true ((mapi_lst ( * ) [10;5;3;7]) = [0;5;6;21])
TEST_UNIT "mapi_list_test4" =
    assert_true ((mapi_lst (-) [1]) = [~-1])
TEST_UNIT "mapi_list_test5" =
    assert_true ((mapi_lst (/) []) = [])

(* tests for outline *)
TEST_UNIT "outline_test1" =
    assert_true ((outline ["point 1";"point 2";"point 3"]) 
        = ["1. point 1";"2. point 2";"3. point 3"])
TEST_UNIT "outline_test2" =
    assert_true ((outline ["blah blah"]) = ["1. blah blah"])
TEST_UNIT "outline_test3" =
    assert_true ((outline []) = [])

(* tests for scan_right *)
TEST_UNIT "scan_right_test1" =
    assert_true ((scan_right (+) [1;2;3] 0) = [0;3;5;6])
TEST_UNIT "scan_right_test2" =
    assert_true ((scan_right (^) ["zar"; "doz"] "swag") = 
        ["swag"; "dozswag"; "zardozswag"])

(* tests for scan_left *)
TEST_UNIT "scan_left_test1" =
    assert_true ((scan_left (+) 0 [1;2;3]) = [0;1;3;6])
TEST_UNIT "scan_left_test2" =
    assert_true ((scan_left (^) "swag" ["zar"; "doz"]) = 
        ["swag"; "swagzar"; "swagzardoz"])

(* tests for fact_list *)
TEST_UNIT "fact_list_test1" = 
    assert_true ((fact_list 1) = [1])
TEST_UNIT "fact_list_test2" = 
    assert_true ((fact_list 2) = [1;2])
TEST_UNIT "fact_list_test3" = 
    assert_true ((fact_list 3) = [1;2;6])
TEST_UNIT "fact_list_test4" = 
    assert_true ((fact_list 4) = [1;2;6;24])
TEST_UNIT "fact_list_test5" = 
    assert_true ((fact_list 5) = [1;2;6;24;120])

(* tests for insert_col *)
TEST_UNIT "insert_col_test1" = 
    assert_true ((insert_col [[1;2;3];[4;5;6]] [0;0]) = 
                [[1;2;3;0];[4;5;6;0]])
(* TEST_UNIT "insert_col_test2" =  tests for exception from invalid input
    assert_true ((insert_col [[1;2;3];[4;5;6]] [0;0;0]) = [[]]) *)
TEST_UNIT "insert_col_test3" = 
    assert_true ((insert_col [[1;2;3]] [(-4)]) = [[1;2;3;(-4)]])
TEST_UNIT "insert_col_test4" = 
    assert_true ((insert_col [[0]] [5]) = [[0;5]])
TEST_UNIT "insert_col_test5" = 
    assert_true ((insert_col [[]] [5]) = [[5]])

(* tests for transpose *)
TEST_UNIT "transpose_test1" = 
    assert_true ((transpose [[1;2;3];[4;5;6]]) = [[1;4];[2;5];[3;6]])
TEST_UNIT "transpose_test2" = 
    assert_true ((transpose [[1];[2]]) = [[1;2]])
TEST_UNIT "transpose_test3" =  (* transpose of a transpose is the original *)
    assert_true ((transpose [[1;2]]) = [[1];[2]])
TEST_UNIT "transpose_test4" = 
    assert_true (transpose(transpose [[1;2;3];[4;5;6]]) = [[1;2;3];[4;5;6]])

(* tests for add_matrices *)
TEST_UNIT "add_matrices_test1" = 
    assert_true ((add_matrices [[1;2;3];[4;5;6]] [[0;0;0];[1;2;3]]) = 
        [[1;2;3];[5;7;9]])
TEST_UNIT "add_matrices_test2" = 
    assert_true ((add_matrices [[1;0];[(-5);2]] [[0;0];[2;3]]) = 
        [[1;0];[(-3);5]])
TEST_UNIT "add_matrices_test3" = 
    assert_true ((add_matrices [[9]] [[10]]) = [[19]])

(* tests for multiply_matrices *)
TEST_UNIT "multiply_matrices_test1" =
    assert_true ((multiply_matrices [[1;2;3];[1;2;3]] [[2;2];[2;2];[2;2]])
        = ([[12; 12]; [12; 12]]))
TEST_UNIT "multiply_matrices_test2" =
    assert_true ((multiply_matrices [[1;2;3];[1;2;3];[1;2;3]] 
        [[2;2];[2;2];[2;2]]) =
             ([[12; 12];[12; 12];[12;12]]))
TEST_UNIT "multiply_matrices_test3" =
    assert_true ((multiply_matrices [[2]] [[6]])
        = ([[12]]))
TEST_UNIT "multiply_matrices_test4" =
    assert_true ((multiply_matrices [[10;8;7];[~-1;1;8]] [[0;4];[3;2];[6;7]])
        = ([[66; 105]; [51; 54]]))
TEST_UNIT "multiply_matrices_test1" =
    assert_true ((multiply_matrices [[1;2;3];[1;2;3]] 
            [[2;2;2];[2;2;2];[2;2;2]])
                = ([[12;12;12]; [12;12;12]]))

(* test for count_wcs *)
TEST_UNIT "count_wcs_test1" = 
    assert_true (count_wcs (WCPat) = 1)
TEST_UNIT "count_wcs_test2" = 
    assert_true (count_wcs (ConstPat 7) = 0)
TEST_UNIT "count_wcs_test3" = 
    assert_true (count_wcs (TuplePat [WCPat; ConstPat 7]) = 1)
TEST_UNIT "count_wcs_test4" = 
    assert_true (count_wcs (TuplePat[WCPat; WCPat; WCPat]) = 3)
TEST_UNIT "count_wcs_test5" = 
    assert_true (count_wcs (TuplePat[TuplePat[WCPat; UnitPat];WCPat; StructorPat ("x" , Some WCPat)]) = 3)

(* test for count_wcs_and_var_lengths *)
TEST_UNIT "count_wcs_and_var_lengths_test1" = 
    assert_true (count_wcs_and_var_lengths(WCPat) = 1)
TEST_UNIT "count_wcs_and_var_lengths_test2" = 
    assert_true (count_wcs_and_var_lengths(VarPat "3110makesuscry") = 14)
TEST_UNIT "count_wcs_and_var_lengths_test3" = 
    assert_true (count_wcs_and_var_lengths(TuplePat [WCPat; StructorPat ("str" , Some (VarPat "imtired"))]) = 8)
TEST_UNIT "count_wcs_and_var_lengths_test4" = 
    assert_true (count_wcs_and_var_lengths(ConstPat 7) = 0)

(* test for count_var *)
TEST_UNIT "count_var_test1" = 
    assert_true (count_var "x" (VarPat "x") = 1)
TEST_UNIT "count_var_test2" = 
    assert_true (count_var "x" (VarPat "notx") = 0)
TEST_UNIT "count_var_test3" = 
    assert_true (count_var "imtired" ( StructorPat ("str" , Some (VarPat "imtired"))) = 1)
TEST_UNIT "count_var_test4" = 
    assert_true (count_var "x" (TuplePat [ConstPat 7; VarPat "hello"; VarPat "x"; VarPat "x"]) = 2)

(*tests for has_dups helper function *)
TEST_UNIT "has_dups" =
    assert_true (has_dups ["hello";"hi";"hi"])
TEST_UNIT "has_dups" =
    assert_false (has_dups ["hello";"hi";"hiiiii"])
TEST_UNIT "has_dups" =
    assert_true (has_dups ["hello";"hello";"hi";"asd;lkfjasldf"])
TEST_UNIT "has_dups" =
    assert_false (has_dups ["hello"])
TEST_UNIT "has_dups" =
    assert_false (has_dups [])
TEST_UNIT "has_dups" =
    assert_true (has_dups ["hello";"alex";"alex";"sommer"])
TEST_UNIT "has_dups" =
    assert_true (has_dups ["hello";"hi";"I";"hate";"tests";"tests"])

(* test for all_vars_unique *)
TEST_UNIT "all_vars_unique_test1" =
    assert_true (all_vars_unique (ConstPat 7) )
TEST_UNIT "all_vars_unique_test2" = 
    assert_false (all_vars_unique (TuplePat [VarPat "x"; ConstPat 7; VarPat "x"]))
TEST_UNIT "all_vars_unique_test3" = 
    assert_false (all_vars_unique (TuplePat [WCPat; StructorPat ("str", Some (VarPat "x")); VarPat "x"])) 

(* test for all_answers *)
TEST_UNIT "all_answers_test1" =  
    assert_true (all_answers (fun x -> if x = 0 then None else Some [x]) [0;1;2;3] = None)
TEST_UNIT "all_answers_test2" =  
    assert_true (all_answers (fun x -> if x = 0 then None else Some [x]) [1;2;3] = Some [3;2;1])
TEST_UNIT "all_answers_test3" = 
    assert_true (all_answers (fun x -> if x = 0 then None else Some [x]) [] = Some [])

(* test for match_pat *)
TEST_UNIT "match_pat_test1" = 
    assert_true (match_pat (ConstVal 7 , WCPat) = Some [] )
TEST_UNIT "match_pat_test2" = 
    assert_true (match_pat (ConstVal 7, VarPat "x") = Some [("x", ConstVal 7)] )
TEST_UNIT "match_pat_test3" = 
    assert_true (match_pat (UnitVal, ConstPat 7) = None)
TEST_UNIT "match_pat_test4" = 
    assert_true 
    (match_pat (TupleVal [ConstVal 7; StructorVal ("str", Some (ConstVal 9))] , 
    TuplePat [ConstPat 7; StructorPat ("str" , Some (VarPat "x"))] ) 
    = Some [("x" , ConstVal 9)])
TEST_UNIT "match_pat_test5" = 
    assert_true (match_pat (TupleVal [ConstVal 7; ConstVal 9] , TuplePat [VarPat "x"; VarPat "y"]) 
        = Some [("y", ConstVal 9); ("x" , ConstVal 7)] )

(* test for first_answer *)
TEST_UNIT "first_answer_test1" =  
    assert_true (first_answer (fun x -> if x = 0 then None else Some [x]) [0;1;2;3] = [1])
TEST_UNIT "first_answer_test2" =  
    assert_true (first_answer (fun x -> if x = 0 then None else Some [x]) [1;2;3] = [1])
(* TEST_UNIT "first_answer_test3" = 
    assert_raises (all_answers (fun x -> if x = 0 then None else Some [x]) [] = Exception: "NoAnswer") *)

(* test for match_pats *)
TEST_UNIT "match_pats_test1" = 
    assert_true (match_pats (ConstVal 7, [ConstPat 7]) = None )
TEST_UNIT "match_pats_test3" = 
    assert_true (match_pats (ConstVal 7, [UnitPat; VarPat "x"]) 
        = Some [("x", ConstVal 7)]  )
TEST_UNIT "match_pats_test3" = 
    assert_true (match_pats (TupleVal [UnitVal; ConstVal 7] , [ TuplePat[WCPat; VarPat "x"]] )
    = Some [("x", ConstVal 7)])



(* Summary statement for test cases *)
let () = Pa_ounit_lib.Runtime.summarize()
