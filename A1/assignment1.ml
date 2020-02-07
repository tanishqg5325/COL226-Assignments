type vector = float list
type matrix = float list list

(* Exceptions *)
exception InvalidInput
exception UnequalVectorSize
exception UnequalMatrixShape
exception IncompatibleMatrixShape
exception SingularMatrix

(* returns the dimension of a given vector *)
let rec vdim (v:vector): int = match v with
    [] -> 0
  | x::xs -> 1 + (vdim xs);;

(* given a dimension n > 0, returns the zero vector of that dimension *)
let rec mkzerov (n:int): vector =
  if n < 0 then raise InvalidInput
  else if(n = 0) then []
  else 0.::mkzerov (n - 1);;

(* checks if given vector is a zero vector *)
let rec iszerov (v:vector): bool = match v with
    [] -> true
  | x::xs -> (x = 0.) && (iszerov xs);;

(* checks if given vector is empty *)
let isEmpty (v:vector): bool = match v with
    [] -> true
  | _ -> false;;

(* adds two vectors v1 and v2 (of the same dimension) *)
let rec addv (v1:vector) (v2:vector): vector = match v1 with
    [] -> if (isEmpty v2) then []
          else raise UnequalVectorSize
  | x::xs -> if (isEmpty v2) then raise UnequalVectorSize
              else (x +. (List.hd v2))::addv xs (List.tl v2);;

(* given a scalar c and a vector v, performs the scalar multiplication *)
let rec scalarmultv (c:float) (v:vector): vector = match v with
    [] -> []
  | x::xs -> (x *. c)::scalarmultv c xs;;

(* given two vectors v1 and v2 of same dimension, returns their dot product v1 . v2 *)
let rec dotprodv (v1:vector) (v2:vector): float = match v1 with
    [] -> if (isEmpty v2) then 0.
          else raise UnequalVectorSize
  | x::xs -> if (isEmpty v2) then raise UnequalVectorSize
              else (x *. (List.hd v2)) +. dotprodv xs (List.tl v2);;

(* given two vectors v1 and v2 in 3 dimensions, returns their cross product v1 x v2 *)
let rec crossprodv (v1:vector) (v2:vector): vector =
  if (vdim v1) <> 3 || (vdim v2) <> 3 then raise InvalidInput
  else match v1 with 
      a1::a2::a3::x1 -> (match v2 with 
          b1::b2::b3::x2 -> [a2 *. b3 -. a3 *. b2; a3 *. b1 -. a1 *. b3; a1 *. b2 -. a2 *. b1]
        | _ -> [])
    | _ -> [];;

(* checks if given matrix is invalid i.e. whether there exists rows with different
   number of columns *)
let isinvalidm (m:matrix): bool = match m with
    [] -> false
  | x::xs ->
      let rec isvalid m l = match m with
          [] -> true
        | x::xs -> if (vdim x) <> l then false
                    else isvalid xs l
      in not (isvalid xs (vdim x));;

(* returns the dimensions of a given matrix *)
let rec mdim (m:matrix): int*int =
  if (isinvalidm m) then raise InvalidInput
  else (List.length m, List.length (List.hd m));;

(* given a dimension m_, n_ > 0, returns the zero m_ x n_ matrix *)
let rec mkzerom (m_:int) (n_:int): matrix =
  if (m_ < 0 || n_ < 0) then raise InvalidInput
  else if m_ = 0 then []
  else (mkzerov n_)::mkzerom (m_ - 1) n_;;

(* checks if a given matrix is a  zero matrix *)
let rec iszerom (m:matrix): bool =
  if (isinvalidm m) then raise InvalidInput
  else 
    let rec iszeromutil m = match m with
        [] -> true
      | x::xs -> (iszerov x) && (iszeromutil xs)
    in iszeromutil m;;

(* given a dimension m > 0, returns the unit m x m (square) matrix *)
let rec mkunitm (m_:int): matrix =
  if m_ < 0 then raise InvalidInput
  else if m_ = 0 then []
  else
    let rec prependzero mat = match mat with
        [] -> []
      | x::xs -> (0.::x)::prependzero xs
    in (1.::mkzerov (m_ - 1))::prependzero (mkunitm (m_ - 1));;

(* ruturns the matrix after removing first column *)
let rec removeFirstColumn (m:matrix): matrix = match m with
    [] -> []
  | x::xs -> (List.tl x)::removeFirstColumn xs;;

(* checks if a given matrix is a unit (square) matrix *)
let rec isunitm (m:matrix): bool =
  if (isinvalidm m) then raise InvalidInput
  else if (List.length m) <> (List.length (List.hd m)) then false 
  else
    let rec isunitmutil m = match m with
        [] -> true
      | x::xs -> 
        if((List.hd x) <> 1.) then false
        else if(iszerov (List.tl x) = false) then false
        else
          let rec iszerovertical mat = match mat with
              [] -> true
            | x::xs -> ((List.hd x) = 0.) && (iszerovertical xs)
          in
          if (iszerovertical xs) = false then false
          else isunitmutil (removeFirstColumn xs)
    in isunitmutil m;;

(* adds two matrices m1 and m2 (of the same dimensions) *)
let rec addm (m1:matrix) (m2:matrix): matrix =
  if (isinvalidm m1) || (isinvalidm m2) then raise InvalidInput
  else if (List.length m1) <> (List.length m2) then raise UnequalMatrixShape
  else if (List.length (List.hd m1)) <> (List.length (List.hd m2)) then raise UnequalMatrixShape
  else
    let rec addmutil m1 m2 = match m1 with
        [] -> []
      | x::xs -> (addv x (List.hd m2))::addmutil xs (List.tl m2)
    in addmutil m1 m2;;

(* given a scalar c and a matrix m, performs the scalar multiplication *)
let rec scalarmultm (c:float) (m:matrix): matrix = 
  if (isinvalidm m) then raise InvalidInput
  else
    let rec scalarmultmutil c m = match m with
        [] -> []
      | x::xs -> (scalarmultv c x)::scalarmultmutil c xs
    in scalarmultmutil c m;;

(* given a matrix, returns the first column of matrix as a vector *)
let rec getFirstColumn (m:matrix): vector = match m with
    [] -> []
  | x::xs -> (List.hd x)::getFirstColumn xs;;

(* given a vector v and a matrix m, multiplies v (1 X a) with m (a X b) to give a 
    vector (1 X b) *)
let rec multvm (v:vector) (m:matrix): vector = 
  if List.length (List.hd m) = 0 then []
  else
    (dotprodv v (getFirstColumn m))::multvm v (removeFirstColumn m);;

(* multiply two matrices m1 and m2 (assuming their dimensions allow them to be multiplied *)
let rec multm (m1:matrix) (m2:matrix): matrix =
  if (isinvalidm m1) || (isinvalidm m2) then raise InvalidInput
  else if List.length (List.hd m1) <> List.length m2 then raise IncompatibleMatrixShape
  else
    let rec multmutil m1 m2 = match m1 with
        [] -> []
      | x::xs -> (multvm x m2)::multmutil xs m2
    in multmutil m1 m2;;

(* transpose a given matrix *)
let rec transm (m:matrix): matrix = 
  if (isinvalidm m) then raise InvalidInput
  else
    let rec transmutil m = 
      if(List.length (List.hd m) = 0) then []
      else (getFirstColumn m)::transmutil (removeFirstColumn m)
    in transmutil m;;

(* given a matrix, returns a row with non-zero entry in the first column *)
let rec getNonZeroRow (m:matrix): vector = match m with
    [] -> []
  | x::xs -> if (List.hd x <> 0.) then x else getNonZeroRow xs;;

(* swaps the elements u and v in list l *)
let rec list_swap l u v = match l with
    [] -> []
  | x::xs -> (if x = u then v
            else if x = v then u
            else x)::(list_swap xs u v);;

(* given a matrix, returns the matrix after making the entry in first column and first row 
    of matrix non zero by swapping it with a row present below in the matrix *)
let makeFirstRowNonZero (m:matrix): matrix = list_swap m (List.hd m) (getNonZeroRow m);;

(* given a matrix and a vector, make first column of matrix zero by applying elementary row 
    transformations with the help of v *)
let rec rowOperations (m:matrix) (v:vector): matrix = match m with
    [] -> []
  | x::xs -> 
    let mu = -.(List.hd x) /. (List.hd v) in
    addv x (scalarmultv mu v)::rowOperations xs v;;

(* compute the determinant of a matrix (assuming it is a square matrix) *)
let rec detm (m:matrix): float =
  if (isinvalidm m) then raise InvalidInput
  else if (List.length m) <> (List.length (List.hd m)) then raise InvalidInput
  else 
    let rec detmutil m = match m with
        [] -> 1.
      | x::xs ->
          if(iszerov (getFirstColumn m) = true) then 0.
          else if (List.hd (List.hd m)) <> 0. then
            (List.hd x) *. detmutil (removeFirstColumn (rowOperations xs x))
          else match (makeFirstRowNonZero m) with
              [] -> 0.
            | x::xs -> -. (List.hd x) *. detmutil (removeFirstColumn (rowOperations xs x))
    in detmutil m;;

(* given a matrix and a vector, prepends the vector as the first column of matrix *)
let rec addColumn (m:matrix) (v:vector): matrix = match m with
    [] -> []
  | x::xs -> ((List.hd v)::x)::addColumn xs (List.tl v);;

(* given two matrices m1 and m2, returns the augmented matrix m1 | m2 *)
let rec createAugmented (m1:matrix) (m2:matrix): matrix = 
  if (List.length (List.hd m1) = 0) then m2
  else addColumn (createAugmented (removeFirstColumn m1) m2) (getFirstColumn m1);;

(* given a matrix, returns the submatrix with row numbers >= row *)
let rec getMatrixBelow (m:matrix) (row:int): matrix = match m with
    [] -> []
  | x::xs -> if (row = 0) then m else getMatrixBelow xs (row-1);;

(* given a matrix, returns the submatrix with row numbers < row *)
let rec getMatrixAbove (m:matrix) (row:int): matrix = match m with
    [] -> []
  | x::xs -> if (row = 0) then [] else x::getMatrixAbove xs (row-1);;

(* given a matrix m, returns the matrix after dividing first row by the entry in first column *)
let normalizeFirstRow (m:matrix): matrix = match m with
    [] -> []
  | x::xs -> (scalarmultv (1. /. (List.hd x)) x)::xs;;

(* given an augmented matrix, perform elemantry row operations to make first column all zero
    except row which is made unity *)
let rec solveInv (m:matrix) (row:int): matrix = 
  if row = (List.length m) then m
  else
    let above = getMatrixAbove m row in
    match normalizeFirstRow (makeFirstRowNonZero (getMatrixBelow m row)) with
        [] -> m
      | x::xs -> solveInv (removeFirstColumn ((rowOperations above x) @ (x::(rowOperations xs x)))) (row+1);;

(* return the inverse of a given matrix (if defined) *)
let rec invm (m:matrix): matrix =
  if ((detm m) = 0.) then raise SingularMatrix
  else solveInv (createAugmented m (mkunitm (List.length m))) 0;;

(* given a matrix and column number, returns the matrix after removing that column *)
let rec removeIthColumn (m:matrix) (i:int): matrix = 
  if i = 0 then removeFirstColumn m
  else addColumn (removeIthColumn (removeFirstColumn m) (i-1)) (getFirstColumn m);;

(* computes the ith entry of cross product of vectors in matrix m *)
let rec crossprodvith (m:matrix) (i:int): vector = 
  if i = List.length (List.hd m) then []
  else if (i mod 2 = 0) then (detm (removeIthColumn m i))::crossprodvith m (i+1)
  else (-.(detm (removeIthColumn m i)))::crossprodvith m (i+1);;

(* cross product of n-1 vectors with dimension n *)
let rec crossprodvn (m:matrix): vector = 
  if (isinvalidm m) then raise InvalidInput
  else if List.length (List.hd m) <> (List.length m + 1) then raise InvalidInput
  else crossprodvith m 0;;

(*
vdim [1.; 2.; 3.; 4.];;
vdim [7.];;

mkzerov (-1);;
mkzerov 1;;
mkzerov 5;;

iszerov [0.];;
iszerov [0.; 0.; 0.];;
iszerov [0.; 3.];;
iszerov [4.; 0.; 0.];;

addv [1.; 2.; 3.; 4.] [10.; 12.; 17.; 9.];;
addv [1.; 2.; 3.; 4.] [1.; 2.; 3.; 4.; 5.];;
addv [1.; 2.; 3.; 4.; 5.] [1.; 2.; 3.; 4.];;
addv [3.] [2.];;

scalarmultv 0. [10.; 12.; 17.; 9.];;
scalarmultv 1.5 [10.; 12.; 17.; 9.];;

dotprodv [1.; 2.; 3.; 4.] [1.; 2.; 3.; 4.; 5.];;
dotprodv [1.; 2.; 3.; 4.; 5.] [1.; 2.; 3.; 4.];;
dotprodv [1.; 2.; 3.; 4.] [10.; 12.; 17.; 9.];;
dotprodv [1.; 2.; 3.] [-10.; 12.; 17.];;

crossprodv [1.; 2.; 3.] [-10.; 12.; 17.];;
crossprodv [1.; 2.] [-10.; 12.; 17.];;
crossprodv [1.; 2.; 3.] [-10.; 12.];;
crossprodv [1.; 2.] [-10.; 12.];;
crossprodvn [[1.; 2.; 3.]; [-10.; 12.; 17.]];;
crossprodvn [[2.; 5.]];;
crossprodvn [[1.; 2.; 3.; 4.]; [3.; 2.; 6.; 1.]; [4.; 5.; 2.; 0.]];;

mdim [[1.; 2.; 3.]; [4.; 5.]];;
mdim [[2.]];;
mdim [[3.; 7.; 0.]];;
mdim [[3.; 7.; 0.]; [3.; 7.; 0.]];;

mkzerom 1 1;;
mkzerom 4 2;;
mkzerom (-5) 1;;
mkzerom 3 3;;

iszerom (mkzerom 1 1);;
iszerom (mkzerom 4 2);;
iszerom (mkzerom 3 3);;
iszerom (mkzerom 1013 970);;
iszerom [[0.; 0.; 0.]; [0.; 1.; 0.]];;
iszerom [[0.; 0.; 0.]; [0.; 0.; 1.]];;
iszerom [[0.; 0.; 0.]; [0.; 0.]];;
iszerom [[3.]];;

mkunitm 1;;
mkunitm (-2);;
mkunitm (4);;

isunitm (mkunitm 1);;
isunitm (mkunitm 4);;
isunitm (mkunitm 519);;
isunitm (mkunitm 2020);;
isunitm [[1.; 0.; 0.]; [0.; 1.; 0.]];;
isunitm [[1.; 0.; 0.]; [0.; 1.; 0.;]; [0.; 1.; 0.]];;
isunitm [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 1.; 1.]];;
isunitm [[0.]];;

addm (mkunitm 3) (mkunitm 3);;
addm (mkunitm 3) (mkunitm 2);;
addm (mkunitm 3) [[0.; 1.; 0.]];;
addm [[0.; 1.; 0.]] (mkunitm 3);;
addm [[3.; 4.]; [24.; 75.]] [[32.; 1.]; [13.; 34.]];;

scalarmultm 5. (mkunitm 4);;
isunitm (scalarmultm 0.5 (scalarmultm 2. (mkunitm 973)));;
scalarmultm 1. [[1.; 2.;]; [4.]];; 

transm [[1.; 2.; 3.]; [4.; 5.; 6.]];;
transm [[1.; 2.; 3.]; [4.; 5.]];;
transm [[3.]];;
transm [[1.; 2.]];;
transm[[2.]; [5.]];;

multm [[1.; 2.; 3.]; [4.; 5.; 6.;]; [7.; 8.; 9.;]] [[1.; 2.; 3.]; [4.; 5.; 6.;]; [7.; 8.; 9.;]];;
multm [[1.; 2.; 3.]; [4.; 5.; 6.;]; [7.; 8.; 9.;]] [[1.; 2.; 3.]; [4.; 5.; 6.;]; [7.; 8.]];;
multm [[1.; 2.; 3.]; [4.; 6.;]; [7.; 8.; 9.;]] [[1.; 2.; 3.]; [4.; 5.; 6.;]; [7.; 8.; 9.;]];;
multm [[1.; 2.; 3.]; [4.; 5.; 6.;]; [7.; 8.; 9.;]] [[1.; 2.]; [4.; 5.]];;
isunitm (multm (mkunitm 200) (mkunitm 200));;
multm [[2.]] [[5.]];;
multm [[1.; 4.; 3.]; [2.; 7.; -3.]] (transm [[1.; 4.; 3.]; [2.; 9.; -3.]]);;
multm (transm [[1.; 4.; 3.]; [2.; 7.; -3.]]) [[1.; 4.; 3.]; [2.; 9.; -3.]];;

detm [[1.]];;
detm [[0.]];;
detm [[3.]];;
detm [[1.; 2.]];;
detm [[3.; 4.]; [5.]];;
detm (addm (mkunitm 20) (mkunitm 20));;
detm (mkunitm 100);;
detm (mkzerom 1000 1000);;

let mat = [[1.; 2.; 3.]; [0.; 1.; 4.]; [5.; 6.; 0.]];;
invm mat;;
multm mat (invm mat);;
invm [[1.; 9.]; [0.; 0.]];;
invm [[2.; 3.]];;
invm [[2.; 3.]; [4.]];;
invm (mkzerom 35 35);;
isunitm (invm (mkunitm 100));;

detm [[1.; 2.; 3.]; [0.; 1.; 4.]; [5.; 6.; 0.]];;
detm [[0.; 1.; 4.]; [1.; 2.; 3.]; [5.; 6.; 0.]];;
multm (invm [[0.; 1.; 4.]; [1.; 2.; 3.]; [5.; 6.; 0.]]) [[0.; 1.; 4.]; [1.; 2.; 3.]; [5.; 6.; 0.]];;
*)
