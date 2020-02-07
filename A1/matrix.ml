open List
open Matrix_type

module Matrix : Matrix_type = struct
  type matrix = float list list
  type vector = float list
  exception InvalidInput
  exception UnequalVectorSize
  exception UnequalMatrixShape
  exception IncompatibleMatrixShape
  exception SingularMatrix

  let isinvalidm m = match m with
      [] -> false
    | x::xs ->
        let rec isvalid m l = match m with
            [] -> true
          | x::xs -> if (length x) <> l then false
                      else isvalid xs l
        in not (isvalid xs (length x));;

  let rec vdim v = match v with
      [] -> 0
    | x::xs -> 1 + (vdim xs);;
  
  let mdim m = 
      if (isinvalidm m) then raise InvalidInput
      else (length m, length (hd m))

  let rec mkzerov n = 
      if n < 0 then raise InvalidInput
      else if(n = 0) then []
      else 0.::mkzerov (n - 1);;

  let rec iszerov v = match v with
      [] -> true
    | x::xs -> (x = 0.) && (iszerov xs);;

  let isEmpty v = match v with
      [] -> true
    | _ -> false;;

  let rec addv v1 v2 = match v1 with
      [] -> if (isEmpty v2) then []
            else raise UnequalVectorSize
    | x::xs -> if (isEmpty v2) then raise UnequalVectorSize
                else (x +. (hd v2))::addv xs (tl v2);;

  let rec scalarmultv c v = match v with
      [] -> []
    | x::xs -> (x *. c)::scalarmultv c xs;;

  let rec dotprodv v1 v2 = match v1 with
      [] -> if (isEmpty v2) then 0.
            else raise UnequalVectorSize
    | x::xs -> if (isEmpty v2) then raise UnequalVectorSize
                else (x *. (hd v2)) +. dotprodv xs (tl v2);;

  let rec crossprodv v1 v2 = 
      if (vdim v1) <> 3 || (vdim v2) <> 3 then raise InvalidInput
      else match v1 with 
          a1::a2::a3::x1 -> (match v2 with 
              b1::b2::b3::x2 -> [a2 *. b3 -. a3 *. b2; a3 *. b1 -. a1 *. b3; a1 *. b2 -. a2 *. b1]
            | _ -> [])
        | _ -> [];;


  let rec mkzerom m_ n_ = 
      if (m_ < 0 || n_ < 0) then raise InvalidInput
      else if m_ = 0 then []
      else (mkzerov n_)::mkzerom (m_ - 1) n_;;
  
  let rec iszerom m = 
      if (isinvalidm m) then raise InvalidInput
      else 
        let rec iszeromutil m = match m with
            [] -> true
          | x::xs -> (iszerov x) && (iszeromutil xs)
        in iszeromutil m;;
  
  let rec mkunitm m_ =
    if m_ < 0 then raise InvalidInput
    else if m_ = 0 then []
    else
      let rec prependzero mat = match mat with
          [] -> []
        | x::xs -> (0.::x)::prependzero xs
      in
      (1.::mkzerov (m_ - 1))::prependzero (mkunitm (m_ - 1));;

  let rec removeFirstColumn m = match m with
      [] -> []
    | x::xs -> (tl x)::removeFirstColumn xs;;
  
  let rec isunitm m = 
    if (isinvalidm m) then raise InvalidInput
    else if (length m) <> (length (hd m)) then false 
    else
      let rec isunitmutil m = match m with
          [] -> true
        | x::xs -> 
          if((hd x) <> 1.) then false
          else if(iszerov (tl x) = false) then false
          else
            let rec iszerovertical mat = match mat with
                [] -> true
              | x::xs -> ((hd x) = 0.) && (iszerovertical xs)
            in
            if (iszerovertical xs) = false then false
            else isunitmutil (removeFirstColumn xs)
      in isunitmutil m;;
  
  let rec addm m1 m2 =
    if (isinvalidm m1) || (isinvalidm m2) then raise InvalidInput
    else if (length m1) <> (length m2) then raise UnequalMatrixShape
    else if (length (hd m1)) <> (length (hd m2)) then raise UnequalMatrixShape
    else
      let rec addmutil m1 m2 = match m1 with
          [] -> []
        | x::xs -> (addv x (hd m2))::addmutil xs (tl m2)
      in addmutil m1 m2;;

  let rec scalarmultm c m = 
    if (isinvalidm m) then raise InvalidInput
    else 
      let rec scalarmultmutil c m = match m with
          [] -> []
        | x::xs -> (scalarmultv c x)::scalarmultmutil c xs
      in scalarmultmutil c m;;

  let rec getFirstColumn mat = match mat with
      [] -> []
    | x::xs -> (hd x)::getFirstColumn xs;;

  let rec multvm v mat = 
    if length (hd mat) = 0 then []
    else
      (dotprodv v (getFirstColumn mat))::multvm v (removeFirstColumn mat);;
  
  let rec multm m1 m2 = 
    if (isinvalidm m1) || (isinvalidm m2) then raise InvalidInput
    else if length (hd m1) <> length m2 then raise IncompatibleMatrixShape
    else
      let rec multmutil m1 m2 = match m1 with
          [] -> []
        | x::xs -> (multvm x m2)::multmutil xs m2
      in multmutil m1 m2;;
  
  let rec transm m = 
    if (isinvalidm m) then raise InvalidInput
    else
      let rec transmutil m = 
        if(length (hd m) = 0) then []
        else (getFirstColumn m)::transmutil (removeFirstColumn m)
      in transmutil m;;

  let rec getNonZeroRow mat = match mat with
      [] -> []
    | x::xs -> if (hd x <> 0.) then x else getNonZeroRow xs;;

  let rec list_swap l u v = match l with
      [] -> []
    | x::xs -> (if x = u then v
                else if x = v then u
                else x)::(list_swap xs u v);;

  let makeFirstRowNonZero mat = list_swap mat (hd mat) (getNonZeroRow mat);;
  
  let rec rowOperations mat v = match mat with
      [] -> []
    | x::xs -> 
        let m = -.(hd x) /. (hd v) in
        addv x (scalarmultv m v)::rowOperations xs v;;
  
  let rec detm m = 
    if (isinvalidm m) then raise InvalidInput
    else if (length m) <> (length (hd m)) then raise InvalidInput
    else 
      let rec detmutil m = match m with
          [] -> 1.
        | x::xs ->
            if(iszerov (getFirstColumn m) = true) then 0.
            else if (hd (hd m)) <> 0. then
              (hd x) *. detmutil (removeFirstColumn (rowOperations xs x))
            else match (makeFirstRowNonZero m) with
                [] -> 0.
              | x::xs -> -. (hd x) *. detmutil (removeFirstColumn (rowOperations xs x))
      in detmutil m;;

  let rec addColumn mat v = match mat with
      [] -> []
    | x::xs -> ((hd v)::x)::addColumn xs (tl v);;

  let rec createAugmented mat1 mat2 = 
    if (length (hd mat1) = 0) then mat2
    else addColumn (createAugmented (removeFirstColumn mat1) mat2) (getFirstColumn mat1);;
  
  let rec getMatrixBelow mat row = match mat with
      [] -> []
    | x::xs -> if (row = 0) then mat else getMatrixBelow xs (row-1);;
  
  let rec getMatrixAbove mat row = match mat with
      [] -> []
    | x::xs -> if (row = 0) then [] else x::getMatrixAbove xs (row-1);;

  let normalizeFirstRow mat = match mat with
      [] -> []
    | x::xs -> (scalarmultv (1. /. (hd x)) x)::xs;;
  
  let rec solveInv mat row = 
    if row = (length mat) then mat
    else
        let above = getMatrixAbove mat row in
        match normalizeFirstRow (makeFirstRowNonZero (getMatrixBelow mat row)) with
            [] -> mat
          | x::xs -> solveInv (removeFirstColumn ((rowOperations above x) @ (x::(rowOperations xs x)))) (row+1);;

  let rec invm m = 
    if ((detm m) = 0.) then raise SingularMatrix
    else solveInv (createAugmented m (mkunitm (length m))) 0;;

  let rec prependColumn mat v = match mat with
      [] -> []
    | x::xs -> ((hd v)::x)::prependColumn xs (tl v);;

  let rec removeIthColumn mat i = 
    if i = 0 then removeFirstColumn mat
    else prependColumn (removeIthColumn (removeFirstColumn mat) (i-1)) (getFirstColumn mat);;
  
  let rec crossprodvith m i = 
    if i = length (hd m) then []
    else if (i mod 2 = 0) then (detm (removeIthColumn m i))::crossprodvith m (i+1)
    else (-.(detm (removeIthColumn m i)))::crossprodvith m (i+1);;

  let rec crossprodvn m = 
    if (isinvalidm m) then raise InvalidInput
    else if length (hd m) <> (length m + 1) then raise InvalidInput
    else crossprodvith m 0;;

end
