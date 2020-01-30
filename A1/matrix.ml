open List
open Matrix_type

module Matrix : Matrix_type = struct
  type matrix = (float list) list
  exception SingularMatrix;;

  let mdim mat = (length mat, length (hd mat))

  let rec mkzerov l = if(l = 0) then []
                      else 0.::mkzerov (l-1);;

  let rec iszerov l = match l with
                      [] -> true
                    | x::xs -> (x = 0.) && (iszerov xs);;

  let rec addv v1 v2 = match v1 with
                    [] -> []
                  | x::xs -> (x +. (hd v2))::addv xs (tl v2);;

  let rec scalermultv c v = match v with
                  [] -> []
                | x::xs -> (x *. c)::scalermultv c xs;;

  let rec dotprodv v1 v2 = match v1 with
                [] -> 0.0
              | x::xs -> (x *. (hd v2)) +. dotprodv xs (tl v2);;

  let rec mkzerom m n = 
      if(m = 0) then []
      else (mkzerov n)::(mkzerom (m-1) n);;
  
  let rec iszerom mat = match mat with
      [] -> true
    | x::xs -> (iszerov x) && (iszerom xs);;
  
  let rec mkunitm m =
    if(m = 0) then []
    else
      let rec prependzero mat = match mat with
          [] -> []
        | x::xs -> (0.::x)::prependzero xs
      in
      (1.0::mkzerov (m-1))::prependzero (mkunitm (m-1));;

  let rec removeFirstColumn mat = match mat with
      [] -> []
    | x::xs -> (tl x)::removeFirstColumn xs;;
  
  let rec isunitm mat = match mat with
      [] -> true
    | x::xs -> 
              if((hd x) <> 1.) then false
              else if(iszerov (tl x) = false) then false
              else
                let rec iszerovertical mat = match mat with
                    [] -> true
                  | x::xs -> ((hd x) = 0.) && (iszerovertical xs)
                in
                if((iszerovertical xs) = false) then false
              else isunitm (removeFirstColumn xs);;
  
  let rec addm mat1 mat2 = match mat1 with
      [] -> []
    | x::xs -> (addv x (hd mat2))::addm xs (tl mat2);;

  let rec scalermultm c mat = match mat with
      [] -> []
    | x::xs -> (scalermultv c x)::scalermultm c xs;;

  let rec getFirstColumn mat = match mat with
      [] -> []
    | x::xs -> (hd x)::getFirstColumn xs;;

  let rec multvm v mat = 
    if(snd (mdim mat) = 0) then []
    else
      (dotprodv v (getFirstColumn mat))::multvm v (removeFirstColumn mat);;
  
  let rec multm mat1 mat2 = match mat1 with
      [] -> []
    | x::xs -> (multvm x mat2)::multm xs mat2;;
  
  let rec transm mat = 
    if(snd (mdim mat) = 0) then []
    else
      (getFirstColumn mat)::transm (removeFirstColumn mat);;

  let rec getNonZeroRow mat = match mat with
      [] -> []
    | x::xs -> if (hd x <> 0.) then x else getNonZeroRow xs;;

  let rec list_swap l u v = match l with
      [] -> []
    | x::xs -> (if x = u then v
                else if x = v then u
                else x)::(list_swap xs u v);;

  let makeFirstRowNonZero mat = 
    list_swap mat (hd mat) (getNonZeroRow mat);;
  
  let rec rowOperations mat v = match mat with
      [] -> []
    | x::xs -> 
        let m = -.(hd x) /. (hd v) in
        addv x (scalermultv m v)::rowOperations xs v;;
  
  let rec detm mat = match mat with
      [] -> 1.
    | x::xs ->
      if(iszerov (getFirstColumn mat) = true) then 0.
      else if (hd (hd mat)) <> 0. then
        (hd x) *. detm (removeFirstColumn (rowOperations xs x))
      else match (makeFirstRowNonZero mat) with
        x::xs -> -. (hd x) *. detm (removeFirstColumn (rowOperations xs x))

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
    | x::xs -> (scalermultv (1. /. (hd x)) x)::xs;;
  
  let rec solveInv mat row = 
    if (row = (length mat)) then mat
    else
        let above = getMatrixAbove mat row in
        match normalizeFirstRow (makeFirstRowNonZero (getMatrixBelow mat row)) with
          x::xs -> solveInv (removeFirstColumn (append (rowOperations above x) (x::(rowOperations xs x)))) (row+1);;

  let rec invm mat = 
    if ((detm mat) = 0.) then raise SingularMatrix
    else solveInv (createAugmented mat (mkunitm (length mat))) 0;;

end
