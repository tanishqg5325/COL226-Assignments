open List
open Vector_type

module Vector : Vector_type = struct
  type vector = float list
  
  let rec vdim v = match v with
                  [] -> 0
                | x::xs -> 1 + (vdim xs);;

  let rec mkzerov n = if (n = 0) then []
                    else 0.0::mkzerov (n-1);;

  let rec isvzerov v = match v with
                      [] -> true
                    | x::xs -> (x = 0.0) && (isvzerov xs);;

  let rec addv v1 v2 = match v1 with
                      [] -> []
                    | x::xs -> (x +. (hd v2))::addv xs (tl v2);;

  let rec scalermultv c v = match v with
                          [] -> []
                        | x::xs -> (x *. c)::scalermultv c xs;;

  let rec dotprodv v1 v2 = match v1 with
                          [] -> 0.0
                        | x::xs -> (x *. (hd v2)) +. dotprodv xs (tl v2);;

  (*let rec crossprodv v1 v2 =*)  
end
