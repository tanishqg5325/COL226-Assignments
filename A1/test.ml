#use "matrix.ml";;
open Matrix;;

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

isunitm [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 1.; 1.]];;
addm (mkunitm 3) (mkunitm 3);;
scalarmultm 5. (mkunitm 4);;
multm [[1.; 2.; 3.]; [4.; 5.; 6.;]; [7.; 8.; 9.;]] [[1.; 2.; 3.]; [4.; 5.; 6.;]; [7.; 8.; 9.;]];;
transm [[1.; 2.; 3.]; [4.; 5.; 6.]];;
let mat = [[1.; 2.; 3.]; [0.; 1.; 4.]; [5.; 6.; 0.]];;
invm mat;;
multm mat (invm mat);;
multm (mkunitm 2) (mkunitm 3);;
