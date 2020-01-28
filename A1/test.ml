#use "matrix.ml";;
open Matrix;;

iszerom [[0.; 0.; 0.]; [0.; 1.; 0.]];;
mkzerom 3 4;;
mkunitm 2;;
isunitm [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 1.; 1.]];;
addm (mkunitm 3) (mkzerom 3 3);;
scalermultm 5. (mkunitm 4);;
multm [[1.; 2.; 3.]; [4.; 5.; 6.;]; [7.; 8.; 9.;]] [[1.; 2.; 3.]; [4.; 5.; 6.;]; [7.; 8.; 9.;]];;
transm [[1.; 2.; 3.]; [4.; 5.; 6.]];;
makeFirstRowNonZero [[0.; 1.; 3.]; [0.; 1.; 3.]; [7.; 1.; 5.]];;
