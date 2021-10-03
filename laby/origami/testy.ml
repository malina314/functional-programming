open Origami;;

let pr = ((0., 0.), (1., 1.));;
let p1 = (2., 0.);;

assert (prostokat (0., 0.) (2., 2.) (1., 1.) = 1);;

let prost = prostokat (4., 2.) (6., 6.);;
let prost1 = zloz (6., 3.4) (4., 5.2) prost;;
let prost2 = zloz (6., 3.2) (4., 3.85) prost1;;

assert (prost2 (4., 3.85) = 2);;
assert (prost2 (5.88, 3.03) = 1);;
assert (prost2 (5.88, 3.05) = 2);;
assert (prost2 (5., 3.) = 3);;


let p1 = prostokat (0., 0.) (10., 10.);;
let l2 = [((8., 0.), (10., 2.));
        ((6., 0.), (10., 4.));
        ((4., 0.), (10., 6.));
        ((2., 0.), (10., 8.));
        ((0., 0.), (10., 10.));
        ((0., 2.), (8., 10.));
        ((0., 4.), (6., 10.));
        ((0., 6.), (4., 10.));
        ((0., 8.), (2., 10.))];;

let p3 = skladaj l2 p1;;

assert (p3 ((-4.), 6.) = 2);;
assert (p1 (0., 2.) = 1);;
assert (p1 (2., 0.) = 1);;