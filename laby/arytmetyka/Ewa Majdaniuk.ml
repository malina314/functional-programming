(* autor: Ewa Majdaniuk *)
(* reviewer: Mateusz Malinowski *)

open List;;

type wartosc = (float*float)*(float*float) 

	let clas x = classify_float x
	(* funkcje pomocnicze *)
	let print a = (* funkcja do wypisywania (usunąć przed wysłaniem) *)
		match a with
			((a,b),(c,d)) -> print_float a;
                             print_string " ";
                             print_float b;
                             print_string " ";
                             print_float c;
                             print_string " ";
                             print_float d;
                             print_string "\n";;

	let scal x y = (*funkcja scalająca lub nie dwa przedziały*)
        print (x, y);
		match (x,y) with 
			((ax,bx),(ay,by)) ->
				if (bx < ay) then (x,y)
				else if (by < ax) then (y,x)
				else  ((min ax ay,max bx by),(nan,nan));;
				
let aabs x = if x >= 0. then x else -.x;;


(* funkcje właściwe *)		
let wartosc_dokladnosc x p =
	((x -. aabs(0.01*.p*.x) , x +. aabs(0.01*.p*.x)) , (nan,nan))

let wartosc_od_do x y = 
	((x,y),(nan, nan))

let wartosc_dokladna x = 
	((x,x),(nan, nan))

let min_wartosc war =
	match  war with
		((a,b),(c,d))-> a

let max_wartosc war = 
	match war with 
		((a,b),(c,d)) -> 
			if (clas c = FP_nan) then b
			else d
	
let in_wartosc war x =
	match war with
		((a,b),(c,d)) -> 
			if (a = neg_infinity && b = infinity) then true
			else if (a = neg_infinity && d = infinity && (x<=b || x>=c)) then true
			else if (a = neg_infinity && d = infinity && x > b && x < c) then false
			else if (x >= min_wartosc war && x <= max_wartosc war)then true (* przedział (a,b) *)
			else false

let sr_wartosc war = 
	match war with
		((a,b),(c,d))-> 
			if (a = neg_infinity && (b = infinity || d = infinity)) then nan
			else if (d = infinity || b = infinity) then infinity
			else if (a = neg_infinity) then neg_infinity
			else (a +. b)/. 2.
					
let plus x y = 
    print x; print y;
	match (x,y) with
		((ax,bx),(cx,dx)),((ay,by),(cy,dy)) ->
		if (clas ax = FP_nan || clas ay = FP_nan)  
			then ((nan,nan),(nan,nan))
		else if (min_wartosc x = neg_infinity && max_wartosc y = infinity)   
			then ((neg_infinity, infinity),(nan,nan))
		else if (clas cx = FP_nan && clas cy = FP_nan) (* przedział (a,b) *)
			then (((ax +. ay),(bx +. by)),(nan,nan))
		else if (clas cx = FP_nan) 
			then scal (neg_infinity,by+.bx) (cy+.ax,infinity)
		else if (clas cy = FP_nan) 
			then scal (neg_infinity,by+.bx) (cx+.ay,infinity)
		else ((nan,nan),(nan,nan))
		
		
let minus x y = 
	match y with 
		((a,b),(c,d)) ->
			if(clas c = FP_nan) then plus x ((-.b,-.a),(c,d))
			else plus x ((-.d,-.c),(-.b,-.a)) (* TU BYL BUG *)(* TU BYL BUG *)(* TU BYL BUG *)(* TU BYL BUG *)(* TU BYL BUG *)(* TU BYL BUG *)(* TU BYL BUG *)(* TU BYL BUG *)(* TU BYL BUG *)(* TU BYL BUG *)
	
	let pomodwroc (a,b) = (*funkcja pomocnicza do odwracania przedziału (a,b) *)
			if (a = 0.) then (1./.b , infinity)  
			else if (b = 0.) then (neg_infinity , 1./.a)
			else (1./.b , 1./.a)
			
	let odwroc war = (*funkcja odwracająca przedział / dzieląca 1/war *)
		match war with
		((a,b),(c,d)) ->
			if(a = 0. && b = 0.) then ((nan,nan),(nan,nan))  (* 1/(0,0) *)
			else if (a = neg_infinity && b = infinity) then ((neg_infinity,infinity),(nan,nan))
			else if (clas a = FP_nan) then ((nan,nan),(nan,nan))
			else if (clas c = FP_nan) then (* przedział (a,b) *)
				if(a < 0. && b > 0.) then ((neg_infinity,1./.a),(1./.b,infinity))
				else (pomodwroc (a,b) , (nan,nan))
			else if (b <= 0. && c >= 0.) then scal (pomodwroc (a,b)) (pomodwroc (c,d))
			else ((neg_infinity,1./.c),(1./.b,infinity)) (* obie wartości >0 lub <0 oraz b<c więc  1/b > 1/c *)

	let rec pomrazy x y = (* funkcja pomocnicza do mnożenia- mnoży dwa przedzialy (ax,bx)*(ay,by)->(a,b) *)
		match (x,y) with 
			((ax,bx),(ay,by)) ->
					let pom1 = if(ax=0.||ay=0.) then 0. else ax *. ay in
					let pom2 = if(ax=0.||by=0.) then 0. else ax *. by in
					let pom3 = if(bx=0.||ay=0.) then 0. else bx *. ay in
					let pom4 = if(bx=0.||by=0.) then 0. else bx *. by in
					let minim = min (min pom1 pom2 ) (min pom3 pom4 )in
					let maxim = max (max pom1 pom2 ) (max pom3 pom4) in
				(minim, maxim)
		
let rec razy x y =
		match (x,y) with 
		(((ax,bx),(cx,dx)),((ay,by),(cy,dy))) ->
			if (clas ax = FP_nan || clas ay = FP_nan) 
				then ((nan,nan),(nan,nan)  )
			else if ((ax = 0. && bx = 0.) || (ay = 0. && by = 0.))
				then ((0.,0.),(nan,nan))
			else if (clas cx = FP_nan && clas cy = FP_nan) (* przedział (a,b) *)
				then ((pomrazy (ax,bx) (ay,by)),(nan,nan))
			else if (clas cx <> FP_nan && clas cy <> FP_nan) 
				then if (bx > 0. || by > 0. || cx < 0. || cy < 0. ) (* czy któryś przedział zawiera (r<0,inf) lub (-inf,r>0) *)
					then ((neg_infinity,infinity),(nan,nan))
					else odwroc (razy (odwroc x) (odwroc y)) (* bo któryś z tych przedziałów po odwróceniu będzie miał postać (a,b) *)
			else if (clas cy = FP_nan) 
				then scal (pomrazy (ax,bx) (ay,by) ) (pomrazy (cx,dx) (ay,by) )
			else if (clas cx = FP_nan) 
				then scal (pomrazy (ay,by) (ax,bx) ) (pomrazy (cy,dy) (ax,bx) )
			else ((nan,nan),(nan,nan))
		
let podzielic x y = razy x (odwroc y)
;;
