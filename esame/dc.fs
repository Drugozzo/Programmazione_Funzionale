type names = string;;
type chilDes = Childes of names * category;;


let rec number (cat,ls)=
	match (cat,ls) with
	|(_,[])->0
	|(cat,Childes(n,c)::xs)->
		//let (n,c)= x0
		if cat=c then 1+number (cat,xs)
		else number (cat,xs);;


let pay (name,ls)=
	let t=0
	let rec remove (name,ls)=
		match (name,ls) with
		|(_,[])->0.0
		|(name,Childes(n,c)::xs)->
			let t = remove(name,xs)
			if n=name && t<>0.0 then
				if c=Daycare then (float 225)+remove(name,xs)
				elif c=Nursery then (float 116)+remove(name,xs)
				else (float 110)+remove(name,xs)				
			elif n=name && t=0.0 then
				if c=Daycare then (float 225)/(float 2)+remove(name,xs)
				elif c=Nursery then (float 116)/(float 2)+remove(name,xs)
				else (float 110)/(float 2)+remove(name,xs)
			else 0.0+remove(name,xs)
	let tot=remove(name,ls)
	tot;;
	



let pay (name,ls)=
	let rec remove (name,ls)=
		match (name,ls) with
		|(_,[])->(0.0,[])
		|(name,Childes(n,c)::xs)->
			//let (n,c)=x0
			if n=name then
				if c=Daycare then (float 225,xs)
				elif c=Nursery then (float 116,xs)
				else (float 110,xs)
			else remove(name,xs)
	let (prIni,tail) = remove (name,ls)
	let rec payHalf (name,tail) =
		match (name,tail) with
		|(_,[])->0.0
		|(name,Childes(n,c)::xs)->
			if n=name then //no
				if c=Daycare then (float 225)/(float 2)+payHalf(name,xs)
				elif c=Nursery then (float 116)/(float 2)+payHalf(name,xs)
				else (float 110)/(float 2)+payHalf(name,xs)
			else payHalf(name,xs)
	let tot = payHalf(name,tail)+prIni
	tot;;

				


let lsC=[Childes("rossi",Daycare);Childes("rossi",Daycare);Childes("colombo",Daycare);Childes("masini",Daycare);Childes("rossi",Nursery);Childes("masini",Nursery);Childes("verdi",Nursery);Childes("bianchi",Nursery);Childes("colombo",Recreation);Childes("rossi",Recreation);Childes("gialli",Recreation)];;
