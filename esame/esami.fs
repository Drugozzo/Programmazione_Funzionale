

type valV= 
	|V of string * int;;

type valG= 
	|G of string * string;;



let valuta voto =
	match voto with
	|V(n,v) -> 
		if v<18 then G(n,"insufficiente")
		elif v>=18 && v<=22 then G(n,"sufficiente")
		elif v>=23 && v<=27 then G(n,"buono")
		else G(n,"ottimo");;


let rec valutaList ls=
	match ls with
	|[]->[]
	|x0::xs->valuta x0 :: valutaList xs;;


let rec creaValList (strNomi,strVoti)=
	match (strNomi,strVoti) with
	|(_,[]) | ([],_)->[]
	|(n0::ns,v0::vs)->V(n0,v0)::creaValList(ns,vs);;


let media ls=
	let rec sommaAndConta vLists=
		match vLists with
		|[]->(0,0)
		|[x]->
			let (n,v)= x
			(v,1)
		|x0::xs->
			let (n,voti)=x0
			let (vs,count)=sommaAndConta xs
			(voti+vs,count+1)
	let (s,c)= sommaAndConta ls
	(float s/float c);;


let rec separa ls=
	match ls with
	|[]->([],[])
	|V(n,v)::xs->
		let (bocciati,promossi)=separa xs
		if v<18 then (V(n,v)::bocciati,promossi)
		else (bocciati,V(n,v)::promossi);;
