(*INTERPRETE IN OCAMEL GESUELE LUIGI MATRICOLA:562376*)
(*PARTE OBBLIGATORIA (CON SCOPING STATICO)*)
(*Sintassi astratta*)
(*Definisco cosa è un identificatore*)
type ide = string;;
(*Definisco i tipi di tutte le espressioni usabili nel mio linguaggio*)
type exp = 
EInt of int |
EBool of bool |
EString of string |
Den of ide |
Addizione of exp * exp |
Sottrazione of exp * exp |
Moltiplicazione of exp * exp |
Divisione of exp * exp |
Quadrato of exp |
Cubo of exp |
Esp of exp * exp |
Zero of exp |
Equivalente of exp * exp |
Maggiore of exp * exp |
Minore of exp * exp |
MaggioreUg of exp * exp |
MinoreUg of exp * exp |
Meno of exp |
Assoluto of exp |
Not of exp |
And of exp * exp |
Or of exp * exp|
Impl of exp * exp|
Xor of exp * exp |
Nand of exp * exp |
IfThenElse of exp * exp * exp |
Let of ide * exp * exp |
Fun of ide * exp |
Apply of exp * exp | 
Letrec of ide * exp * exp |
MyDict of dict | (*Definisco il tipo dizionario*)
Clear of exp | (*Definisco il tipo di Clear*)
Get of ide * exp |(*Definisco il tipo di Get*)
Remove of exp * ide | (*Definisco il tipo di Remove*)
ApplyOver of exp * exp |
SetElem of ide * exp * exp(*Definisco il tipo di ApplyOver*)
and dict = Empty | Item of (ide * exp) * dict;; (*Specifico il tipo dict (la grammatica)*)

(*CREAZIONE DELL'AMBIENTE POLIMORFO*)
type 't env = ide -> 't;;

(*DEFINIZIONE DEI TIPI ESPRIMIBILI NEL MIO LINGUAGGIO*)
type evT = Bool of bool | Int of int | Den of string | String of string | Unbound | Funval of efun | RecFunval of ide * efun |
DictVal of edict (*Tipo esprimibile per il mio dizionario *) 
and efun = ide * exp * evT env 
and edict = Vuoto | Elem of (ide * evT) * edict;; (*Grammatica del tipo esprimibile del dizionario*)

(*DEFINIZIONE DI AMBIENTE VUOTO*)
let emptyenv = fun x -> Unbound;; 
(*FUNZIONE CHE CREA UNA NUOVA ASSOCIAZIONE NELL'AMBIENTE*)
let applyenv (r : 't env) (i : ide) = r i;; 
(*FUNZIONE CHE ASSOCIA UN NUOVO VALORE A UN IDENTIFICATORE NELL'AMBIENTE SE ESISTE GIA', CHIAMA APPLYENV ALTRIMENTI (E QUINDI LO CREA)*)
let bind (r : 't env) (i : ide) (v : 't) = fun x -> if (x = i) then v else applyenv r x;;





(*RUN TIME SUPPORT*)
(*FUNZIONE CHE MI CONTROLLA I TIPI*)
let typecheck (s : string) (e : evT) : bool =
    match s with 
        "int" -> (match e with  
            Int(_) -> true |
            (_) -> false) |
        "string" -> (match e with
            String(_) -> true |
            (_) -> false) |
        "bool" -> (match e with
            Bool(_) -> true |
            (_) -> false) |
        (*"dizionario" -> (match e with
            DictVal(_) -> true |
            (_) -> false) |*)
        _ -> failwith("Errore, tipo non valido");;

(*FUNZIONI AUSILIARIE DELLE OPERAZIONI DI BASE*)
(*Addizione *)
let add x y = if (typecheck "int" x && typecheck "int" y) then
                    (match (x, y) with
                        (Int(a),Int(b)) -> Int(a+b) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Sottrazione *)
let sott x y = if (typecheck "int" x && typecheck "int" y) then
                    (match (x, y) with
                        (Int(a),Int(b)) -> Int(a-b) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Moltiplicazione *)
let molt x y = if (typecheck "int" x && typecheck "int" y) then
                    (match (x, y) with
                        (Int(a),Int(b)) -> Int(a*b) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Divisione *)
let div x y = if (typecheck "int" x && typecheck "int" y) then
                    (match (x, y) with
                        (Int(a),Int(b)) -> if (b != 0) then Int(a/b) else failwith("Errore divisione per zero") |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Quadrato *)
let quad x = if (typecheck "int" x) then
                    (match x with
                        Int(a) -> Int(a*a) |
                        (_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Cubo *)
let cub x = if (typecheck "int" x) then
                    (match x with
                        (Int(a)) -> Int(a*a*a) |
                        (_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Funzione ausiliaria per la funzione esponenziale (sotto) *)
let rec pow x y = match (x,y) with
                    (0,0) -> failwith("Errore matematico") |
                    (0,_) -> 0 |
                    (1,_) -> 1 |
                    (_,0) -> 1 |
                    (x,1) -> x |
                    (x,y) -> x * pow x (y-1);;
(*Esponenziale *)
let espon x y = if (typecheck "int" x && typecheck "int" y) then
                    (match (x, y) with
                        (Int(a),Int(b)) -> Int(pow a b) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Funzione che controlla se un valore è zero*)
let iszero x = if (typecheck "int" x) then
                    (match x with
                        (Int(a)) -> if x = Int(0) then Bool(true) else Bool(false) |
                        (_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Funzione che nega un intero *)
let meno x = if (typecheck "int" x) then
                    (match x with
                        (Int(a)) -> Int(-a) |
                        (_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Funzione che ritorna il valore assoluto di un intero*)
let val_abs x =  if (typecheck "int" x) then
                    (match x with
                        (Int(a)) -> if a < 0 then Int(-a) else Int(a) |
                        (_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Equivalenza *)
let equiv x y = if (typecheck "int" x && typecheck "int" y) then
                    (match (x, y) with
                        (Int(a),Int(b)) -> Bool(a = b) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Strettamente maggiore *)
let magg x y = if (typecheck "int" x && typecheck "int" y) then
                    (match (x, y) with
                        (Int(a),Int(b)) -> Bool(a > b) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Strettamente minore *)
let minn x y = if (typecheck "int" x && typecheck "int" y) then
                    (match (x, y) with
                        (Int(a),Int(b)) -> Bool(a < b)|
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Maggiore uguale *)
let maggu x y = if (typecheck "int" x && typecheck "int" y) then
                    (match (x, y) with
                        (Int(a),Int(b)) -> Bool(a >= b) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Minore uguale *)
let minu x y = if (typecheck "int" x && typecheck "int" y) then
                    (match (x, y) with
                        (Int(a),Int(b)) -> Bool(a <= b) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*And logico *)
let aand x y = if (typecheck "bool" x && typecheck "bool" y) then
                    (match (x, y) with
                        (Bool(a),Bool(b)) -> Bool(a && b) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Or logico *)
let oooh x y = if (typecheck "bool" x && typecheck "bool" y) then
                    (match (x, y) with
                        (Bool(a),Bool(b)) -> (Bool(a || b)) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Implicazione logica *)
let impl x y = if (typecheck "bool" x && typecheck "bool" y) then
                    (match (x, y) with
                        (Bool(a),Bool(b)) -> if (a && not(b)) then Bool(false) else Bool(true) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Negazione logica *)
let nooh x = if (typecheck "bool" x) then
                (match x with
                    Bool(a) -> Bool(not(a))
                | (_) -> failwith("Match sbagliato"))
             else failwith("Errore di tipo");;
(*NAND Logico *)
let nand x y = if (typecheck "bool" x && typecheck "bool" y) then
                    (match (x, y) with
                        (Bool(a),Bool(b)) -> Bool((not(a)&&not(b)) || (not(a)&&b) || (a&&not(b))) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Or esclusivo logico (XOR) *)
let escorr x y = if (typecheck "bool" x && typecheck "bool" y) then
                    (match (x, y) with
                        (Bool(a),Bool(b)) -> Bool(not((a&&b) || (not(a)&&not(b)))) |
                        (_,_) -> failwith("Match sbagliato"))
              else failwith("Errore di tipo");;
(*Alcuni metodi ausiliari per le estensioni del linguaggio didattico *)
(*Metodo che controlla se un id è già presente nel dizionario*)
let rec check (i:ide) (d:dict) : bool =
        (
            match d with
                Empty -> true |
                Item((id,_),dd) -> if id = i then false else check i dd 
        );;
(*Metodo che cerca un valore associato a un id nel dizionario e lo restituisce*)
let rec cerca (i:ide) (el:evT): evT =  
            (match el with
                  DictVal(Vuoto) -> Unbound |
                  DictVal(Elem((id,ell),dd)) -> if i = id then ell else cerca i (DictVal(dd))|
                _ -> failwith("Non e' un dizionario")
            );;
(*Metodo che rimuove un elemento corrispondente a un identificatore all'interno del dizionario e restituisce il dizionario modificato*)
let rec rimuovi (i:ide) (el:edict) : edict =
            (match el with
                Vuoto ->  Vuoto|
                (Elem((id,ex),dd)) -> 
                                if i=id then dd else Elem((id,ex),rimuovi i dd));;

(*Metodo che aggiorna un valore all'interno del dizionario se esiste già, altrimenti lo aggiunge, e restituisce il dizionario modificato*)
let rec setA (i:ide) (v:evT) (d1:edict) : edict =
        (match d1 with
            Vuoto -> Elem((i,v),d1) |(*Se arrivo qui significa che l'elemento non è già presente, per cui lo aggiungo in testa*)
            Elem((id,el),dd) -> if i = id then Elem((id,v),dd) else Elem((id,el),setA i v dd)   
        );;
(*CREAZIONE DELL' INTERPRETE*)
let rec eval (x:exp) (r: evT env) : evT = match x with
    EInt n -> Int n |
    EBool n -> Bool n |
    EString n -> String n |
    Den n -> applyenv r n |
    Addizione (n,m) -> add(eval n r)(eval m r) |
    Sottrazione (n,m) -> sott(eval n r)(eval m r)|
    Moltiplicazione (n,m) -> molt(eval n r)(eval m r)|
    Divisione (n,m) -> div(eval n r)(eval m r)|
    Maggiore (n,m) -> magg(eval n r)(eval m r)|
    Minore (n,m) -> minn(eval n r)(eval m r)|
    MinoreUg (n,m) -> minu(eval n r)(eval m r)|
    MaggioreUg (n,m) -> maggu(eval n r)(eval m r)|
    Equivalente (n,m) -> equiv(eval n r)(eval m r)|
    Cubo n -> cub (eval n r)|
    Quadrato n -> quad (eval n r)|
    Esp (n,m) -> espon(eval n r)(eval m r)|
    Zero n -> iszero (eval n r)|
    Meno n -> meno (eval n r)|
    Assoluto n -> val_abs (eval n r) |
    Not n -> nooh (eval n r)|
    And (n,m) -> aand(eval n r)(eval m r)|
    Or (n,m) -> oooh(eval n r)(eval m r)|
    Xor (n,m) -> escorr(eval n r)(eval m r)|
    Nand (n,m) -> nand(eval n r)(eval m r)|
    Impl (n,m) -> impl(eval n r)(eval m r)|
    IfThenElse (cond,thn,els) -> let c = eval cond r in
                                    if c=Bool(true) then eval thn r else eval els r |
        
    Let (ide,e1,e2) -> eval e2 (bind r ide (eval e1 r))|
    Fun (nomePar,corpoFun) -> Funval (nomePar,corpoFun,r)|
    Apply (ambFunDich,valParam) ->
			let chiusura = (eval ambFunDich r) in
				(match chiusura with(* EvItem((id,v),rest) ->let v1 = eval v r in eval fbody (bind ambDich id v1*)
				Funval (paramName,corpFun,ambDich) -> 
					eval corpFun (bind ambDich paramName (eval valParam r))
				|RecFunval(nomeFun, (paramName, corpoFun, ambDich)) -> 
					let actVal = (eval valParam r) in 
						let ambFinale = (bind ambDich nomeFun chiusura) in
							let ambAct = (bind ambFinale paramName actVal) in
								eval corpoFun ambAct
				| _ -> failwith("Definizione che non e' una funzione"))| 
    Letrec(nome, defFun, corpoFun) -> 
        (match defFun with
           	Fun(nomePar, corp) -> let r1 = (bind r nome (RecFunval(nome, (nomePar, corp, r)))) in
                  			                eval corpoFun r1
            | _ -> failwith("Definizione che non e' una funzione")) |
    (*Costrutti aggiunti al linguaggio didattico funzionale*)
    (*Valutazione di un dizionario*)
    MyDict(d) -> DictVal(evalDict d r) |
    (*Valutazione di un dizionario e svuotamento di esso*)
    Clear (d) -> (match (eval d r) with 
                    DictVal(a) -> DictVal(Vuoto)|
                    _ -> failwith("Non e' un dizionario")
    ) |
    (*Restituzione di un dato valore da un dizionario*)
    Get (i,d) -> cerca i (eval d r)|
    (*Rimozione di un dato valore da un dizionario*)
    Remove(d,i) -> (let v = eval d r in 
            match v with 
            DictVal(a) -> DictVal(rimuovi i a)|
            _ -> failwith("Non e' un dizionario"))|
    (*Applicazione di funzione su un dizionario*)
    ApplyOver(f,d) -> let chiusura = (eval f r) in 
                        (match (eval d r) with 
                            DictVal(a) -> DictVal(applyO chiusura a) |
                            _ -> failwith("Non e' un dizionario"))
    |
    (*Aggiornamento dizionario mediante aggiunta/modifica di un elemento*)
    SetElem(i,e,d) -> (let v1 = (eval d r) in
                            let v2 = eval e r in
                                match v1 with
                                DictVal(a) -> DictVal(setA i v2 a) |
                                _ -> failwith("Non e' un dizionario") )
(*Metodo che valuta il dizionario (dict) e restituisce un dizionario valutato (edict)*)
and evalDict (di:dict) (r:evT env) : edict = 
        (match di with
            Empty -> Vuoto |
            Item((id,ex),dd) -> if (check id dd) then Elem((id,eval ex r),evalDict dd r) 
                                else evalDict dd r (*Prendo in considerazione l'ipotesi di 'buttare via' la prima definizione*)
        )
(*Metodo che permette l'applicazione di una funzione su tutto un dizionario e restituisce il dizionario valutato aggiornato*)
and applyO (f:evT) (h:edict) : edict =
            match f,h with
            _,Vuoto -> Vuoto
            |(Funval(par,body,amb),Elem((id,v),rest)) -> 
                Elem((id,(eval body (bind amb par v))),(applyO f rest))
            |(RecFunval(funName,(par,body,amb)),Elem((id,v),rest)) -> 
                let newamb = bind amb funName f in
                    let finamb = bind newamb par v in
                        let res = eval body finamb in
                            Elem((id,res),(applyO f rest))
            | _ -> failwith("Non e' una funzione");;
    


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~TEST CASES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*Test dell'IF THEN ELSE (con addizione,sottrazione, maggioreuguale, quadrato, cubo, let funzionanti)*)
let espr1 = 
IfThenElse(
    MaggioreUg(
        EInt 5, EInt 6
    ),
    Let(
        "x",
        Addizione(
            EInt 0,
            EInt 3
        ),
        Cubo(
            Den "x"
        )
    ),
    Quadrato(
        EInt 5
    )
);;
eval espr1 emptyenv;;
let espr2 = 
IfThenElse(
    MaggioreUg(
        EInt 5, EInt 4
    ),
    Let(
        "x",
        Sottrazione(
            EInt 0,
            EInt 3
        ),
        Cubo(
            Den "x"
        )
    ),
    Quadrato(
        EInt 5
    )
);;
eval espr2 emptyenv;;
(* Test dei booleani (funzionanti)*)
let espr3 = 
IfThenElse(
    Impl(
        Xor(
            Nand(
                Not(
                    EBool true
                ),
                EBool false
            ),
            Or(
                And(
                    EBool false,
                    EBool false
                ),
                EBool true
            )
        ),
        EBool false
    ),
    EString "Corretto",
    EString "Errato"
);;
eval espr3 emptyenv;;

let espr4 = 
IfThenElse(
    Impl(
        Xor(
            Nand(
                Not(
                    EBool true
                ),
                EBool false
            ),
            Or(
                And(
                    EBool false,
                    EBool false
                ),
                EBool false
            )
        ),
        EBool false
    ),
    EString "Sbagliato",
    EString "Corretto"
);;
eval espr4 emptyenv;;

(*Test FUNZIONI (normali e ricorsive) funzionanti*)
(*Funzione che dato un parametro mi restituisce il suo doppio diviso per 4 (in intero)*)
let corpoFunzioneAnon =
Fun(
    "x",
    Divisione(
        Moltiplicazione(
            Den "x",
            EInt 2
        ),
        EInt 4
    )
);;
let chiamataFunzAnon =
Apply(
    corpoFunzioneAnon,
    EInt 10
);;
eval chiamataFunzAnon emptyenv;;
(*Test altre operazioni *)
let espr5 =
IfThenElse(
    Zero(
        EInt 0
        ),
    IfThenElse(
        MinoreUg(
            EInt 4,
            EInt 4
        ),
        IfThenElse(
            Minore(
                EInt 4,
                EInt 4
            ),
            Quadrato(
                EInt 4
            ),
            Esp(
                Meno(EInt 2),
                EInt 5
            )
        ),
        Meno(
            EInt 13
        )
    ),
    Let(
        "c",
        Cubo(
            Den "c"
        ),
        Den "c"
    )
);;
eval espr5 emptyenv;;
(*Funzione del fattoriale, dato un numero restituisce il suo fattoriale*)
let corpF =
Fun(
    "x",
    IfThenElse(
        Equivalente(
            Den "x",
            EInt 0
        ),
        EInt 1,
        Moltiplicazione(
            Den "x",
            Apply(
                Den "fact",
                Sottrazione(
                    Den "x",
                    EInt 1
                )
            )
        )
    )
);;
(*Applico il fattoriale di 5 *)
let fattoriale =
Letrec(
    "fact",
    corpF,
    Apply(
        Den "fact",
        EInt 5
    )
);;
eval fattoriale emptyenv;;
(*Fattoriale di un negativo (Errata)*)

let fattoriale_err =
Letrec(
    "fact",
    corpF,
    Apply(
        Den "fact",
        Meno(EInt 5)
    )
);;
eval fattoriale_err emptyenv;;
(*Miglioro la funzione fattoriale *)
let corpF =
Fun(
    "x",
    IfThenElse(
        Equivalente(
            Assoluto(
                Den "x"
            ),
            EInt 0
        ),
        EInt 1,
        Moltiplicazione(
            Assoluto(
                Den "x"
            ),
            Apply(
                Den "fact",
                Sottrazione(
                    Assoluto(
                        Den "x"
                    ),
                    EInt 1
                )
            )
        )
    )
    );;
let fattoriale_corr =
Letrec(
    "fact",
    corpF,
    Apply(
        Den "fact",
        Meno(EInt 5)
    )
);;
eval fattoriale_corr emptyenv;;
(*Prova di funzioni annidate e ritorno di funzione*)
(*let f x = 
    let val = x 
    in 
    let g y = y + val 
in g;; *)

let funzRetFunz = 
Let(
    "f",
    Fun(
        "x",
        Let(
            "val",
            Den "x",
            Let(
                "g",
                Fun(
                    "y",
                    Addizione(
                    Den "y",
                    Den "val"
                    )
                ),
                Den "g"
            )
        )
    ),
    Apply(
        Apply(
            Den "f",
        EInt 2
        ),
        EInt 2
    )
);;

eval funzRetFunz emptyenv;;
(* TEST SUI DIZIONARI *)
(*Creazione dizionario vuoto *)
let dict0 =
    MyDict(Empty)
;;
eval dict0 emptyenv;;
(*Creazione dizionario con valori *)
let dict1 =
    MyDict(
        Item(
            (
            "name",
            EString "Giovanni"
            ),
            Item(
            (
                "matricola",
                EInt 123456
            ),
            Empty
            )
            )
    );;
eval dict1 emptyenv;;

(*Accesso a un elemento del dizionario *)
let v1 = 
Get(
    "name",
    dict1
);;
eval v1 emptyenv;;

let v2 = 
Get(
    "matricola",
    dict1
);;
eval v2 emptyenv;;
(*Operazioni (dizionari sono immutabili)*)
(*Aggiunta elemento *)
let dict1 = 
SetElem(
    "eta",
    EInt 23,
    dict1
);;
eval dict1 emptyenv;;
(*Modifica elemento*)
let dict1 = 
SetElem(
    "eta",
    EInt 22,
    dict1
);;
eval dict1 emptyenv;;

(*Rimozione elemento*)
let dict4 = 
Remove(
    dict1,
    "name"
);;
eval dict4 emptyenv;;
(*Svuotamento dizionario*)
let dict3 = 
Clear(
    dict1
);;
eval dict3 emptyenv;;
(*Applicazione fun x -> x+1 su dizionario*)
eval (
    ApplyOver(
        Fun(
            "x",
            Addizione(
                Den "x",
                EInt 1
            )
        ),
        dict4
    ))
emptyenv;;
(*Test per controllare che i dizionari siano immutabili*)
(*Con il primo esempio dimostro che il tentativo di eliminare l'elemento "name" direttamente in d, in cui è definito il dizionario, fallisce*)
let immutabile = 
    Let(
        "d",
        MyDict(
            Item(
                (
                    "name",
                    EString "luigi"
                ),
                Empty
            )
        ),
        Let(
            "a",
            Remove(
                Den "d",
                "name"
            ),
            Den "d"
        )
    )
;;
eval immutabile emptyenv;; (*Risultato: dizionario immutato*)

(*In questo secondo esempio invece dimostro che la modifica di un dizionario può avvenire solamente mediante la copia modificata di esso in un altro Let*)
let immutabile2 = 
    Let(
        "d",
        MyDict(
            Item(
                (
                    "name",
                    EString "luigi"
                ),
                Empty
            )
        ),
        Let(
            "a",
            Remove(
                Den "d",
                "name"
            ),
            Den "a"
        )
    )
;;
eval immutabile2 emptyenv;; (*Risultato: dizionario vuoto*)

(*ALTRI TEST SUI DIZIONARI*)
(*Test creazione dizionario con 3 elementi di tipi diversi*)
    let dict1 = 
    MyDict(
        Item(
                (
                "Nome",
                EString "Luigi"
                ),
                Item(
                        (
                        "Eta",
                        EInt 20
                        ),
                    Item(
                            (
                            "Matricola",
                            EInt 562376
                            ),
                            Item(
                                (
                                "Iscritto",
                                EBool true
                                ),
                                Empty
                            )
                    )
                )
        )
    );;
    eval dict1 emptyenv;;
(*Test della get sull'elemento eta*)
let eta =
Get(
    "Eta",
    dict1
);;
eval eta emptyenv;;
(*Test modifica elemento esistente (eta)*)
let modif =
SetElem(
    "Eta",
    EInt 21,
    dict1
);;
eval modif emptyenv;;
(*Test della rimozione del nome*)
let newDi =
Remove(
    dict1,
    "Nome"
);;
eval newDi emptyenv;;
(*Test della rimozione dell' eta*)
let newDi =
Remove(
    dict1,
    "Eta"
);;
eval newDi emptyenv;;
(*Creazione dizionario con 3 elementi omogenei*)
    let dict2 =
    MyDict(
        Item(
            (
                ("Uno"),
                (EInt 1)
            ),
            Item(
                (
                    ("Due"),
                    (EInt 2)
                ),
                Item(
                    (
                        ("Tre"),
                        (EInt 3)
                    ),
                    Empty
                )
            )
        )
    );;
(*Creazione funzione da applicare al dizionario omogeneo (fun x -> x+1)*)
let funzione =
Let(
    "f",
    Fun(
        "x",
        Addizione(
            Den "x",
            EInt 1
        )
    ),
    Den "f"
);;
(*Applicazione funzione su tutto il dizionario(omogeneo)*)
let ap2 =
ApplyOver(
    funzione,
    dict2
);;
eval ap2 emptyenv;;
(*Applicazione funzione su tutto il dizionario (non omogeneo) con errore*)
let ap1 =
ApplyOver(
    funzione,
    dict1
);;
eval ap1 emptyenv;;
(*Test svuotamento dizionario dict1*)
let dict1 = (Clear(dict1));;
eval dict1 emptyenv;;
(*Test applicazione funzione su dizionario vuoto *)
let ap1 =
ApplyOver(
    funzione,
    MyDict(Empty)
);;
eval ap1 emptyenv;;

(*Test utilizzando il costrutto Let del mio interprete*)
let a =
eval (
Let(
    "dizionario",
    Clear(dict1)
    , Den "dizionario"
)
) emptyenv;;


(*Test applicazione funzione ricorsiva su tutto il dizionario*)
(*Per poter effettuare questo test bisogna prima valutare corpF della linea 489*)
let fattoriale =
Letrec(
    "fact",
    corpF,
    ApplyOver(
        Den "fact",
        dict2
    )
);;
eval fattoriale emptyenv;;

(*
SINTASSI CONCRETA MODIFICATA
/* creazione di un dizionario vuoto */
my_dict = {}
:- my_dict {}
/* creazione di un dizionario con valori */
my_dict = {'name': 'Giovanni', 'matricola': 123456}
:- my_dict = {'name': 'Giovanni', 'matricola': 123456}
/* accedere a un elemento di un dizionario */
my_dict.get('name')
:- 'Giovanni'
my_dict.get('matricola')
:- 123456
/* operazioni (dizionari sono immutable) */
/*Qui modifico per comodità la posizione in cui viene aggiunto l'elemento se non presente nel dizionario.
Se è già presente, aggiorno il suo valore con quello che viene impostato*/
my_dict1 = my_dict.set('età') = 22
-: my_dict1{'name': 'Giovanni', 'matricola': 123456, 'età' : 22}
my_dict2 = rm(my_dict1, 'name')
-: my_dict2{'matricola': 123456, 'età': 22}
my_dict3 = clear(my_dict2)
-: my_dict3{}
ApplyOver((fun x -> x+1), my_dict2)
:- {'matricola': 123457, 'età': 23}
*)