# Interpreter_Ocaml
This is an interpreter in Ocaml of Operational Semantics. It implements lots of basic operations, plus Dictionaries such as the ones of Python. This interpreter is a project of the subject "Programmazione 2" of the University of Pisa.

# Sintassi
## Sintassi concreta
Come sintassi concreta ho utilizzato quella proposta nella consegna, con una piccola modifica sull’aggiunta di un
elemento. Ho infatti pensato di far aggiungere il nuovo elemento in fondo al dizionario invece che in cima, cosı̀ da
permettere, prima di aggiungere l’elemento immediatamente, di controllare se non ne esiste già uno con la stessa
chiave (questo per rispettare la filosofia dei dizionari di non avere chiavi duplicate). In caso essa sia già presente, viene
aggiornato il suo valore con quello nuovo (siccome i dizionari sono immutable per utilizzare un dizionario aggiornato
bisognerà crearne uno nuovo). In fondo al file ”SECONDO PROGETTINO.ml” ho riscritto, sotto forma di commento,
la sintassi concreta con le dovute modifiche.

## Sintassi astratta
Ho adattato la sintassi astratta alla sintassi concreta sopra descritta. Essa, oltre a tutte le operazioni di base con
interi, booleani e stringhe, implementa il dizionario come MyDict of dict, dove dict è un nuovo tipo dict = Empty |Item
of (ide * exp) * dict. Poi ho implementato i costrutti per operare sui dizionari Clear of exp, Get of ide * exp, Remove
of exp * ide, ApplyOver of exp * exp e SetElem of ide * exp * exp. Ho inoltre esteso anche i tipi esprimibili per poter
permettere la valutazione di un dizionario in questo modo: DictVal of edict dove edict è un nuovo tipo edict = Vuoto
|Elem of (ide * evT) * edict. Per facilitarne l’utilizzo in fase di test ho poi modificato la definizione di ambiente vuoto
nel seguente modo: let emptyenv = fun x ->Unbound.

# Type-checker
Ho utilizzato un type-checker dinamico sui valori primitivi Int, Bool e String, creando una funzione typecheck per
rendere più maneggevole il suo utilizzo a runtime. Essa prende in input una stringa (che sarà l’identificatore del tipo)
e un evT (expressible-value-type) e restituirà true se e solo se il valore esprimibile è di quel tipo (false altrimenti). Per
il type-checking sui dizionari, invece, ho utilizzato un controllo dei tipi esplicito ma sempre dinamico.

# Funzioni ausiliarie
Ho implementato un po’ di funzioni ausiliarie per effettuare diverse operazioni su interi, booleani e stringhe, utilizzando
opportunamente per ognuna di esse il metodo typecheck sopra descritto. Tra di esse ho inoltre implementato anche
una funzione check per controllare se in un dizionario un elemento dato è già presente, una funzione rimuovi per
rimuovere un elemento da un dizionario già valutato, una funzione setA per aggiungere/modificare un elemento al/del
dizionario già valutato.

# Interprete (eval)
Infine ho realizzato l’interprete OCaml seguendo le regole dello scoping statico e implementando tutte le operazioni
di base definite nella sintassi astratta. Prima di eseguire le varie operazioni del dizionario, le valuto e verifico che
effettivamente il tipo dell’espressione ad esse passato sia un DictVal, ovvero un valore evT per i dizionari, per poter
gestire gli errori di tipo (type-checking). Inoltre si può notare l’utilizzo di 2 ulteriori metodi ausiliari, evaldict per
convertire un dizionario non valutato dict in un dizionario valutato edict, e applyO per applicare una funzione su ogni
elemento del dizionario. In quest’ultima in particolare, prima verifico che il parametro f sia effettivamente una funzione
(FunVal in caso di funzione iterativa, RecFunVal in caso di funzione ricorsiva) e controllo che il dizionario sia pieno
(in quanto non avrebbe senso applicare una funzione su un dizionario vuoto, vedasi test-case numero 033). Anche qui
ho riscritto l’applicazione di funzione su ogni singolo elemento del dizionario seguendo sempre le regole dello scoping
statico. Nel caso in cui gli elementi del dizionario non siano omogenei oppure gli elementi di esso non siano conformi
con il tipo della funzione da applicare, verrà lanciata un’eccezione a runtime.

# Parte facoltativa: Dynamic scope
Ho aggiunto il costrutto Rt eval come richiedeva la parte facoltativa della consegna, che altro non doveva fare che
valutare una data espressione utilizzando le regole di scoping dinamico. Per fare ciò ho scritto un metodo ausiliario
eval dynamic, che è molto simile all’interprete classico eval, con la differenza che ogni qual volta che ha a che fare
con una funzione, la valuta nell’ambiente globale r e non in un ambiente ”ad hoc” come avviene invece con lo scoping
statico. Ho inoltre dovuto estendere il costrutto evT, aggiungendo FunvalD of efunD |RecFunvalD of ide * efunD con
efunD = ide * exp, per poter valutare le funzioni appunto nell’ambiente r. I test case relativi a questa parte sono in
fondo al file principale, appena sopra la sintassi concreta.

# Test cases
Ho scritto e opportunamente commentato diversi test per tutte le varie operazioni di base, e nella maggior parte di
essi ho utilizzato delle variabili di OCaml per memorizzare la semantica operazionale da valutare con l’interprete per
rendere più leggibile il codice dell’operazione di valutazione, che avrà quindi quasi sempre una forma simile a questa:
eval espr1 emptyenv. Per poter testare il funzionamento dell’interprete, sarà sufficiente dunque importare su OCaml
tutto il codice della sintassi astratta, delle funzioni ausiliarie e dell’interprete (quindi da riga 5 a riga 447). Dopodichè
bisognerà copiare e incollare tutto ciò che sta al di sotto della riga 451, effettuando cosı̀ i test sia sulle operazioni
di base dell’interprete, sia sulle operazioni con i dizionari, comprese quelle della consegna del testo. Si potrà poi
controllare la correttezza degli output visualizzandoli nel terminale. Per una più facile lettura degli esiti dei vari test
cases, consiglierei di controllarli copiandoli ed incollandoli uno ad uno, essendo tutti opportunamente commentati e
anche numerati (da 001 a 037). Se si vogliono testare solamente le operazioni con i dizionari e sulla parte facoltativa,
la procedura è la solita, ma a partire dalla linea 742.
