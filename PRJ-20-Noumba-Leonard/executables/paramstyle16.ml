
(* C:\Users\user\Desktop\Projectdocs\prog\paramstyle12.ml *)

(**Assuming the factors that effect parameter passing include entity passed, context, evaluation and typing *)

(**List of integers for holding various instances for factors *)
type factors_instance_list = int list 

(*the variouse types of entities *)
type entity_type =   
Entity_type of factors_instance_list   (*list of integers holds the various type of entity for a passing style and each integer corresponds to a specific type of entity*)   

 (*the various types of context*)
type context_type = 
Context_type of factors_instance_list         

 (*the various evaluation strategies*)
type evaluation_strat =   
Evaluation_strat of factors_instance_list                 

 (*the various types*)
type typing =              
Correct_type of factors_instance_list        
 
(* various passing style names*)
type passing_style =            
Passing_style of string 
 

(** Record type to hold known factors that effect the various passing styles
@param entity, context, evaluation, typing: various factors that effect parameter passing*)

type factors = {entity : entity_type; context : context_type; evaluation : evaluation_strat; typing : typing} 

(** Record type to hold a passing style, consist of passing style name and factors
@param name: style name
@param factor: style factors*)

type style_factors = {name : passing_style; factor : factors}     
 

(**Record to hold new instances for factors. Each consist of a name and meaning (value) *)

type new_factor_instance1 = {fact_name:string ; num:int ; new_entity_instance_list:(string*int) list}	(** for entity passed *)
type new_factor_instance2 = {fact_name:string ; num:int ; new_context_instance_list:(string*int) list} (** for context *)
type new_factor_instance3 = {fact_name:string ; num:int ; new_eval_instance_list:(string*int) list} (** for evaluation *)
type new_factor_instance4 = {fact_name:string ; num:int ; new_typ_instance_list:(string*int) list} (** for typing *)

 (**Record type to hold instances of parameter passing style factors*)

type time = {ent: factors_instance_list ; context: factors_instance_list; eval: factors_instance_list; 
typ: factors_instance_list; new_entity_instance_list:(string*int) list; new_context_instance_list:(string*int) list;
 new_eval_instance_list:(string*int) list; new_typ_instance_list:(string*int) list }   

(**Record to hold results of styles, interpretation(interp), record and effect out of some methods (Insert_new_passing_style) *)
 type interpret_rec1 =
	{styles : style_factors list ; interp : (passing_style * string) list; 
 	record:time; effect : (passing_style * string) list }

(**Default instances(value) for parameter passing style factors
@value [5] : default instance for factors not of interest to the users
*)
let record = {ent = [5] ; context = [5]; eval = [5]; typ = [5]; new_entity_instance_list = [("new",12)]; 
new_context_instance_list = [("new",12)]; new_eval_instance_list = [("new",12)]; 
new_typ_instance_list = [("new",12)] } 

(**List of  Known(hard-coded) passing styles   *)

let user_styles = [{name = Passing_style "PBV";				
  factor =
   {entity = Entity_type [1]; context = Context_type [1]; evaluation = Evaluation_strat [1];
    typing = Correct_type [1]}}; 
	{name = Passing_style "PBR";
  factor =
   {entity = Entity_type [1]; context = Context_type [2]; evaluation = Evaluation_strat [3];
    typing = Correct_type [3]}}; 
	{name = Passing_style "PBN";
  factor =
   {entity = Entity_type [2]; context = Context_type [2]; evaluation = Evaluation_strat [2];
    typing = Correct_type [2]}}; 
	{name = Passing_style "PBCR";
  factor =
   {entity = Entity_type [3]; context = Context_type [3]; evaluation = Evaluation_strat [3];
		typing = Correct_type [3]}}; 
	{name = Passing_style "PBNE";
  factor =
   {entity = Entity_type [1]; context = Context_type [2]; evaluation = Evaluation_strat [3];
		typing = Correct_type [4]}} ] 

(**Association list to hold interpretations for the various passing styles *)

let interpretation_list = [
	(Passing_style "PBV", "Evaluation from left to right");
  (Passing_style "PBN", "No reduction inside abstraction");
	(Passing_style "PBNE", "Arguments first evaluation used in all occurences")
]

(**Association list to hold effects for the various passing styles *)

let effect_list = [
	(Passing_style "PBV", "No mutation of variables");
	(Passing_style "PBN", "No mutation of variables");								
	(Passing_style "PBR", "Variables mutated after computation")
]

(**Method to set the name of a passing style to the type passing style *)
let setName name =              
let n = Passing_style name in 
match n with 
Passing_style name -> Passing_style name

(*Method to get the passing style name
@param name : name of passing style with defined constructor
**)
let getName name = 			
 match name with 
 Passing_style a -> a 

 (**Method used to output record of factors*)
let f x = match x with 			
x -> x 

(**
returns a specific style from the list of passing styles
@param user_style: list from which we want to return the passing style
@param y; passing style to be returned
*)
let rec return_inserted_style user_styles y =    
			 match user_styles with 
			 [] -> y
			 |h :: t ->if h = y then h else return_inserted_style t y 

(**
Inserts a new passing style in the initial list of passing style(known passing styles)
@param user_styles: the list to add new passing style
@param y : passing style to be added to list
*)
let rec insert_new_style user_styles y =   
			 match user_styles with 
			 [] -> let () = print_string " successfully added.\n\n" in  [y]
			 |h :: t ->if h = y then let () = print_string " already exists.\n " 
			 in  user_styles else h :: insert_new_style t y

(** Method that inserts intances into factors list of instances
@param  instance_list: instance list as defined by the type factors_instance_list above
@param  y : specific instance of a factor to be added in instance list from user for that factor
*)		
let rec insert_instance instance_list y = 
 match instance_list with 
 [] -> [y]
 |h :: t -> if h = y then instance_list else h :: insert_instance t y 

(**Method used in the "delete_passing_style" method to remove a passing style from the list of passing styles
 @param styles: list to remove passing style from
 @param y : passing style to be removed
*)	
let rec remove_passing_style styles y =
 match styles with 
 | [] -> let () = print_string "\nNo such passing style in the structure. \n\n" in []
 | h :: t -> if h = y 
						 then 
						 let () = print_string " deleted successfully." in t 
			 else h :: remove_passing_style t y  

(** Method used to validate input for integers *)
let validate_input () = let () = print_string " " in 
let rec check_int () =  
try read_int() with | _ -> let () = print_string "INVALID input enter number: " in   check_int () 
in check_int ()


(**Method(sanitize) that checks if passing style is in list, adds it if not and returns the list
@param	user_styles, name: list of known passing styles and name of passing styles recpectively
@param entity_type, context_type, evaluation_strat, typing: factors that effect parameter passing
@param x: record that holds passing style factors with their values
@param y: passing style constructed
*)
let sanitize entity_type context_type evaluation_strat typing user_styles name=     
  match (entity_type, context_type, evaluation_strat, typing) with      
  (Entity_type [1],Context_type [1],Evaluation_strat [1],Correct_type [1]) -> user_styles
  |(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [3]) -> user_styles																										
  |(Entity_type [2],Context_type [2],Evaluation_strat [2],Correct_type [2]) -> user_styles 		 
  |(Entity_type [3],Context_type [3],Evaluation_strat [3],Correct_type [3]) -> user_styles
	|(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [4]) -> user_styles																				
  |(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> let new_style = 
								let  x = {entity = entity_type; context = context_type;
								          evaluation = evaluation_strat; typing = typing}
								in 								
								let y = {name = (setName name); factor = x }
								in
								let () = print_string "\npassing style " in let () = print_string "''" in 
								let () = print_string name in let () = print_string "''"
								in																																					
							  insert_new_style user_styles y 	
							  in new_style 

(**Method that Gets the name of a passing style or takes name from user if user defined style*)
let get_name entity_type context_type evaluation_strat typing  =   
  match (entity_type, context_type, evaluation_strat, typing) with
  (Entity_type [1],Context_type [1],Evaluation_strat [1],Correct_type [1]) -> "PBV"
  |(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [3]) -> "PBR"																										
  |(Entity_type [2],Context_type [2],Evaluation_strat [2],Correct_type [2]) -> "PBN" 		 
  |(Entity_type [3],Context_type [3],Evaluation_strat [3],Correct_type [3]) -> "PBCR"
  |(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [4]) -> "PBNE"																				
	|(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> let () = print_string "-->Enter style name: " 
																																								in read_line()

(**Method that returns a specific passing style from list of styles*)
let paramstyle entity_type context_type evaluation_strat typing user_styles name= 
 match (entity_type, context_type, evaluation_strat, typing) with     
  (Entity_type [1],Context_type [1],Evaluation_strat [1],Correct_type [1]) -> let rec get_style_list =  
					let name = name 																				
					in					
					let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
					in 
					let y = {name = (setName name); factor = x }
					in																				
					return_inserted_style user_styles y 
				in get_style_list
  |(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [3]) -> let rec get_style_list  =  
				let name = name
				in	 																	
				let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
				in 
				let y = {name = (setName name); factor = x }
				in
				return_inserted_style user_styles y 
			in get_style_list 																												
  |(Entity_type [2],Context_type [2],Evaluation_strat [2],Correct_type [2]) -> let rec get_style_list  =  
				let name = name
				in
				let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
				in 
				let y = {name = (setName name); factor = x }
				in 				
				return_inserted_style user_styles y
			 in get_style_list 		 
  |(Entity_type [3],Context_type [3],Evaluation_strat [3],Correct_type [3]) -> let rec get_style_list  =  
				let name = name
				in																			
				let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
				in 
				let y = {name = (setName name); factor = x }
				in
				return_inserted_style user_styles y
			in get_style_list 
|(Entity_type [1],Context_type [2],Evaluation_strat [3],Correct_type [4]) -> let rec get_style_list  =  
																		
				let name = name
				in																			
				let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
				in 
				let y = {name = (setName name); factor = x }
				in
				return_inserted_style user_styles y
			in get_style_list 																					
  |(Entity_type _,Context_type _,Evaluation_strat _,Correct_type _) -> let new_style = 
						let  x = {entity = entity_type; context = context_type; evaluation = evaluation_strat; typing = typing}
						in 
						let y = {name = (setName name); factor = x } 
						in																																																								
					return_inserted_style user_styles y 	
					in new_style  


(* method that display the various factors available for parameter passing*)

let help = fun () -> print_string "\nAvailable factors for passing styles: ( ENT, CON, EVA, TYP )\n"

(**Method that display the main menu of the system *)
let display_menu = 
	fun() -> print_endline "\n
	PASSING STYLE ACTION MENU 
	1. Add new passing style
	2. Delete passing style
	3. See all available passing style
	4. Select passing style from structure
	5. Help
	6. Exit system
	 "


(**Method that display the help menu item*)
let user_facing_information styles interpretation_list record effect= 
	let () =  print_string "This table represents information the user faces concerning factors of parameter passing styles
Factors *Meaning**Instances

Entity     0      New
           1      Val
           2      Ref
           3      Comp
           4      Env
           5      Ctn
           6      Den
  
Context    0      New
           1      Cln
           2      Cld
           3      Cnd
           4      Oth

Eval       0      New
           1      Str                                                                                             
           2      Laz
           3      Non
           4      Man

Typing     0      New 
           1      Yes
           2      No
           3      Nan
" 
in let styles = styles
in let interp = interpretation_list 
in 
{styles = styles; interp = interp; record=record ; effect= effect}


(* 
	method that Removes the head from a list
	@param l: the list from which to remove head
*)
let remove_head l = 
	if List.length l > 1 then
		match l with
			[] -> []
			|h ::t -> t
	else	
		l

(**Method that removes default value([5]) from instance list for the various factors 
if factor is of interest to the user.*)
let preserve_default = fun record -> 
	{
		ent = remove_head record.ent; 
		eval = remove_head record.eval; 
		context = remove_head record.context;  
		typ = remove_head record.typ;
		new_entity_instance_list = record.new_entity_instance_list;
		new_context_instance_list = record.new_context_instance_list;
    new_eval_instance_list = record.new_eval_instance_list;
    new_typ_instance_list = record.new_typ_instance_list 
	} 


(**Method that get the name of a factor and gives it meaning in integer form since the system works but with numbers
@param fac_name : name of the factor (ent for entity passed)*)
let factor_name fac_name =
	match String.lowercase_ascii fac_name with 
	"ent" -> 1
	|"con" -> 2
	|"eva" -> 3
	|"typ" -> 4
	|_->100

(**Method to insert new instance for a factors in the instance list for that factor *)
let rec insert_new_instance new_instance new_instance_list = 
	match new_instance_list with
	[] -> let () = print_string "\nNew instance successfully added.\n" in  new_instance :: []
	|(a,b) :: t -> if (a,b) = new_instance then 
																	let () = print_string "\nalready exist." 
																	in new_instance_list else (a,b) :: insert_new_instance new_instance t

(**Method to get the name and value for the various instance (including new instance) for the factor "entity passed"
@param val_name : instance of the factor
@param new_entity_instance_list: list that holds new instance for the factor "entity passed" *)
let ent_name val_name new_entity_instance_list=
	match String.lowercase_ascii val_name with
		"val" -> {fact_name = val_name ; num=1 ; new_entity_instance_list=new_entity_instance_list}
    |"ref" -> {fact_name = val_name ; num=2 ; new_entity_instance_list=new_entity_instance_list}
    |"comp" ->{fact_name = val_name ; num=3 ; new_entity_instance_list=new_entity_instance_list}
    |"env" -> {fact_name = val_name ; num=4 ; new_entity_instance_list=new_entity_instance_list}
    |"ctn" -> {fact_name = val_name ; num=5 ; new_entity_instance_list=new_entity_instance_list}
		|"den" -> {fact_name = val_name;num=6;new_entity_instance_list=new_entity_instance_list}
		|_-> let ret new_entity_instance_list= let () = print_string "-->Give name for factor: " in 
		              let newfac_name = read_line() in 
									 let () = print_string "-->Give value for factor: "
														in let num = validate_input()
														in let new_instance = (newfac_name,num) 
														in let new_entity_instance_list = insert_new_instance new_instance new_entity_instance_list 
														in {fact_name = newfac_name;num=num; new_entity_instance_list=new_entity_instance_list}
														in ret new_entity_instance_list

(**Method to get the full name of the instance provided by user for the factor "entity" 
@param name: short form name for the instance of the factor "entity passed"*)
let ful_name name = match name with 
"val"-> "value"
|"ref"-> "reference"
|"comp"->"computation"
|"env"-> "environment"
|"ctn"-> "continuation"
|"den"-> "denotation"
|_ -> name

(**Method to get the full name of the instance provided by user for the factor "entity passed" *)
let ful_name_context name = match name with 
"cln" -> "calling"
|"cld" -> "called"
|"cnd" -> "callboth"
|"oth" ->"other"
|_-> name

let ful_name_eval name = match name with 
"str" -> "strict"
|"laz" -> "lazy"
|"non" -> "non"
|"man" -> "manual"
|_-> name

let ful_name_typ name = match name with 
"yes" -> "yes"
|"no" -> "no"
|"non" -> "non"
|_-> name

(**Method to show the full name of a passing style
@param name : abbreviated name *)
let ful_name_style name = match String.uppercase_ascii name with 
"PBV" -> "Pass by value"
|"PBR" -> "Pass by reference"
|"PBCR" -> "Pass by copy-restore"
|"PBN" -> "Pass by name"
|"PBNE" -> "Pass by need"
|_-> name 

(**Method that returns the name of a value(meaning of an instance in integer form) for the factor entity
@param num: integer meaning of an instance
@param new_entity_instance_list : list of new instances for the factor entity  *)				
let return_factor_instance num new_entity_instance_list= 
	match num with 
	1-> "val"
	|2-> "ref"
	|3-> "comp"
	|4-> "env"
	|5-> "ctn"
	|6-> "den"
	|_-> let rec get_instance new_entity_instance_list=
				match new_entity_instance_list with 
				[] -> "none"
				|(a,b) :: tl -> if b = num then a else get_instance tl 
				in get_instance new_entity_instance_list 

(**Method to get the name and value for the various instance (including new instance) for the factor "context"
@param val_name : instance of the factor
@param new_context_instance_list: list that holds new instance for the factor "context" *)
let context_name val_name new_context_instance_list =
	match String.lowercase_ascii val_name with
		"cln" ->{fact_name = val_name ; num=1 ; new_context_instance_list=new_context_instance_list}
		|"cld" ->{fact_name = val_name ; num=2 ; new_context_instance_list=new_context_instance_list}
		|"cnd" -> {fact_name = val_name ; num=3 ; new_context_instance_list=new_context_instance_list}
		|"oth" ->{fact_name = val_name ; num=4 ; new_context_instance_list=new_context_instance_list}
		|_-> let ret new_context_instance_list= let () = print_string "-->Give name for factor: " in let newfac_name = read_line() in 
		let () = print_string "-->Give value for factor: "
						 in let num = validate_input()
						 in let new_instance = (newfac_name,num) 
						 in let new_context_instance_list = insert_new_instance new_instance new_context_instance_list 
						 in {fact_name = newfac_name;num=num; new_context_instance_list=new_context_instance_list}
						 in ret new_context_instance_list

(**Method that returns the name of a value(meaning of an instance in integer form) for the factor context
@param num: integer meaning of an instance
@param new_entity_instance_list : list of new instances for the factor context  *)	
let return_factor_context_instance num new_context_instance_list= 
match num with 
1-> "cln"
|2-> "cld"
|3-> "cnd"
|4-> "oth"
|5-> "Default"
|_-> let rec get_instance new_context_instance_list=
			match new_context_instance_list with 
			[] -> "none"
			|(a,b) :: tl -> if b = num then a else get_instance tl 
			in get_instance new_context_instance_list 
						

let eval_name val_name new_eval_instance_list =
	match String.lowercase_ascii val_name with
		"str" -> {fact_name = val_name ; num=1 ; new_eval_instance_list=new_eval_instance_list}
		|"laz" -> {fact_name = val_name ; num=2 ; new_eval_instance_list=new_eval_instance_list}
		|"non" -> {fact_name = val_name ; num=3 ; new_eval_instance_list=new_eval_instance_list}
		|"man" -> {fact_name = val_name ; num=4 ; new_eval_instance_list=new_eval_instance_list}
		|_->  let ret new_eval_instance_list= let () = print_string "-->Give name for factor: " in let newfac_name = read_line() in 
						let () = print_string "-->Give value for factor: "
						 in let num = validate_input()
						 in let new_instance = (newfac_name,num) 
						 in let new_eval_instance_list = insert_new_instance new_instance new_eval_instance_list 
						 in {fact_name = newfac_name;num=num; new_eval_instance_list=new_eval_instance_list}
						 in ret new_eval_instance_list


let return_factor_eval_instance num new_eval_instance_list= 
match num with 
1-> "str"
|2-> "laz"
|3-> "non"
|4-> "man"
|_-> let rec get_instance new_eval_instance_list=
			match new_eval_instance_list with 
			[] -> "none"
			|(a,b) :: tl -> if b = num then a else get_instance tl 
			in get_instance new_eval_instance_list


let typ_name val_name new_typ_instance_list=
	match String.lowercase_ascii val_name with
		"yes" -> {fact_name = val_name ; num=1 ; new_typ_instance_list=new_typ_instance_list}
		|"no" -> {fact_name = val_name ; num=2 ; new_typ_instance_list=new_typ_instance_list}
		|"non" -> {fact_name = val_name ; num=3 ; new_typ_instance_list=new_typ_instance_list}
		|_->  let ret new_typ_instance_list= let () = print_string "-->Give name for factor: " in let newfac_name = read_line() in 
            let () = print_string "-->Give value for factor: "
             in let num = validate_input()
             in let new_instance = (newfac_name,num) 
             in let new_typ_instance_list = insert_new_instance new_instance new_typ_instance_list 
             in {fact_name = newfac_name;num=num; new_typ_instance_list=new_typ_instance_list}
             in ret new_typ_instance_list

let return_factor_typ_instance num new_typ_instance_list= 
match num with 
1-> "yes"
|2-> "no"
|3-> "non"
|_-> let rec get_instance new_typ_instance_list=
      match new_typ_instance_list with 
      [] -> "none"
      |(a,b) :: tl -> if b = num then a else get_instance tl 
      in get_instance new_typ_instance_list
            
              


(**Method that give users the ability to select abitrary numbers of factors and also 
initialise selected factors of interest from the user 
@param record : record that holds Default instances(value) for parameter passing style factors
The preserve default method preserve the default value of factors not of interest to the user*)

let rec factor_initializer record = 
let () = help() in let () = print_string "-->Give selected factor type for the passing style: " 
in let t1 = read_line ()
in let t = (factor_name t1)
in if t = 1 then 
 let () = print_string "Possible instances: (val, ref, comp, env, new)\n"
 in let () = print_string " -->Initialise selected factor(see help menu for possible value): " 
 in let y1 = read_line()
 in let new_entity_instance_list1 = (ent_name y1 (record.new_entity_instance_list))
 in let new_entity_instance_list2 = new_entity_instance_list1.new_entity_instance_list
 in let y = new_entity_instance_list1.num
 in 
 let () = print_string "More factors? (y/n): " in let opt = read_line() in 
		if opt.[0] = 'y' then
				factor_initializer ({record with 
				ent = insert_instance (record.ent) y; new_entity_instance_list = new_entity_instance_list2}) 
		else 
		preserve_default 	{ record with 
			ent = insert_instance (record.ent) y; new_entity_instance_list = new_entity_instance_list2 }
		else 
	if t = 2 then 
	  let () = print_string "Possible instances: (cld, cln, cnd, new)\n"
		in let () = print_string " -->Initialise selected factor(see help menu for possible value): " 
		in let y1 = read_line()
		in let new_context_instance_list1 = (context_name y1 (record.new_context_instance_list))
 		in let new_context_instance_list2 = new_context_instance_list1.new_context_instance_list 
		in let y = new_context_instance_list1.num in 
		let () = print_string "More factors? (y/n): " in let opt = read_line() in 
			if opt.[0] = 'y' then
				factor_initializer ({record with 
				context = insert_instance (record.context) y; new_context_instance_list = new_context_instance_list2 })  
			else 
			preserve_default	{record with 
				context = insert_instance (record.context) y ; new_context_instance_list = new_context_instance_list2}
	else 
		if t = 3 then
			let () = print_string "Possible instances: (str, laz,non, new)\n"
			in let () = print_string " -->Initialise selected factor(see help menu for possible value): " 
			in let y1 = read_line() 
			in let new_eval_instance_list1 = (eval_name y1 (record.new_eval_instance_list))
			in let new_eval_instance_list2 = new_eval_instance_list1.new_eval_instance_list 
			in let y = new_eval_instance_list1.num in 
			let () = print_string "More factors? (y/n): " in let opt = read_line() in 
			if opt.[0] = 'y' then
				factor_initializer ({record with 
				eval =  insert_instance (record.eval) y; new_eval_instance_list = new_eval_instance_list2 })  
			else 
			 preserve_default {record with 
			 eval = insert_instance (record.eval) y ; new_eval_instance_list = new_eval_instance_list2 }
		else 
	if t = 4 then 
		let () = print_string "Possible instances: (new, yes, no)\n"
		in let () = print_string " -->Initialise selected factor(see help menu for possible value): " 
		in let y1 = read_line() 
    in let new_typ_instance_list1 = (typ_name y1 (record.new_typ_instance_list))
    in let new_typ_instance_list2 = new_typ_instance_list1.new_typ_instance_list 
    in let y = new_typ_instance_list1.num in 
		let () = print_string "More factors? (y/n): " in let opt = read_line() in 
			if opt.[0] = 'y' then
				factor_initializer ({record with 
				typ =  insert_instance (record.typ) y; new_typ_instance_list = new_typ_instance_list2})  
			else
			preserve_default {record with 
			typ = insert_instance (record.typ) y; new_typ_instance_list = new_typ_instance_list2 }
  else 
		preserve_default record

(**Method that takes a constructed interpretation and inserts in the interpretation list
@param (k,v) : constructed interpretation in key-value pair *)
let rec insert_interpretation (k,v) interpret_list = 
	match interpret_list with
	[] -> let () = print_string "\nInterpretation successfully added.\n" in  (k,v) :: []
	|(a,b) :: t -> if (a,b) = (k,v) then interpret_list else (a,b) :: insert_interpretation (k,v) t 
	
(**Method that deletes an interpretation for a passing style. It uses the passing style name to delete the interpretation
@param name : passing style name
@param interpret_list : list of interpretations *)
let rec delete_interpretation name interpret_list = 
	match interpret_list with 
	[] ->let () = print_string "\nNo interpretation for the passing style\n" in  []
	|(a,b) :: t -> if String.lowercase_ascii (getName a) = (String.lowercase_ascii name) then 
																					let () = print_string "\n Interpretation deleted successfully." 
																					in t 
																				else 
																					(a,b) :: delete_interpretation name t 

(**Method that takes a constructed interpretation and inserts in the interpretation list
@param (k,v) : constructed interpretation in key-value pair *)
let insert_new_interpretation name interpretation_list = 
	let () = print_string "--> Give interpretation(text on how style works): "
	in let interpretation = read_line()
	in let interpreter = (Passing_style name, interpretation)
  in insert_interpretation interpreter interpretation_list
 
(**Method that takes a constructed effect and inserts in the effect list
@param (k,v) : constructed effect in key-value pair *)
let rec insert_effect (k,v) effect_list = 
  match effect_list with
  [] -> let () = print_string "\nEffect successfully added.\n" in  (k,v) :: []
  |(a,b) :: t -> if (a,b) = (k,v) then effect_list else (a,b) :: insert_effect (k,v) t 

(**Method that takes a constructed effect and inserts in the effect list
@param (k,v) : constructed effectn in key-value pair *)
let insert_new_effect name effect_list = 
  let () = print_string "\n--> Give effect(induced by values of factors): "
  in let effect = read_line()
  in let sty_effect = (Passing_style name, effect)
  in insert_effect sty_effect effect_list

(*retieves an interpretation for a passing style 
@param name : name of the passing style
@param : interpretation list
**)	
let rec look_up_interpretation name interpret_list = 
	match interpret_list with
	[] -> "No interpretation."
  |(a,b) :: t -> if String.lowercase_ascii (getName name) = String.lowercase_ascii (getName a) 
                    then 
                        b 
										else look_up_interpretation name t 
										
(*retieves an effect for a passing style *)
let rec look_up_effect name effect_list = 
	match effect_list with
	[] -> "No effect."
	|(a,b) :: t -> if String.lowercase_ascii (getName name) = String.lowercase_ascii (getName a) 
										then 
												b 
										else look_up_effect name t 

(*Function to print instances for the factor "entity"**)	
let print_list_li new_entity_instance_list li =
let rec print_list new_entity_instance_list li = 
	match li with 
	[] -> ()
	|hd :: tl -> if  List.length tl = 0
								then
								(print_string (ful_name(return_factor_instance hd new_entity_instance_list)); 
								print_string ""; print_list new_entity_instance_list tl )								
								else 
								(print_string (ful_name(return_factor_instance hd new_entity_instance_list)); 
								print_string ";"; print_list new_entity_instance_list tl )
								in 
	print_string "[";
	print_list new_entity_instance_list li ;
	print_string "]"

(*Function to print instances for the factor "context"**)
let print_list_li_context new_context_instance_list li =
	let rec print_list new_context_instance_list li = 
		match li with 
		[] -> ()
		|hd :: tl -> if  List.length tl = 0 
						then
							(print_string (ful_name_context(return_factor_context_instance hd new_context_instance_list));
							print_string ""; print_list new_context_instance_list tl )
						else 
							(print_string (ful_name_context(return_factor_context_instance hd new_context_instance_list));
							print_string ";"; print_list new_context_instance_list tl)
						in 
						print_string "[";
						print_list new_context_instance_list li ;
						print_string "]"

(*Function to print instances for the factor "evaluation strategy"**)
let print_list_li_eval new_eval_instance_list li =
	let rec print_list new_eval_instance_list li = 
		match li with 
		[] -> ()
		|hd :: tl ->if List.length tl = 0 
								then 
									(print_string (ful_name_eval(return_factor_eval_instance hd new_eval_instance_list)) ; 
									print_string ""; print_list new_eval_instance_list tl) 
								else
									(print_string (ful_name_eval(return_factor_eval_instance hd new_eval_instance_list)) ; 
									print_string ";"; print_list new_eval_instance_list tl) 
								in 
								print_string "[";
								print_list new_eval_instance_list li ;
								print_string "]"
    


let print_list_li_typ new_typ_instance_list li =
  let rec print_list new_typ_instance_list li = 
    match li with 
    [] -> ()
		|hd :: tl ->if List.length tl = 0 
								then 
									(print_string (ful_name_typ(return_factor_typ_instance hd new_typ_instance_list)); 
									print_string ""; print_list new_typ_instance_list tl)
								else 
									(print_string (ful_name_typ(return_factor_typ_instance hd new_typ_instance_list)); 
									print_string ";"; print_list new_typ_instance_list tl)
						in 
						print_string "[";
						print_list new_typ_instance_list li ;
						print_string "]"

(**Method that prints (show) names of available styles in the structure 
@param li : list of styles*)
let print_styles_name li =
	let rec print_list li = 
		match li with 
		[] -> ()
		|hd :: tl ->if List.length tl = 0 
								then 
									(print_string (getName hd.name); 
									print_string ""; print_list  tl)
								else 
									(print_string (getName hd.name); 
									print_string ";"; print_list tl)
						in 
						print_string "(";
						print_list  li ;
						print_string ")"

(*Method to print a list of passing style 
	@param styles : list of styles
	**)	
let print_styles styles new_entity_instance_list new_context_instance_list new_eval_instance_list new_typ_instance_list=
let rec print_style_list styles=
	match styles with 
	[] -> ()
	|{name = Passing_style a;	factor ={entity = Entity_type b; context = Context_type c; evaluation = Evaluation_strat d;
		typing = Correct_type e}} :: tl
			-> print_endline "{name = "; print_string (ful_name_style a); print_endline ";"; 
										print_string "factors = {entity = ";
										print_list_li new_entity_instance_list b;print_string "; context = ";
										print_list_li_context new_context_instance_list c;
										print_string "; evaluation = ";
										print_list_li_eval new_eval_instance_list d;print_string " typing = "; 
										print_list_li_typ new_typ_instance_list e;print_endline "}};";
										print_style_list tl
						in print_string "[";
							print_style_list styles;
							print_string "]"

(*Method to print a specific passing style 
	@param styles : list of styles
	**)					 
let print_specific_style record new_entity_instance_list new_context_instance_list new_eval_instance_list new_typ_instance_list= 
match record with 
{name = Passing_style a;	factor ={entity = Entity_type b; context = Context_type c; evaluation = Evaluation_strat d;
		typing = Correct_type e}} 
		 -> print_endline "{name = "; print_string (ful_name_style a); print_endline ";"; 
												 print_string "factors = {entity = ";print_list_li new_entity_instance_list b;
												 print_string "; context = ";print_list_li_context new_context_instance_list c;
												 print_string "; evaluation = ";print_list_li_eval new_eval_instance_list d;
												 print_string " typing = "; print_list_li_typ new_typ_instance_list e;print_endline "}}"

(*Method to print specific factors of a passing style
			@param record : passing style
			**)	
let print_factors record new_entity_instance_list new_context_instance_list new_eval_instance_list new_typ_instance_list= 
	match record.factor with
	{entity = Entity_type b; context = Context_type c; evaluation = Evaluation_strat d;typing = Correct_type e}
								->  print_string "factors = {entity = ";print_list_li new_entity_instance_list b;
								print_string "; context = ";print_list_li_context new_context_instance_list c;
								print_string "; evaluation = ";print_list_li_eval new_eval_instance_list d;
								print_string " typing = "; print_list_li_typ new_typ_instance_list e;print_endline "}"



(**Method to Insert passing style into updated list of styles at runtime
@param styles: Updated list of passing styles we want to add new passing style
*)
let insert_new_passing_style styles interpretation_list effect_list record = 
  let record = factor_initializer record in 
	let name = get_name (Entity_type record.ent) (Context_type record.context) (Evaluation_strat record.eval) (Correct_type record.typ) ; in 
	let styles = sanitize (Entity_type record.ent) (Context_type record.context) (Evaluation_strat record.eval) (Correct_type record.typ) styles name
	in let interp = insert_new_interpretation name interpretation_list
	in let effect = insert_new_effect name effect_list
	in 
	{ styles = styles ; interp = interp; record = record; effect = effect }

(**Method to Remove a passing style from list of passing styles
@param styles: list of passing styles
*)
let rec delete_passing_style styles interpretation_list record effect= 
	let () = print_string " Available styles: " in 
	let () = print_styles_name styles in
	let () = print_string "\n-->Enter name for the style: " in 
	let name = read_line() in
	let rec removal styles interpretation_list= 	   
		match styles with
		[] -> let () = print_string "Style don't exist." in [] 
		|hd :: tl -> if String.lowercase_ascii (name)= String.lowercase_ascii (getName hd.name) 
												then let () = print_string "Passing style " in  
												let () = print_string "''" in 
												let () = print_string (ful_name_style name) 
												in  let () = print_string "''" in
												remove_passing_style styles hd else 
												hd :: removal tl interpretation_list
	in let styles = removal styles interpretation_list  
	in let interp = delete_interpretation name interpretation_list
	in 
	{ styles = styles ; interp = interp; record = record ; effect = effect} 

(*Method that outputs the properties of a specific passing style in the structure (list of passing styles)
@param styles : list of styles
**)		
let rec view_style styles interpretation_list new_entity_instance_list new_context_instance_list new_eval_instance_list new_typ_instance_list effect_list=
let () = print_string "\n-->Give passing style name: "
in let ps_name = read_line() 
in
let rec view styles interpretation_list new_entity_instance_list= 
match styles  with 
[] -> let () = print_string "No such passing style, try again.\n" in ()
|hd :: t -> 
				if String.lowercase_ascii (ps_name)= String.lowercase_ascii (getName hd.name)
				then 
(						
	let () = print_specific_style (paramstyle (hd.factor.entity) (hd.factor.context) (hd.factor.evaluation) (hd.factor.typing)  styles ps_name) new_entity_instance_list new_context_instance_list new_eval_instance_list new_typ_instance_list
	in let () = print_string "\nInterpretation =  "																	
	in let () =	 print_string (look_up_interpretation (setName ps_name) interpretation_list)
	in let () = print_string "\n"
	in let () = print_factors (paramstyle (hd.factor.entity) (hd.factor.context) (hd.factor.evaluation) (hd.factor.typing)  styles ps_name) new_entity_instance_list new_context_instance_list new_eval_instance_list new_typ_instance_list

	in let () = print_string "\nEffects = " in 
		print_string   (look_up_effect (setName ps_name) effect_list)  				
)											
				else view t interpretation_list new_entity_instance_list
				in let styles = styles
				in let new_entity_instance_list = new_entity_instance_list																	
				in view styles interpretation_list new_entity_instance_list

(*Method to view a specific passing styles in the structure (list of passing styles)
@param styles : list of styles
**)		
let view_specific_passing_style style interpretation_list record effect_list=
	let () = print_string " Available styles: " in let () = print_styles_name style in
  let () = 
  view_style style interpretation_list record.new_entity_instance_list record.new_context_instance_list record.new_eval_instance_list record.new_typ_instance_list effect_list
  in 
  {styles = style;interp=interpretation_list; record = record ; effect = effect_list}

(*Method to view all passing styles in the structure (list of passing styles)
@param styles : list of styles
@param interpretation: list of interpretation
**)		
let display_styles styles interpretation_list record effect= 
let styles =  styles
in let () = print_styles styles record.new_entity_instance_list record.new_context_instance_list record.new_eval_instance_list record.new_typ_instance_list
in let interp = interpretation_list 
in  
{ styles = styles ; interp = interp;record = record ; effect = effect}

let getOpinion op = match op with 
1 -> "Add_style"
|2 -> "Del_style"
|3 -> "Show_styles"
|4 -> "Specific_style"
|5 -> "Help"
|_ -> "Exit"
;;

let () = print_string "\nWelcome\nTo test for known style(hardcoded), enter values for it factors\n 
Else other values defines a new style\n" 

(**Method that gives users options on they may wish to do and initiates the action selected by the user.
	@param user_styles : list of passing style to work with	
*)
let rec usermind user_styles interpretation_list record effect_list= 
let () = display_menu () 
in let () = print_string " Enter your choice (1-6): "
in let opinion  = validate_input() in 
  match (getOpinion opinion) with
  |"Add_style"-> let update = (insert_new_passing_style user_styles interpretation_list effect_list record ) in
									let record = {update.record with ent = [5]; context = [5];eval = [5]; typ = [5] } in
                   usermind update.styles update.interp record  update.effect
          
  |"Del_style"->let update = (delete_passing_style user_styles interpretation_list record effect_list) in	
                
                   usermind  update.styles update.interp update.record update.effect
        
  |"Show_styles"->let update = (display_styles user_styles interpretation_list record effect_list) in 
  
                   usermind  update.styles update.interp update.record update.effect
        
  |"Specific_style"->let update = (view_specific_passing_style user_styles interpretation_list record effect_list) in
    
                  usermind  update.styles update.interp update.record update.effect
        
  |"Help"->let update = (user_facing_information user_styles interpretation_list record effect_list) in
  
                  usermind  update.styles update.interp update.record update.effect
  |_-> ()				  

;;

usermind user_styles interpretation_list  record effect_list
;;

