open Types
open Async.Std

(**
 * [update] returns a new database
 * fills the Ivar in the database (the deferred read in [updated]
 * becomes determined), and fills the new updated field with an empty Ivar
 *)
let update (db: database) (table_name: string) =
    let new_db = {db with updated = Ivar.create ()} in
    Ivar.fill db.updated (new_db, table_name);
    new_db

let reset db = {db with updated = Ivar.create ()}

let updated db = Ivar.read (db.updated)

let create_database (new_name: string): result =
    if new_name = "" then
        Failure "Database name cannot be empty string"
    else
        Success
        {
            name = new_name;
            data = Tst.create ();
            updated = Ivar.create ()
        }

let get_name db = db.name

let set_name new_name db =
    if new_name = "" then
        Failure "Database name cannot be empty string"
    else
        Success {db with name = new_name}


let create_table (db: database) (table_name: string) (col_names: string list): result =
    if col_names = [] then Failure ("No column names are provided to " ^
        "initialize table") else
    let (new_table: table) = Tst.create () in
    let rec add_cols table col_names =
        match col_names with
        | [] ->
            let (is_duplicate, new_data) = Tst.insert table_name table db.data in
            if is_duplicate then
                Failure "Table name already exists in database"
            else
                Success (update {db with data = new_data} table_name)
        | h::t ->
            let (new_column: column) = {data = Bst.create (); length = 0} in
            let (is_duplicate, new_table) = Tst.insert h new_column table in
            if is_duplicate then
                Failure "Duplicate column name used to initialize table"
            else
                add_cols new_table t in
    add_cols new_table col_names


let get_table_names (db: database): string list =
    let rec get_names = function
    | [] -> []
    | (name,_)::t -> name::get_names t in
    get_names (Tst.list_tst db.data)


(* Note: the dropped table will be passed to writeJSON. WriteJSON should note
 * that the table does not exist and clear or remove the file *)
let drop_table (db: database) (table_to_drop: string): result =
    let (success, new_data) = Tst.remove table_to_drop db.data in
    if success then
        Success (update {db with data = new_data} table_to_drop)
    else
        Failure (table_to_drop ^ " is not a table in the database")


(* Returns true if all items in sublst are a member of assoc_lst *)
let rec all_mem (sublst: 'a list) (assoc_lst: ('a * 'b) list): bool =
    match sublst with
    | [] -> true
    | h::t ->
        try
            ignore(List.assoc h assoc_lst);
            true && (all_mem t assoc_lst)
        with
        | Not_found -> false


let add_row (db: database) (table_name: string) (cols_to_change: string list)
    (vals: value list): result =
    if List.length cols_to_change <> List.length vals then
        Failure "Number of columns and values to be added do not match"
    else match Tst.get table_name db.data with
    | None -> Failure (table_name ^ " is not a table in the database")
    | Some table_to_change ->
        (* Cols is the associative list of col names and columns *)
        let cols = Tst.list_tst table_to_change in
        if cols = [] then
            Failure "Table has no columns"
        else if all_mem cols_to_change cols then
            (* Add a key, "", or the provided value to each column *)
            let rec add_cols table cols =
            match cols with
            | [] ->
                let (updated, new_data) = Tst.insert table_name table db.data in
                if updated then
                    Success (update {db with data = new_data} table_name)
                else
                    Failure (table_name ^ " is not a table in the database")
            | (name, curr_col)::t ->
                (* [find_cols]: find the value to be inserted into curr_col *)
                let rec find_val names vs =
                    match names, vs with
                    | h1::t1, h2::t2 ->
                        if h1 = name then h2
                        else find_val t1 t2
                    | _,_ -> "" in
                let (updated, new_col) =
                    Bst.insert curr_col.length
                        (find_val cols_to_change vals) curr_col.data in
                if updated then Failure "Row key already exists"
                else
                    let (updated, new_table) =
                        Tst.insert name
                        {length = curr_col.length + 1; data = new_col} table in
                    if updated then add_cols new_table t
                    else Failure (name ^ " is not a column in the table " ^ table_name) in
            add_cols table_to_change cols
        else
            Failure ("Provided columns do not exist in the table " ^ table_name)


let delete_row (db: database) (table_name: string) (key_to_delete: key): result =
    match Tst.get table_name db.data with
    | None -> Failure (table_name ^ " is not a table in the database")
    | Some table_to_change ->
        (* Cols is the associative list of col names and columns *)
        let cols = Tst.list_tst table_to_change in
        if cols = [] then
            Failure "Table has no columns"
        else
            (* [remove_key] removes the key_to_delete from all columns in cols *)
            let rec remove_key table cols =
            match cols with
            | [] ->
                let (updated, new_data) = Tst.insert table_name table db.data in
                if updated then
                    Success (update {db with data = new_data} table_name)
                else
                    Failure (table_name ^ " is not a table in the database")
            | (name, curr_col)::t ->
                let (x: column) = curr_col in
                let (removed, new_col) = Bst.remove key_to_delete (x.data) in
                if removed then
                    let (updated, new_table) =
                        Tst.insert name
                        {length = curr_col.length - 1; data = new_col} table in
                    if updated then remove_key new_table t
                    else Failure (name ^ " is not a column in the table " ^ table_name)
                else Failure "Key does not exist" in
            remove_key table_to_change cols


let update_value (db: database) (table_name: string) (column_name: string) (key_to_change: key)
    (value_to_add: value) : result =
    match Tst.get table_name db.data with
    | None -> Failure (table_name ^ " is not a table in the database")
    | Some table_to_change ->
        match Tst.get column_name table_to_change with
        | None -> Failure (column_name
            ^ " is not a column in the table " ^ table_name)
        | Some column_to_change ->
            let (updated, new_col) =
                Bst.insert key_to_change value_to_add column_to_change.data in
            if updated then
                let (updated, new_table) =
                    Tst.insert column_name
                    {column_to_change with data = new_col} table_to_change in
                if updated then
                    let (updated, new_data) = Tst.insert table_name new_table db.data in
                    if updated then
                        Success (update {db with data = new_data} table_name)
                    else Failure (table_name ^ " is not a table in the database")
                else Failure (column_name ^ " is not a column in the table " ^ table_name)
            else Failure "Key does not exist"


let get_column_names (db: database) (table_name: string): result =
    match Tst.get table_name db.data with
    | None -> Failure (table_name ^ " is not a table in the database")
    | Some selected_table ->
        (* Cols is the associative list of col names and columns *)
        let cols = Tst.list_tst selected_table in
        if cols = [] then Failure ("No columns exist in table" ^ table_name)
        else
            let rec get_names = function
            | [] -> []
            | (names, _)::t -> names::(get_names t) in
            ColNames (get_names cols)


let get_column_vals (db: database) (table_name: string) (column_name: string)
    (to_add: value -> bool): result =
    match Tst.get table_name db.data with
    | None -> Failure (table_name ^ " is not a table in the database")
    | Some selected_table ->
        match Tst.get column_name selected_table with
        | None -> Failure (column_name
            ^ " is not a column in the table " ^ table_name)
        | Some selected_col ->
            (* extract list of valid values from list of keys and values *)
            let rec check_vals = function
            | (_,v)::t -> if to_add v then v::check_vals t else check_vals t
            | [] -> [] in
            Column (check_vals (Bst.list_bst selected_col.data))


exception Empty_table

let get_row (db: database) (table_name: string) (column_name: string)
    (to_add: value -> bool): result =
    match Tst.get table_name db.data with
    | None -> Failure (table_name ^ " is not a table in the database")
    | Some selected_table ->
        try
            let column_name = if column_name = "" then
                match Tst.list_tst selected_table with
                | [] -> raise Empty_table (* Not possible due to parser check *)
                | (x,_)::t -> x
            else column_name in
            match Tst.get column_name selected_table with
            | None -> Failure (column_name
                ^ " is not a column in the table " ^ table_name)
            | Some selected_col ->
                (* extract list of valid keys from list of keys and values *)
                let rec check_keys = function
                | (k,v)::t -> if to_add v then k::check_keys t else check_keys t
                | [] -> [] in
                Keys (check_keys (Bst.list_bst selected_col.data))
        with
        | Empty_table -> Failure (table_name ^ " has no columns")


exception Key_not_found of key

let get_values (db: database) (table_name: string) (column_name: string) (keys: key list): result =
    match Tst.get table_name db.data with
    | None -> Failure (table_name ^ " is not a table in the database")
    | Some selected_table ->
        match Tst.get column_name selected_table with
        | None -> Failure (column_name
            ^ " is not a column in the table " ^ table_name)
        | Some selected_col ->
            (* extract list of values from list of keys *)
            let rec get_vals = function
            | [] -> []
            | k::t ->
                (match Bst.get k selected_col.data with
                | Some v -> v::get_vals t
                | None -> raise (Key_not_found k)) in
            try
                Column (get_vals keys)
            with
                | Key_not_found k -> (Failure ("The key " ^ string_of_int k
                    ^ " does not exist in the table " ^ table_name))


let create_whole_table (db: database) (table_name: string)
    (col_names: string list) (values: value list list): result =
    if col_names = [] then
        Failure "No column names are provided to initialize table"
    else if List.length col_names <> List.length values then
        Failure "List of columns and values do not match"
    else
    (* Add a list of list of values into list of columns *)
    let rec add_cols (added_table: table) (cols: string list) (vals: value list list) =
        match cols, vals with
        | [],[] ->
            let (is_duplicate, new_data) = Tst.insert table_name added_table db.data in
            if is_duplicate then
                Failure "Table name already exists in database"
            else
                Success (update {db with data = new_data} table_name)
        | col_name::next_names, val_lst::next_vals ->
            (* Add a list of values to a column *)
            let rec add_vals (col: column) (vals_to_add: value list) =
                match vals_to_add with
                | [] ->
                    (* Insert col into a table *)
                    let (is_duplicate, new_table) =
                        Tst.insert col_name col added_table in
                    if is_duplicate then
                        Failure "Duplicate column name used to initialize table"
                    else
                        add_cols new_table next_names next_vals
                | v::vs ->
                    (* Insert value v in column col *)
                    let (is_duplicate, new_col) =
                        Bst.insert col.length v col.data in
                    if is_duplicate then
                        Failure "Duplicate key used to initialize table"
                    else
                        add_vals {data = new_col; length = col.length + 1} vs in
            add_vals {data = Bst.create (); length = 0} val_lst
        | _,_ -> Failure "List of columns and values do not match" in
    add_cols (Tst.create ()) col_names values
