open Types

let is_parser_fail res = match res with
  | PFailure _ -> true
  | _ -> false

let is_parser_message res = match res with
  | PMessage _ -> true
  | _ -> false

let is_database_result res = match res with
  | PFailure _ -> false
  | PMessage _ -> false
  | Success -> true
  | Failure _ -> true

TEST "EXIT invalid" =
  is_parser_fail (fst (Parser.evaluate "EXIT extra params"))

TEST "EXIT valid caps" =
  is_parser_message (fst (Parser.evaluate "EXIT"))

TEST "EXIT valid lowercase" =
  is_parser_message (fst (Parser.evaluate "exit"))


TEST "HELP invalid" =
  is_parser_fail (fst (Parser.evaluate "HELP extra params"))

TEST "HELP valid caps" =
  is_parser_message (fst (Parser.evaluate "HELP"))

TEST "HELP valid lowercase" =
  is_parser_message (fst (Parser.evaluate "help"))


TEST "LOAD invalid no params" =
  is_parser_fail (fst (Parser.evaluate "LOAD"))

TEST "LOAD invalid extra params" =
  is_parser_fail (fst (Parser.evaluate "LOAD extra params"))

TEST "LOAD valid caps" =
  is_database_result (fst (Parser.evaluate "LOAD filename"))

TEST "LOAD valid lowercase" =
  is_database_result (fst (Parser.evaluate "load filename"))


TEST "CREATE TABLE invalid no params" =
  is_parser_fail (fst (Parser.evaluate "CREATE TABLE"))

TEST "CREATE TABLE invalid too few params" =
  is_parser_fail (fst (Parser.evaluate "CREATE TABLE tablename"))

TEST "CREATE TABLE valid caps" =
  is_database_result (fst (Parser.evaluate "CREATE TABLE col1 col2 col3"))

TEST "CREATE TABLE valid lowercase" =
  is_database_result (fst (Parser.evaluate "create table col1 col2"))


TEST "DROP TABLE invalid no params" =
  is_parser_fail (fst (Parser.evaluate "DROP TABLE"))

TEST "DROP TABLE invalid extra params" =
  is_parser_fail (fst (Parser.evaluate "DROP TABLE extra params"))

TEST "DROP TABLE valid caps" =
  is_database_result (fst (Parser.evaluate "DROP TABLE tablename"))

TEST "DROP TABLE valid lowercase" =
  is_database_result (fst (Parser.evaluate "drop table tablename"))


TEST "INSERT INTO invalid no params" =
  is_parser_fail (fst (Parser.evaluate "INSERT INTO"))

TEST "INSERT INTO invalid too few params" =
  is_parser_fail (fst (Parser.evaluate "INSERT INTO tablename"))

TEST "INSERT INTO invalid param mismatch" =
  is_parser_fail (fst (Parser.evaluate "INSERT INTO tablename col1 VALUES"))

TEST "INSERT INTO invalid fewer columns than values" =
  is_parser_fail (fst (Parser.evaluate "INSERT INTO tablename col1 VALUES col1 col2"))

TEST "INSERT INTO valid fewer values than columns" =
  is_database_result (fst (Parser.evaluate "INSERT INTO tablename col1 col2 VALUES val1"))

TEST "INSERT INTO valid caps" =
  is_database_result (fst (Parser.evaluate "INSERT INTO tablename col1 col2 VALUES val1 val2"))

TEST "INSERT INTO valid lowercase" =
  is_database_result (fst (Parser.evaluate "insert into tablename col1 col2 values val1 val2"))


TEST "DELETE FROM invalid no params" =
  is_parser_fail (fst (Parser.evaluate "DELETE FROM"))

TEST "DELETE FROM invalid bad params" =
  is_parser_fail (fst (Parser.evaluate "DELETE FROM tname nokeyword col='val'"))

TEST "DELETE FROM invalid bad params" =
  is_parser_fail (fst (Parser.evaluate "DELETE FROM tname WHERE col=noquotes"))

TEST "DELETE FROM valid delete all" =
  is_database_result (fst (Parser.evaluate "DELETE FROM tname"))

TEST "DELETE FROM valid delete all" =
  is_database_result (fst (Parser.evaluate "DELETE * FROM tname"))

TEST "DELETE FROM valid with WHERE" =
  is_database_result (fst (Parser.evaluate "DELETE FROM tname WHERE col='val'"))

TEST "DELETE FROM valid with WHERE spaces" =
  is_database_result (fst (Parser.evaluate "DELETE FROM tname WHERE col = 'val'"))

TEST "DELETE FROM valid lowercase" =
  is_database_result (fst (Parser.evaluate "delete from tname where col='val'"))

(*
TEST "UPDATE invalid no params" =
  is_parser_fail (fst (Parser.evaluate "UPDATE"))

TEST "UPDATE invalid too few params" =
  is_parser_fail (fst (Parser.evaluate "UPDATE tablename"))

TEST "UPDATE invalid too few params" =
  is_parser_fail (fst (Parser.evaluate "UPDATE tablename SET"))

TEST "UPDATE invalid bad syntax" =
  is_parser_fail (fst (Parser.evaluate "UPDATE tablename SET col1=val1, col2=val2"))

TEST "UPDATE invalid bad syntax" =
  is_parser_fail (fst (Parser.evaluate "UPDATE tablename SET col1=val1 col2=val2"))

TEST "UPDATE valid no WHERE" =
  is_parser_fail (fst (Parser.evaluate "UPDATE tablename SET"))

TEST "UPDATE valid with WHERE" =
  is_parser_fail (fst (Parser.evaluate "UPDATE tablename SET col1='val1',col2='val2' WHERE col='val'"))

TEST "UPDATE valid lowercase" =
  is_parser_fail (fst (Parser.evaluate "update tablename set col1='val1',col2='val2' where col='val'"))
*)