open Types

let is_fail res = match res with
  | Failure _ -> true
  | _ -> false

let is_message res = match res with
  | Message _ -> true
  | _ -> false


TEST "EXIT invalid" =
  is_fail (fst (Parser.evaluate "EXIT extra params"))

TEST "EXIT valid caps" =
  is_message (fst (Parser.evaluate "EXIT"))

TEST "EXIT valid lowercase" =
  is_message (fst (Parser.evaluate "exit"))


TEST "HELP invalid" =
  is_fail (fst (Parser.evaluate "HELP extra params"))

TEST "HELP valid caps" =
  is_message (fst (Parser.evaluate "HELP"))

TEST "HELP valid lowercase" =
  is_message (fst (Parser.evaluate "help"))


TEST "LOAD invalid no params" =
  is_fail (fst (Parser.evaluate "LOAD"))

TEST "LOAD invalid extra params" =
  is_fail (fst (Parser.evaluate "LOAD extra params"))

TEST "LOAD valid caps" =
  is_message (fst (Parser.evaluate "LOAD filename"))

TEST "LOAD valid lowercase" =
  is_message (fst (Parser.evaluate "load filename"))


TEST "CREATE TABLE invalid no params" =
  is_fail (fst (Parser.evaluate "CREATE TABLE"))

TEST "CREATE TABLE valid caps" =
  is_message (fst (Parser.evaluate "CREATE TABLE col1 col2 col3"))

TEST "CREATE TABLE valid lowercase" =
  is_message (fst (Parser.evaluate "create table col1 col2"))


TEST "DROP TABLE invalid no params" =
  is_fail (fst (Parser.evaluate "DROP TABLE"))

TEST "DROP TABLE invalid extra params" =
  is_fail (fst (Parser.evaluate "DROP TABLE extra params"))

TEST "DROP TABLE valid caps" =
  is_message (fst (Parser.evaluate "DROP TABLE tablename"))

TEST "DROP TABLE valid lowercase" =
  is_message (fst (Parser.evaluate "drop table tablename"))


TEST "INSERT INTO invalid no params" =
  is_fail (fst (Parser.evaluate "INSERT INTO"))

TEST "INSERT INTO invalid too few params" =
  is_fail (fst (Parser.evaluate "INSERT INTO tablename"))

TEST "INSERT INTO invalid param mismatch" =
  is_message (fst (Parser.evaluate "INSERT INTO tablename col1 VALUES"))

TEST "INSERT INTO invalid param mismatch" =
  is_message (fst (Parser.evaluate "INSERT INTO tablename col1 VALUES col1 col2"))

TEST "INSERT INTO valid caps" =
  is_message (fst (Parser.evaluate "INSERT INTO tablename col1 col2 VALUES val1 val2"))

TEST "INSERT INTO valid lowercase" =
  is_message (fst (Parser.evaluate "insert into tablename col1 col2 values val1 val2"))


TEST "DELETE FROM invalid no params" =
  is_fail (fst (Parser.evaluate "DELETE FROM"))

TEST "DELETE FROM invalid bad params" =
  is_fail (fst (Parser.evaluate "DELETE FROM tablename nokeywords col1='val1'"))

TEST "DELETE FROM invalid bad params" =
  is_fail (fst (Parser.evaluate "DELETE FROM tablename WHERE col1=val1"))

TEST "DELETE FROM valid delete all" =
  is_fail (fst (Parser.evaluate "DELETE FROM tablename"))

TEST "DELETE FROM valid delete all" =
  is_fail (fst (Parser.evaluate "DELETE * FROM tablename"))

TEST "DELETE FROM valid with WHERE" =
  is_fail (fst (Parser.evaluate "DELETE FROM tablename WHERE col='val'"))

TEST "DELETE FROM valid with WHERE" =
  is_fail (fst (Parser.evaluate "DELETE FROM tablename WHERE col = 'val'"))

TEST "DELETE FROM valid lowercase" =
  is_fail (fst (Parser.evaluate "delete from tablename where col='val'"))


TEST "UPDATE invalid no params" =
  is_fail (fst (Parser.evaluate "UPDATE"))

TEST "UPDATE invalid too few params" =
  is_fail (fst (Parser.evaluate "UPDATE tablename"))

TEST "UPDATE invalid too few params" =
  is_fail (fst (Parser.evaluate "UPDATE tablename SET"))

TEST "UPDATE invalid bad syntax" =
  is_fail (fst (Parser.evaluate "UPDATE tablename SET col1=val1, col2=val2"))

TEST "UPDATE invalid bad syntax" =
  is_fail (fst (Parser.evaluate "UPDATE tablename SET col1=val1 col2=val2"))

TEST "UPDATE valid no WHERE" =
  is_fail (fst (Parser.evaluate "UPDATE tablename SET"))

TEST "UPDATE valid with WHERE" =
  is_fail (fst (Parser.evaluate "UPDATE tablename SET col1='val1',col2='val2' WHERE col='val'"))

TEST "UPDATE valid lowercase" =
  is_fail (fst (Parser.evaluate "update tablename set col1='val1',col2='val2' where col='val'"))
