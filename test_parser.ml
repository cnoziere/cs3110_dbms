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
  | Success _ -> true
  | Failure _ -> true
  | _ -> true


TEST "NO DB EXIT invalid" =
  is_parser_fail (fst (Parser.evaluate "EXIT extra params"))

TEST "NO DB EXIT valid caps" =
  is_parser_message (fst (Parser.evaluate "EXIT"))

TEST "NO DB EXIT valid lowercase" =
  is_parser_message (fst (Parser.evaluate "exit"))


TEST "NO DB HELP invalid" =
  is_parser_fail (fst (Parser.evaluate "HELP extra params"))

TEST "NO DB HELP valid caps" =
  is_parser_message (fst (Parser.evaluate "HELP"))

TEST "NO DB HELP valid lowercase" =
  is_parser_message (fst (Parser.evaluate "help"))


TEST "NO DB LOAD invalid no params" =
  is_parser_fail (fst (Parser.evaluate "LOAD"))

TEST "NO DB LOAD invalid extra params" =
  is_parser_fail (fst (Parser.evaluate "LOAD extra params"))

TEST "NO DB LOAD valid caps" =
  is_database_result (fst (Parser.evaluate "LOAD filename"))

TEST "NO DB LOAD valid lowercase" =
  is_database_result (fst (Parser.evaluate "load filename"))


TEST "NO DB CREATE DATABASE invalid no params" =
  is_parser_fail (fst (Parser.evaluate "CREATE DATABASE"))

TEST "NO DB CREATE DATABASE valid caps" =
  is_database_result (fst (Parser.evaluate "CREATE DATABASE name"))

TEST "NO DB CREATE DATABASE valid lowercase" =
  is_database_result (fst (Parser.evaluate "create database name"))


let db =
  match InternalRep.create_database "simplest_db" with
    | Success db -> db
    | _ -> failwith "Will not occur"

TEST "EXIT invalid" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "EXIT extra params"))

TEST "EXIT valid caps" =
  is_parser_message (fst (Parser.evaluate_db (InternalRep.reset db) "EXIT"))

TEST "EXIT valid lowercase" =
  is_parser_message (fst (Parser.evaluate_db (InternalRep.reset db) "exit"))


TEST "HELP invalid" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "HELP extra params"))

TEST "HELP valid caps" =
  is_parser_message (fst (Parser.evaluate_db (InternalRep.reset db) "HELP"))

TEST "HELP valid lowercase" =
  is_parser_message (fst (Parser.evaluate_db (InternalRep.reset db) "help"))


TEST "LOAD invalid no params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "LOAD"))

TEST "LOAD invalid extra params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "LOAD extra params"))

TEST "LOAD valid caps" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "LOAD filename"))

TEST "LOAD valid lowercase" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "load filename"))


TEST "CREATE TABLE invalid no params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "CREATE TABLE"))

TEST "CREATE TABLE invalid too few params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "CREATE TABLE tname"))

TEST "CREATE TABLE valid caps" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "CREATE TABLE (col1, col2, col3)"))

TEST "CREATE TABLE valid lowercase" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "create table (col1, col2)"))


TEST "DROP TABLE invalid no params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "DROP TABLE"))

TEST "DROP TABLE invalid extra params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "DROP TABLE extra params"))

TEST "DROP TABLE valid caps" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "DROP TABLE tname"))

TEST "DROP TABLE valid lowercase" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "drop table tname"))


TEST "INSERT INTO invalid no params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "INSERT INTO"))

TEST "INSERT INTO invalid too no columns or values" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "INSERT INTO tname"))

TEST "INSERT INTO invalid param no values" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "INSERT INTO tname col1 VALUES"))

TEST "INSERT INTO invalid fewer columns than values" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db)
    "INSERT INTO tname col1 VALUES val1 val2"))

TEST "INSERT INTO valid fewer values than columns" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db)
    "INSERT INTO tname col1 col2 VALUES val1"))

TEST "INSERT INTO valid caps" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "INSERT INTO tname col1 col2 VALUES val1 val2"))

TEST "INSERT INTO valid lowercase" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "insert into tname col1 col2 values val1 val2"))


TEST "DELETE FROM invalid no tablename or where" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "DELETE FROM"))

TEST "DELETE FROM invalid no where" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "DELETE FROM tname"))

TEST "DELETE FROM invalid bad params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "DELETE FROM tname nokeyword col='val'"))

TEST "DELETE FROM invalid bad params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "DELETE FROM tname WHERE col=noquotes"))

TEST "DELETE FROM valid delete all with star" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "DELETE * FROM tname"))

TEST "DELETE FROM valid with WHERE" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "DELETE FROM tname WHERE col='val'"))

TEST "DELETE FROM valid with WHERE spaces" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "DELETE FROM tname WHERE col = 'val'"))

TEST "DELETE FROM valid lowercase" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "delete from tname where col='val'"))


TEST "UPDATE invalid no params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "UPDATE"))

TEST "UPDATE invalid no keywords" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "UPDATE tname"))

TEST "UPDATE invalid no SET or WHERE" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "UPDATE tname SET"))

TEST "UPDATE invalid SET param mismatch" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db)
    "UPDATE tname SET (col1='val1', col2) WHERE col='val'"))

TEST "UPDATE invalid SET param mismatch" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db)
    "UPDATE tname SET (col1='val1', 'val2') WHERE col='val'"))

TEST "UPDATE invalid SET no equals" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db)
    "UPDATE tname SET (col1 'val1',col2 'val2') WHERE col='val'"))

TEST "UPDATE invalid WHERE no quotes" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db)
    "UPDATE tname SET (col1='val1') WHERE col=val"))

TEST "UPDATE invalid WHERE no equals" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db)
    "UPDATE tname SET (col1='val1') WHERE col 'val'"))

TEST "UPDATE invalid no WHERE" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db)
    "UPDATE tname SET (col1='val1', col2='val2')"))

TEST "UPDATE valid with WHERE" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "UPDATE tname SET (col1='val1',col2='val2') WHERE col='val'"))

TEST "UPDATE valid lowercase" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "update tname set (col1='val1') where col='val'"))


TEST "SELECT FROM invalid no from" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "SELECT *"))

TEST "SELECT FROM invalid no tablename" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "SELECT * FROM"))

TEST "SELECT FROM invalid too many params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "SELECT * FROM tname extras"))

TEST "SELECT FROM invalid no columns" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "SELECT FROM tname"))

TEST "SELECT FROM valid with any" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "SELECT * FROM tname"))

TEST "SELECT FROM valid with conditions" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "SELECT (col1,col2) FROM tname"))

TEST "SELECT FROM valid with spaces" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "SELECT (col1 , col2) FROM tname"))

TEST "SELECT FROM valid lowercase" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "select (col1,col2) from tname"))


TEST "SELECT FROM WHERE invalid no where conditions" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "SELECT * FROM tname WHERE"))

TEST "SELECT FROM invalid too many params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT * FROM tname WHERE col='val' extras"))

TEST "SELECT FROM WHERE valid with any" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT * FROM tname WHERE col='val'"))

TEST "SELECT FROM WHERE valid with conditions" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT col1,col2 FROM tname WHERE col='val'"))

TEST "SELECT FROM WHERE valid with spaces" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT col1 , col2 FROM tname WHERE col = 'val'"))

TEST "SELECT FROM WHERE valid lowercase" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "select col1,col2 from tname where col='val'"))


TEST "SELECT FROM WHERE equals" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT * FROM tname WHERE col='val'"))

TEST "SELECT FROM WHERE equals" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT col1,col2,col3 FROM tname WHERE col='val'"))

TEST "SELECT FROM WHERE not equals" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT * FROM tname WHERE col<>'val'"))

TEST "SELECT FROM WHERE not equals" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT col1,col2,col3 FROM tname WHERE col<>'val'"))

TEST "SELECT FROM WHERE greater than" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT * FROM tname WHERE col>'val'"))

TEST "SELECT FROM WHERE greater than" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT col1,col2,col3 FROM tname WHERE col>'val'"))

TEST "SELECT FROM WHERE less than" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT * FROM tname WHERE col<'val'"))

TEST "SELECT FROM WHERE less than" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT col1,col2,col3 FROM tname WHERE col<'val'"))

TEST "SELECT FROM WHERE greater than equal to" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT * FROM tname WHERE col>='val'"))

TEST "SELECT FROM WHERE greater than equal to" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT col1,col2,col3 FROM tname WHERE col>='val'"))

TEST "SELECT FROM WHERE less than equal to" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT * FROM tname WHERE col<='val'"))

TEST "SELECT FROM WHERE less than equal to" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db)
    "SELECT col1,col2,col3 FROM tname WHERE col<='val'"))


TEST "PRINT invalid too many params" =
  is_parser_fail (fst (Parser.evaluate_db (InternalRep.reset db) "PRINT tname extras"))

TEST "PRINT valid" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "PRINT tname"))

TEST "PRINT valid lowercase" =
  is_database_result (fst (Parser.evaluate_db (InternalRep.reset db) "print tname"))


TEST_UNIT "print Success" = Parser.print_result (Success db)
TEST_UNIT "print Failure empty" = Parser.print_result (Failure "")
TEST_UNIT "print Failure" = Parser.print_result (Failure "x")
TEST_UNIT "print PMessage" = Parser.print_result (PMessage "")
TEST_UNIT "print PMessage" = Parser.print_result (PMessage "x")
TEST_UNIT "print PFailure" = Parser.print_result (PFailure "")
TEST_UNIT "print PFailure" = Parser.print_result (PFailure "x")

(* TEST_UNIT "print OpColumn" *)
