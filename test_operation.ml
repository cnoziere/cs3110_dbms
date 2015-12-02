open Types

let db =
  match InternalRep.create_database "simplest_db" with
    | Success db -> db
    | _ -> failwith "Will not occur"

TEST "concat_failures on all Success (would never be called in Operation)" =
  Operation.concat_failures [Success(db); Success(db); Success(db); Success(db)] ""
  = Failure("")

TEST "concat_failures on all Failure" =
  Operation.concat_failures [Failure("a"); Failure("b"); Failure("c")] ""
  = Failure(" a b c")

TEST "concat_failures on mixed" =
  Operation.concat_failures [Failure("a"); Success(db); Failure("b");
                             Failure("c")] ""
  = Failure(" a b c")


TEST "check_failures on all Success" =
  Operation.check_failures [Success(db); Success(db); Success(db); Success(db)]
  = Success(db)

TEST "check_failures on all Failure" =
  Operation.check_failures [Failure("a"); Failure("b"); Failure("c")]
  = Failure(" a b c")

TEST "check_failures on mixed" =
  Operation.check_failures [Failure("a"); Success(db); Failure("b"); Failure("c")]
  = Failure(" a b c")


TEST "concat_columns on all Failure" =
  Operation.concat_columns [Failure("a"); Failure("b"); Failure("c")] []
  = Failure("Not a column--will not reach this case.")

TEST "concat_columns on all Column" =
  Operation.concat_columns [Column(["a"]); Column(["b"]); Column(["c"; "d"])] []
  = OpColumn([["a"]; ["b"]; ["c"; "d"]])

TEST "concat_columns on mixed Failure and Column" =
  Operation.concat_columns [Failure("a"); Column(["b"]); Column(["c"; "d"]);
                            Failure("e")] []
  = Failure("Not a column--will not reach this case.")


TEST "filter_columns on all Failure" =
  Operation.filter_columns [Failure("a"); Failure("b"); Failure("c")]
  = Failure(" a b c")

TEST "filter_columns on all Column" =
  Operation.filter_columns [Column(["a"]); Column(["b"]); Column(["c"; "d"])]
  = OpColumn([["a"]; ["b"]; ["c"; "d"]])

TEST "filter_columns on mixed Failure and Column" =
  Operation.filter_columns [Failure("a"); Column(["b"]); Column(["c"; "d"]);
                            Failure("e")]
  = Failure(" a e")

