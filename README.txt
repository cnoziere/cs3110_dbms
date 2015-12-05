Compiling and running

Obtain the source code and navigate to the directory containing this README
file. Run the following two commands:

cs3110 compile parser.ml
cs3110 run parser.ml

This will start the DBMS prompt.

--------------------------------------------------------------------------------

Valid commands

EXIT
  - Exits the REPL.

HELP
  - Displays a list of commands with help on valid formats for parameters.

LOAD filename
  - Loads the backed up database contained in the JSON file with name filename.

CREATE DATABASE dbname
  - Creates a database with name dbname.
  - *** Note: There must be a database before doing operations on tables.

CREATE TABLE tablename (col1,col2,...)
  - Creates a table with name tablename and columns col1, col2, ...

DROP TABLE tablename
  - Drops the table with name tablename

INSERT INTO tablename (col1,col2,...) VALUES (val1,val2,...)
  - Inserts a new row into table with name tablename. Sets column name col1 to
  value val1, col2 to val2, ...

DELETE FROM tablename WHERE []
  - Deletes rows from table with name tablename.
  - Identifies the rows to delete by the WHERE condition. See below for help on
  WHERE.

DELETE * FROM tablename
  - Another way to delete rows. This deletes all rows from the table with name
  tablename.

UPDATE tablename SET (col1=val1, col2=val2, ...) WHERE []
  - Updates rows in the table with name tablename. Updates column name col1 to
  value val1, col2 to val2, …
  - Identifies the rows to update by the WHERE condition. See below for help on
  WHERE.

SELECT (col1,col2,...) FROM tablename WHERE []
  - Selects, and prints to terminal, the columns col1, col2, … from the table
  with name tablename.
  - Identifies the rows to print from by the WHERE condition. See below for
  help on WHERE.

SELECT * FROM tablename
  - Another way to select rows. This selects, and prints to terminal, every
  column in the table.

WHERE column_name OPERATOR value
  - WHERE filters rows for other operations. Rows that satisfy column_name
  OPERATOR value are selected.
  - OPERATOR must be one of the following:
    =
    <>
    >
    <
    >=
    <=
    All operators are string comparisons, as all values are saved as strings.

PRINT tablename
  - Prints the table with name tablename to the terminal.

--------------------------------------------------------------------------------

Example session

Try running the following commands line-by-line, while occasionally
calling PRINT tablename to see the effects. Also note that the .json
files update with every change. The files are in the directory which
shares the database name.


CREATE DATABASE Characters
    Note: if you're revisiting this example, Characters may already exist.
    Either [LOAD Characters], delete the directory [Characters] to try
    again, or use a different database name.

CREATE TABLE SesameSt (Name, Color, Team)
INSERT INTO SesameSt (Name, Color, Team) VALUES (Elmo, red, a)
INSERT INTO SesameSt (Name, Color, Team) VALUES (CookieMonster, blue, c)
INSERT INTO SesameSt (Name, Color, Team) VALUES (BigBird, yellow, b)
INSERT INTO SesameSt (Name, Color) VALUES (Oscar, green)
PRINT SesameSt
INSERT INTO SesameSt (Name, Color) VALUES (Count, purple)
UPDATE SesameSt SET (Team=a) WHERE (Color=green)
SELECT (Name, Color) FROM SesameSt WHERE (Team=a)

CREATE TABLE DoctorWho Name Species Planet
INSERT INTO DoctorWho (Name, Species, Planet) VALUES (Twelve, TimeLord, Gallifrey)
INSERT INTO DoctorWho (Name, Species) VALUES (Rose, human)
INSERT INTO DoctorWho (Name, Species, Planet) VALUES (Strax, Sontaran, Sontar)
INSERT INTO DoctorWho (Name, Species, Planet) VALUES (Vastra, Silurian, Earth)
PRINT DoctorWho
INSERT INTO DoctorWho (Name, Species, Planet) VALUES (Jack, immortal, Earth)
INSERT INTO DoctorWho (Species, Planet) VALUES (Dalek, Skaro)
INSERT INTO DoctorWho (Species) VALUES (Silence)
PRINT DoctorWho
UPDATE DoctorWho SET (Planet=Earth) WHERE (Species=human)
SELECT * FROM DoctorWho

DELETE FROM SesameSt WHERE (Team=c)
DELETE * FROM DoctorWho
INSERT INTO DoctorWho (Name, Species, Planet) VALUES (Twelve, TimeLord, Gallifrey)
DROP TABLE SesameSt

LOAD minimalDB
    Note that [LOAD minimalDB] switches the database to the database saved
    in minimalDB.json.
PRINT t1
PRINT t2
PRINT t3
