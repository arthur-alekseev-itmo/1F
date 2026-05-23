  $ ./test_interpreter.exe <<- EOF
  > пусть
  File: ??, line: 1, characters 0-5:
  пусть
  ^^^^^
  Let binding must have a name

  $ ./test_interpreter.exe <<- EOF
  > пусть икс
  File: ??, line: 1, characters 10-11:
  пусть икс
            ^
  Unexpected EOF, but awaited: "оператор ="

  $ ./test_interpreter.exe <<- EOF
  > пусть икс =
  File: ??, line: 1, characters 10-11:
  пусть икс =
            ^
  Awaited expr after eq

  $ ./test_interpreter.exe <<- EOF
  > пусть икс = 3

  $ ./test_interpreter.exe <<- EOF
  > пусть (альфа, бета, гамма =
  File: ??, line: 1, characters 6-27:
  пусть (альфа, бета, гамма =
        ^^^^^^^^^^^^^^^^^^^^^
  Unmatched brackets: got "оператор =" instead of "скобка )"

  $ ./test_interpreter.exe <<- EOF
  > пусть альфа = (
  File: ??, line: 1, characters 14-15:
  пусть альфа = (
                ^
  Bracket is unmached: EOF

  $ ./test_interpreter.exe <<- EOF
  > пусть альфа = (2 + 2 + 2
  File: ??, line: 1, characters 14-15:
  пусть альфа = (2 + 2 + 2
                ^
  Bracket is unmached: EOF

  $ ./test_interpreter.exe <<- EOF
  > пусть альфа = лямбда
  File: ??, line: 1, characters 14-20:
  пусть альфа = лямбда
                ^^^^^^
  Awaited at least one pattern

  $ ./test_interpreter.exe <<- EOF
  > пусть альфа = лямбда икс
  File: ??, line: 1, characters 25-26:
  пусть альфа = лямбда икс
                           ^
  Unexpected EOF, but awaited: "стрелка"

  $ ./test_interpreter.exe <<- EOF
  > пусть альфа = лямбда икс игрек
  File: ??, line: 1, characters 31-32:
  пусть альфа = лямбда икс игрек
                                 ^
  Unexpected EOF, but awaited: "стрелка"

  $ ./test_interpreter.exe <<- EOF
  > пусть _ = если
  File: ??, line: 1, characters 10-14:
  пусть _ = если
            ^^^^
  Awaited expr after "если"

  $ ./test_interpreter.exe <<- EOF
  > пусть _ = если то
  File: ??, line: 1, characters 10-14:
  пусть _ = если то
            ^^^^
  Awaited expr after "если"

  $ ./test_interpreter.exe <<- EOF
  > пусть _ = если 3 то
  File: ??, line: 1, characters 17-19:
  пусть _ = если 3 то
                   ^^
  Awaited expr after "то"

  $ ./test_interpreter.exe <<- EOF
  > пусть _ = если 3
  File: ??, line: 1, characters 17-18:
  пусть _ = если 3
                   ^
  Unexpected EOF, but awaited: "то"

  $ ./test_interpreter.exe <<- EOF
  > пусть _ = если да то нет
  File: ??, line: 1, characters 25-26:
  пусть _ = если да то нет
                           ^
  Unexpected EOF, but awaited: "иначе"

  $ ./test_interpreter.exe <<- EOF
  > пусть _ = если да то нет иначе
  File: ??, line: 1, characters 25-30:
  пусть _ = если да то нет иначе
                           ^^^^^
  Awaited expr after "иначе"

  $ ./test_interpreter.exe <<- EOF
  > пусть _ = если да то нет иначе да

  $ ./test_interpreter.exe <<- EOF
  > пусть () =
  > пусть () = () в
  > пусть () =
  >   . ошибка  в строке 4
  > в ()
  File: ??, line: 4, characters 2-3:
    . ошибка  в строке 4
    ^
  Awaited expr after "пусть"

  $ ./test_interpreter.exe <<- EOF
  > модуль БезТела
  File: ??, line: 1, characters 15-16:
  модуль БезТела
                 ^
  Unexpected EOF, but awaited: "оператор ="

  $ ./test_interpreter.exe <<- EOF
  > модуль СПлохимТелом = структура
  File: ??, line: 1, characters 32-33:
  модуль СПлохимТелом = структура
                                  ^
  Unexpected EOF, but awaited: "конец"

  $ ./test_interpreter.exe <<- EOF
  > модуль ПерепуталВыражениеСДекларацией = структура
  >   пусть икс = 32 в
  > конец
  File: ??, line: 2, characters 17-18:
    пусть икс = 32 в
                   ^
  Awaited: "конец", but got: "в"

  $ ./test_interpreter.exe <<- EOF
  > модуль Страшные скобки = структура
  >   пусть икс = ((((((((((((((((32)))))))))))))
  > конец
  File: ??, line: 1, characters 16-22:
  модуль Страшные скобки = структура
                  ^^^^^^
  Awaited: "оператор =", but got: "имя (маленькое) скобки"

  $ ./test_interpreter.exe <<- EOF
  > модуль СтрашныеСкобки = структура
  >   пусть икс = ((((((((((((((((32)))))))))))))
  > конец
  File: ??, line: 2, characters 14-16:
    пусть икс = ((((((((((((((((32)))))))))))))
                ^^
  Unmatched brackets: got "скобка (" instead of "скобка )"

  $ ./test_interpreter.exe <<- EOF
  > модуль СтрашныеСкобки = структура
  >   пусть икс = ((((((((((((32)))))))))))))
  > конец
  File: ??, line: 2, characters 40-41:
    пусть икс = ((((((((((((32)))))))))))))
                                          ^
  Awaited: "конец", but got: "скобка )"
