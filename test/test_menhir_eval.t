MenhirParser integration tests — parsing + evaluation via Menhir
  $ ./test_menhir_eval.exe <<- EOF
  > 3
  3

  $ ./test_menhir_eval.exe <<- EOF
  > 3 + 2 * 4
  11

  $ ./test_menhir_eval.exe <<- EOF
  > пусть икс = 4 в икс
  4

  $ ./test_menhir_eval.exe <<- EOF
  > пусть f x = x + 1 в f 5
  6

  $ ./test_menhir_eval.exe <<- EOF
  > пусть f x y = x + y в f 3 7
  10

  $ ./test_menhir_eval.exe <<- EOF
  > []
  []

  $ ./test_menhir_eval.exe <<- EOF
  > 2 :: (1 :: [])
  [2; 1]

  $ ./test_menhir_eval.exe <<- EOF
  > 1 :: 2 :: 3 :: []
  [1; 2; 3]

  $ ./test_menhir_eval.exe <<- EOF
  > [1; 2; 3; 4; 5]
  [1; 2; 3; 4; 5]

  $ ./test_menhir_eval.exe <<- EOF
  > сопоставить 5 с икс -> икс
  5

  $ ./test_menhir_eval.exe <<- EOF
  > сопоставить 5 с | икс -> икс
  5

  $ ./test_menhir_eval.exe <<- EOF
  > сопоставить [1; 2; 3] с
  > | голова :: хвост -> голова
  > | _ -> 99999
  1

  $ ./test_menhir_eval.exe <<- EOF
  > сопоставить [] с
  > | голова :: хвост -> голова
  > | _ -> 99999
  99999

  $ ./test_menhir_eval.exe <<- EOF
  > сопоставить [1; 2; 3] с
  > | голова :: хвост когда голова = 1 -> голова
  > | _ -> 99999
  1

  $ ./test_menhir_eval.exe <<- EOF
  > сопоставить [] с
  > | [] -> нет
  > | _ -> да
  нет

  $ ./test_menhir_eval.exe <<- EOF
  > пусть рек факториал n =
  >   если n < 2 то 1
  >   иначе факториал (n - 1) * n
  > в факториал 10
  3628800

  $ ./test_menhir_eval.exe <<- EOF
  > пусть рек карта f список =
  >   сопоставить список с
  >   | [] -> []
  >   | э :: хв -> f э :: карта f хв
  > в
  > пусть двойное x = x * 2 в
  > карта двойное [1; 2; 3; 4; 5]
  [2; 4; 6; 8; 10]


