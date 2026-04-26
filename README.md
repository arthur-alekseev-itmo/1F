# Язык 1Ф
_Данный репозиторий хранит реализацию языка 1Ф -- домашнее задание 2 по формальным языкам МСЕ_

Текст задания живет в TASK.md

### Авторы
- Алексеев Артур (лексер, ревью)
- Куликов Никита (тестцы, типцы)
- Георгий Евсеев (грамматика)
- Хлюпин Иван (грамматика)

## Часть 1. Описание грамматики

Описана в файле GRAMMAR.pdf

## Часть 2. Лексический анализ

Пока выполнен на языке OCaml.

Сборка и запуск

Сначала надо скачать OCaml (https://opam.ocaml.org/doc/Install.html)
```
brew install opam
```

Также надо скачать `dune`
```
opam install dune
eval $(opam env)
```

Зависимости
```
opam install --deps-only -y ./OneF.opam
```

Для сборки как приложения:
```
dune build
./_build/default/bin/main.exe --input <path> [--output <path>]
```

Для разработки:
```
dune exec OneF -- --input <path> [--output <path>]
```
