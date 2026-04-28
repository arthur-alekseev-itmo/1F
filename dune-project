(lang dune 3.20)

(name OneF)

(generate_opam_files true)

(source
 (gitlab arthur.alekseev.itmo/mse-fl))

(authors "Arthur Alekseev <arthur.alekseev.itmo@gmail.com> + OCaml PRO team")

(maintainers "Arthur Alekseev <arthur.alekseev.itmo@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name OneF)
 (synopsis "OneF Language interpreter")
 (description "OneF language interpeter")
 (depends
  ocaml
  uucp
  containers
  dune
  ppx_deriving
  sedlex)
 (tags
  (skibidi importozameshenie fp ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
