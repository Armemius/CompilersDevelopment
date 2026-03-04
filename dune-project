(lang dune 3.21)

(name compiler)

(generate_opam_files true)

(source
 (github Armemius/CompilersDevelopment))

(authors "Armemius rusgy@yandex.ru")

(maintainers "Armemius rusgy@yandex.ru")

(license LICENSE)

(package
 (name compiler)
 (synopsis "Basic educational compiler")
 (description "This repository contains source codes for compiler that is developed in terms
of the course in ITMO University. The compiler is written in OCaml and the
language is designed to be simple and easy to understand.")
 (depends ocaml)
)
