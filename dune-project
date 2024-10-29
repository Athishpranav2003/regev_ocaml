(lang dune 3.16)
(generate_opam_files true)

(name regev)
(source (github Athishpranav2003/regev-ocaml))
(authors "Athish Pranav Dharmalingam")
(maintainers "Athish Pranav Dharmalingam <Athish Pranav Dharmalingam>")

(package
 (name regev)
 (synopsis "Regev implementation OCaml")
 (description "Regev implementation OCaml")

(depends (zarith (>= 1.4))
         (mirage-crypto (>= 1.1.0))
)
)
