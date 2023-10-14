(lang dune 3.0)

(formatting
 (enabled_for ocaml))

(authors "\197\129ukasz Kurowski")

(maintainers "crackcomm@gmail.com")

(generate_opam_files true)

(package
 (name r710_fc)
 (synopsis "R710 fan control")
 (depends
  (async
   (>= 0.16.0))
  (core
   (>= 0.16.0))
  (yojson
   (>= 2.1.0))
  (ocaml
   (>= 4.07.0))))
