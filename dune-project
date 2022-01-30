(lang dune 2.7)
(using menhir 2.0)
(name omcrl2)

(generate_opam_files true)
(version unreleased)

(authors "Paul Monson")
(maintainers "pmonson711@gmail.com")
(license MIT)
(source (uri "git+http://gitlab.com"))
(homepage "http://gitlab.com")
(bug_reports "http://gitlab.com")
(documentation "http://gitlab.com")

(package
  (name omcrl2)
  (synopsis "Parser generater for mCrl2")
  (description "Parser generater for mCrl2")
  (depends
    (dune (>= 2.5))
    (ocaml (>= 4.08))

    (cmdliner (>= 0.9))
    (bos (>= 0.2))
    (menhir (>= 20200624))
    (ppx_deriving (>= 4.4))

    ;; Testing
    (alcotest (and :with-test (>= 1.1)))
    (ppx_inline_test (and :with-test (>= v0.14)))
    (ppx_expect (and :with-test (>= v0.14)))
    (bisect_ppx (and :with_test (>= 2.7)))

    (merlin :dev)
    (ocamlformat :dev)
    (ocp-indent :dev)
    (ocp-index :dev)
    (odig :dev)
    (utop :dev)
  )
 )
