(lang dune 3.10)

(generate_opam_files true)

(authors "Jacob Choi" "Kushal Dulla" "Hans Harck T\195\184nning")

(maintainers "htonnin1@jhu.edu")

(bug_reports "https://github.com/TheHarcker/FPSE_Project")

(homepage "https://github.com/TheHarcker/FPSE_Project")

(package
 (name cli)
 (synopsis "A CLI for playing the classical game Sudoku")
 (description
  "A CLI for playing the classical game Sudoku. Features include a Hint system and a highscore list")
 (depends
  (ounit2
   (>= 2.2.7))
  (core
   (>= 0.16.2))
  (ppx_deriving
   (>= 5.2.1))
  (ppx_deriving_yojson
   (>= 3.7.0))
  (ppx_jane
   (>= v0.16.0))
  (core_unix
   (>= v0.16.0))
  (bisect_ppx
   (>= 2.8.3))))

(package
 (name server)
 (synopsis "A web app for playing the classical game Sudoku")
 (description
  "A web app for playing the classical game Sudoku. Features include a Hint system and a highscore list")
 (depends
  (ounit2
   (>= 2.2.7))
  (core
   (>= 0.16.2))
  (dream
   (>= 1.0.0~alpha5))
  (ppx_deriving
   (>= 5.2.1))
  (ppx_deriving_yojson
   (>= 3.7.0))
  (ppx_jane
   (>= v0.16.0))
  (bisect_ppx
   (>= 2.8.3))))
