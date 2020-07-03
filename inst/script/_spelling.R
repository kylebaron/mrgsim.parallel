library(spelling)

path <- c(
  "inst/docs/about.md",
  "README.Rmd"
)

spell_check_package()
ignore <- readLines("inst/WORDLIST")
spell_check_files(path,ignore)
