library(spelling)

path <- c(
  "inst/doc/about.md",
  "README.Rmd"
)




spell_check_package()
ignore <- readLines("inst/WORDLIST")
spell_check_files(path,ignore)
