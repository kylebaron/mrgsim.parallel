library(spelling)

path <- c(
  "inst/docs/about.md",
  "README.Rmd"
)

message("checking package")
spell_check_package()
ignore <- readLines("inst/WORDLIST")
message("checking other files")
spell_check_files(path, ignore)
