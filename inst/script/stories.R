stopifnot(require("dplyr"))
stopifnot(require("testthat"))
stopifnot(require("yaml"))
stopifnot(require("knitr"))
stopifnot(require("readr"))

stopifnot(file.exists("inst/docs/tests.csv"))

test <- read_csv("inst/docs/tests.csv", show_col_types=FALSE)
stories <- yaml.load_file("inst/docs/stories.yaml")

story <- Map(stories, names(stories), f = function(story, storylabel) {
  tibble(
    STID = storylabel,
    STORY = story$summary,
    test = story$tests
  )
})

story <- bind_rows(story)

all <- left_join(story, test, by = "test")

if(any(is.na(all$failed))) {
  warning("some NA found")  
}

write_csv(all, "inst/docs/stories-tests.csv")

x <- kable(all, format = "markdown")

writeLines(x, con = "inst/docs/stories.md")
