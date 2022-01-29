library(dplyr)
library(testthat)
library(yaml)

stopifnot(file.exists("inst/docs/tests.csv"))

test <- read_csv("inst/docs/tests.csv", show_col_types=FALSE)
stories <- yaml.load_file("inst/docs/stories.yaml")

story <- Map(stories, names(stories), f = function(story, storylabel) {
  tibble(
    st = storylabel,
    summary = story$summary,
    test = story$tests
  )
})

story <- bind_rows(story)

all <- left_join(story, test, by = "test")

na <- filter(all, is.na(failed))
unique(na$test)
