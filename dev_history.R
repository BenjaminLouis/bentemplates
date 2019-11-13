usethis::use_build_ignore("dev_history.R")

# Licence
usethis::use_mit_license("Benjamin Louis")

# Need a inst/rmarkdown/templates directory
usethis::use_directory(("inst/rmarkdown/templates"))

# Quotes
usethis::use_directory(("inst/rmarkdown/templates/quotes"))
usethis::use_directory(("inst/rmarkdown/templates/quotes/skeleton"))


# Dependencies
usethis::use_pipe()
attachment::att_to_description()
