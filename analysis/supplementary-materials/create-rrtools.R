usethis::use_mit_license(copyright_holder = "Claudiu Forgaci")
rrtools::use_readme_rmd()
rrtools::use_analysis()
renv::init()
# rrtools::use_dockerfile()
rrtools::add_dependencies_to_description()
devtools::document()

# allows using magrittr pipe in the pacakge:
usethis::use_pipe()
