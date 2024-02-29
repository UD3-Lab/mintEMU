# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.3.0

# required
MAINTAINER Claudiu Forgaci <C.Forgaci@tudelft.nl>

COPY . /mintEMU

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  # build this compendium package
  && R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" \
  && R -e "remotes::install_github(c('rstudio/renv', 'quarto-dev/quarto-r'))" \
  # install pkgs we need
  && R -e "list.files()" \
  && R -e "renv::restore()" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main qmd file
  && R -e "quarto::quarto_render('/mintEMU/analysis/paper/paper.qmd')"
