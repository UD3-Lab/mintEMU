# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.3.0

# required
MAINTAINER Claudiu Forgaci <C.Forgaci@tudelft.nl>

COPY . /mintEMU

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # needed for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  # needed for the Rmpfr package
  && sudo apt-get install libmpfr-dev \
  # needed for the topicmodels package
  && sudo apt-get install -y libgsl-dev \
  # needed for the terra package
  && sudo apt-get install -y libgdal-dev \
  # build this compendium package
  && R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" \
  && R -e "remotes::install_github(c('rstudio/renv', 'quarto-dev/quarto-r'))" \
  # install pkgs we need
  && R -e "renv::restore(project = '/mintEMU/')" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main qmd file
  && R -e "quarto::quarto_render('/mintEMU/analysis/paper/paper.qmd')"
