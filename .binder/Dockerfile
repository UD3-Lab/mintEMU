FROM rocker/binder:4.2.0
LABEL maintainer= 'Claudiu Forgaci <C.Forgaci@tudelft.nl>'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

RUN wget https://github.com/UD3-Lab/minTEMU/raw/main/DESCRIPTION && R --vanilla -s -e "options(repos = list(CRAN = 'http://cran.rstudio.com'));  install.packages('renv'); renv::restore()"

RUN rm DESCRIPTION.1; exit 0
