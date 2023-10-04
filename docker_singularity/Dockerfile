FROM rocker/verse:4.2.3

LABEL org.opencontainers.image.authors="nkhoshnevis@g.harvard.edu"

# Linux internal commands
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		vim \
		zsh \
		&& apt-get clean

# R package installation from CRAN
RUN R -e "install.packages(c( \
    'covr', \
    'roxygen2', \
    'devtools'), repos='https://cloud.r-project.org')"

# R package installation from CRAN
RUN R -e "install.packages(c( \
    'SuperLearner', \
    'data.table', \
    'xgboost', \
    'earth', \
    'ranger', \
    'gam', \
    'KernSmooth', \
    'MASS', \
    'polycor', \
    'wCorr', \
    'Ecume', \
    'logger', \
    'gnm'), repos='https://cloud.r-project.org')"

ENV RENV_VERSION 0.17.3
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

# R package installation from GitHub
RUN R -e "remotes::install_github('NSAPH-Software/locpol', reference='master')"
