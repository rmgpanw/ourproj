FROM rocker/verse:4.2.0

# Install remotes R package
RUN R -e 'install.packages("remotes")'

# Install ourproj
WORKDIR /ourproj
COPY . .
RUN Rscript -e 'args <- remotes::local_package_deps(dependencies = TRUE); writeLines(remotes::system_requirements("ubuntu", "20.04", package = args))' | \
  while read -r cmd; do \
  echo $cmd && eval sudo $cmd; \
  done
RUN Rscript -e 'remotes::install_local(".", dependencies = TRUE)'

# Install additional R packages
RUN R -e 'remotes::install_cran(c("nanonext", "devtools", "tidyverse"))'
