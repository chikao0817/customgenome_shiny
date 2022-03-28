# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:4.1.2

# system libraries of general use
RUN apt-get update --allow-releaseinfo-change && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
	cmake \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2 \
    libgvc6 \
    libxml2-dev \
    libsodium-dev \
	libglpk-dev \
	libnlopt-dev \
	libbz2-dev \
	libclang-dev \
	liblzma-dev \
	gfortran 

# install Tools


# install R packages required 

RUN R -e 'install.packages("remotes")'

RUN R -e 'remotes::install_cran("BiocManager")'
RUN R -e 'remotes::install_cran("latticeExtra")'
RUN R -e 'remotes::install_cran("devtools")'

## Bioconductor
RUN R -e 'BiocManager::install(version = "3.14")'
RUN R -e 'BiocManager::install("Rhtslib",version = "3.14")'
RUN R -e 'BiocManager::install("karyoploteR",version = "3.14")'
RUN R -e 'BiocManager::install("GenomicAlignments",version = "3.14")'
RUN R -e 'BiocManager::install("VariantAnnotation",version = "3.14")'
RUN R -e 'BiocManager::install("Gviz",version = "3.14")'

## DEVTools
RUN R -e 'remotes::install_github("RinteRface/bs4Dash@v2.0.3")'
RUN R -e 'remotes::install_github("glin/reactable")'
RUN R -e 'remotes::install_github("paul-shannon/igvShiny")'

## CRAN

### cacultate and base package
RUN R -e 'remotes::install_version("magrittr",version = "2.0.1")'
RUN R -e 'remotes::install_version("plyr",version = "1.8.6")'
RUN R -e 'remotes::install_version("dplyr",version = "1.0.7")'
RUN R -e 'remotes::install_version("dtplyr",version = "1.2.1")'
RUN R -e 'remotes::install_version("tidyr",version = "1.2.0")'
RUN R -e 'remotes::install_version("stringr",version = "1.4.0")'
RUN R -e 'remotes::install_version("tibble",version = "3.1.6")'
RUN R -e 'remotes::install_version("cyphr",version = "1.1.2")'
RUN R -e 'remotes::install_version("qs",version = "0.25.2")'
RUN R -e 'remotes::install_version("data.table",version = "1.14.2")'
RUN R -e 'remotes::install_version("lubridate",version = "1.8.0")'

### shiny package
RUN R -e 'remotes::install_version("shiny",version = "1.7.1")'
RUN R -e 'remotes::install_version("colourpicker",version = "1.1.1")'
RUN R -e 'remotes::install_version("waiter",version = "0.2.5")'
RUN R -e 'remotes::install_version("shinyWidgets",version = "0.6.4")'
RUN R -e 'remotes::install_version("shinycssloaders",version = "1.0.0")'
RUN R -e 'remotes::install_version("shinyjqui",version = "0.4.1")'
RUN R -e 'remotes::install_version("shinyalert",version = "3.0.0")'
RUN R -e 'remotes::install_version("shinylogs",version = "0.2.0")'
RUN R -e 'remotes::install_version("shinyjs",version = "2.1.0")'
RUN R -e 'remotes::install_version("shinybusy",version = "0.2.2")'
RUN R -e 'remotes::install_version("sortable",version = "0.4.5")'
RUN R -e 'remotes::install_version("shinyFeedback",version = "0.4.0")'
RUN R -e 'remotes::install_version("fresh",version = "0.2.0")'
RUN R -e 'remotes::install_version("cicerone",version = "1.0.4")'

### plot and table package
RUN R -e 'remotes::install_version("RColorBrewer",version = "1.1.2")'
RUN R -e 'remotes::install_version("gplots",version = "3.1.1")'
RUN R -e 'remotes::install_version("ggplot2",version = "3.3.5")'
RUN R -e 'remotes::install_version("plotly",version = "4.10.0")'
RUN R -e 'remotes::install_version("ggrepel",version = "0.9.1")'
RUN R -e 'remotes::install_version("cowplot",version = "1.1.1")'
RUN R -e 'remotes::install_version("echarts4r",version = "0.4.3")'
RUN R -e 'remotes::install_version("showtext",version = "0.9.5")'
RUN R -e 'remotes::install_version("DT",version = "0.18")'
RUN R -e 'remotes::install_version("rhandsontable",version = "0.3.8")'
RUN R -e 'remotes::install_version("openxlsx",version = "4.2.5")'
RUN R -e 'remotes::install_version("wordcloud2",version = "0.2.1")'

### BSgenome packages
# copy the tar.gz to image
COPY ./BSgenome_tar/BSgenome.Osativa.ENSEMBLPLANTS.IRGSP1.0_1.0.tar.gz  /srv/BSgenome.Osativa.ENSEMBLPLANTS.IRGSP1.0_1.0.tar.gz
RUN R -e 'install.packages("/srv/BSgenome.Osativa.ENSEMBLPLANTS.IRGSP1.0_1.0.tar.gz", repos = NULL)'

COPY ./BSgenome_tar/BSgenome.Slycopersicum.SolGenomicsNetwork.SL3.0_1.0.tar.gz  /srv/BSgenome.Slycopersicum.SolGenomicsNetwork.SL3.0_1.0.tar.gz
RUN R -e 'install.packages("/srv/BSgenome.Slycopersicum.SolGenomicsNetwork.SL3.0_1.0.tar.gz", repos = NULL)'



# timezone setting
RUN apt-get update \
    &&  DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends tzdata
    
RUN TZ=Asia/Taipei \
    && ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
    && echo $TZ > /etc/timezone \
    && dpkg-reconfigure -f noninteractive tzdata

# copy the app to the image
COPY ./igvShiny_TestApp /srv/shiny-server/igvShiny_TestApp 

# copy R change local port 3838
COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server


# select port

EXPOSE 3838

# run app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/igvShiny_TestApp', host='0.0.0.0', port=3838)"]
