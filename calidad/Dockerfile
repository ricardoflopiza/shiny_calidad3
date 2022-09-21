FROM openanalytics/r-base
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssh2-1 \
    libssl1.1 \
    build-essential \
    cmake \
    git \
    libbamtools-dev \
    libboost-dev \
    libboost-iostreams-dev \
    libboost-log-dev \
    libboost-system-dev \
    libboost-test-dev \
    libxml2-dev \
    libmpfr-dev \
    libz-dev \
    curl \
    libarmadillo-dev \
    && rm -rf /var/lib/apt/lists/*
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('devtools'),repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('shinyFeedback',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('shiny',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('haven',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('labelled',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('dplyr',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('openxlsx',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('sjmisc',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('readxl',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('survey',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('feather',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('shinyWidgets',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('rlang',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('kableExtra',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('shinycssloaders',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('readr',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('shinybusy',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('shinyalert',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('writexl',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('shinyjs',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('tibble',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('plotly',repos='http://cran.rstudio.com/')"

RUN R -e "devtools::install_github('https://github.com/inesscc/calidad')"

RUN mkdir /root/calidad
COPY R /root/calidad
#RUN R -e "devtools::install_github('https://github.com/ricardoflopiza/shiny_calidad_package')"
COPY Rprofile.site /usr/lib/R/etc/
EXPOSE 8080
CMD ["R","-e", "shiny::runApp('/root/calidad')"]
