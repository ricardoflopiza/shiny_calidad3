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
RUN R -e "install.packages('git2r', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('devtools', repos='http://cran.rstudio.com/')"

RUN R -e "install.packages('shinyFeedback', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('calidad', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('haven', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('labelled', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('openxlsx', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sjmisc', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('readxl', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('survey', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('feather', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rlang', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('kableExtra', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('readr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinybusy', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyalert', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('writexl', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tibble', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('pkgload', repos='http://cran.rstudio.com/')"

RUN mkdir -p /root/.ssh
ADD id_rsa /root/.ssh/id_rsa
RUN chmod 777 /root/.ssh/id_rsa
ADD id_rsa.pub /root/.ssh/id_rsa.pub
RUN chmod 777 /root/.ssh/id_rsa.pub
RUN R -e "devtools::install_git('git@git.ine.gob.cl:root/shiny-calidad-2.git', credentials = git2r::cred_ssh_key('/root/.ssh/id_rsa.pub', '/root/.ssh/id_rsa'))"
RUN mkdir /root/calidadv2
COPY R /root/calidadv2
COPY Rprofile.site /usr/lib/R/etc/
EXPOSE 8080
ENTRYPOINT ["Rscript "run_app.R"]
