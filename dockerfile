FROM ubuntu:20.04

# Noninteractive Setting
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y dirmngr gnupg apt-transport-https ca-certificates software-properties-common 

#Setting for tzdata
RUN ln -fs /usr/share/zoneinfo/Asia/Seoul /etc/localtime && dpkg-reconfigure --frontend noninteractive tzdata

ENV LANG en_US.UTF-8

# statgarten dependencies
RUN apt-get install -y libssl-dev libcurl4-openssl-dev libxml2-dev cmake

# shinyserver dependencies
RUN apt-get install -y wget gdebi
RUN apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# install R

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'

RUN apt-get install -y r-base

# for install_github
RUN R -e "install.packages('remotes')"

# statgarten dependencies - plotGen
RUN R -e "install.packages(c('dplyr', 'httr', 'rvest', 'xml2', 'tidyverse', 'plotly'))"

RUN R -e "install.packages(c('factoextra', 'rstanarm'))"
# statgarten dependencies - board
#RUN R -e "install.packages(c(''))"

# statgarten dependencies - board
#RUN R -e "install.packages(c(''))"

# statgarten dependencies - scissor
#RUN R -e "install.packages(c(''))"

# statgarten dependencies - goophi
#RUN R -e "install.packages(c(''))"


RUN R -e "remotes::install_github('statgarten/plotGen')"
RUN R -e "remotes::install_github('statgarten/board')"
RUN R -e "remotes::install_github('statgarten/scissor')"
RUN R -e "remotes::install_github('statgarten/goophi')"
RUN R -e "remotes::install_github('statgarten/door')"

RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-18.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

COPY ini.sh /etc/ini.sh
COPY ShinyApps/app.R /srv/shiny-server/app.R

EXPOSE 3838
