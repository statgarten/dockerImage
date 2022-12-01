FROM ubuntu:20.04

# Noninteractive Setting
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y dirmngr gnupg apt-transport-https ca-certificates software-properties-common locales

# Set Locale
RUN locale-gen en_US.UTF-8
RUN update-locale LANG=en_US.UTF-8

#Setting for tzdata
RUN ln -fs /usr/share/zoneinfo/Asia/Seoul /etc/localtime && dpkg-reconfigure --frontend noninteractive tzdata

# statgarten dependencies
RUN apt-get install -y libssl-dev libcurl4-openssl-dev libxml2-dev libfreetype6-dev libgdal-dev cmake 

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

RUN apt install -y r-base-core 
RUN apt install -y r-recommended
RUN apt install -y r-base

# for install_github
RUN R -e "install.packages('remotes')"

# statgarten dependencies - door
RUN R -e "install.packages(c('dplyr', 'httr', 'rvest', 'xml2', 'tidyverse', 'plotly', 'leaflet', 'ggparty', 'caret', 'reactable', 'ggpie', 'naniar'))"
RUN R -e "remotes::install_github('vqv/ggbiplot')"

# statgarten dependencies - colorpen
RUN R -e "install.packages('phosphoricons')"

# statgarten dependencies - board
RUN R -e "install.packages(c('shinydashboardPlus'))"

# statgarten dependencies - colorpen
RUN R -e "install.packages(c('shiny.i18n'))"

# statgarten dependencies - soroban
RUN R -e "install.packages(c('showtext'))"

# statgarten depencencies - scissor
RUN R -e "install.packages(c('RSQLite', 'sortable', 'writexl'))"

# statgarten dependencies - goophi
RUN R -e "install.packages(c('factoextra', 'rstanarm'))"

RUN R -e "remotes::install_github('statgarten/datatoys')"
RUN R -e "remotes::install_github('statgarten/board')"
RUN R -e "remotes::install_github('statgarten/stove')"
RUN R -e "remotes::install_github('statgarten/colorpen')"
RUN R -e "remotes::install_github('statgarten/soroban')"
RUN R -e "remotes::install_github('statgarten/scissor')"
RUN R -e "remotes::install_github('statgarten/door')"

RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-18.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

COPY group /etc/group
COPY passwd /etc/passwd
COPY sudoers /etc/sudoers
RUN chmod 644 /etc/group
RUN chmod 644 /etc/passwd
RUN chmod 644 /etc/sudoers

# install TeX
RUN apt-get install -y texlive-base texlive-latex-base texlive-latex-extra
USER shiny
RUN tlmgr init-usertree
RUN tlmgr option repository http://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2019/tlnet-final
RUN tlmgr install pdftexcmds --verify-repo=none
RUN tlmgr install infwarerr --verify-repo=none
RUN tlmgr install pdfescape --verify-repo=none
RUN tlmgr install letltxmacro --verify-repo=none
USER root
RUN R -e "install.packages('tinytex')"
RUN R -e "tinytex::install_tinytex(force = TRUE)"

#COPY ini.sh /etc/ini.sh
COPY ShinyApps/app.R /srv/shiny-server/app.R
RUN rm -r /srv/shiny-server/index.html /srv/shiny-server/sample-apps
RUN chmod 777 -R /tmp
WORKDIR /var/log/shiny-server
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
