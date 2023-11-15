FROM dao0312/statgarten:latest_core

# Install TeX
RUN apt-get update
RUN apt-get install -y texlive-full libssl-dev openssl bzip2 libfontconfig

# Edit Permission
RUN chown shiny.shiny -R /usr/local/lib/R/site-library/door

RUN R -e "install.packages('webshot')"
USER shiny
RUN R -e 'webshot::install_phantomjs()'

ENV OPENSSL_CONF /dev/null
EXPOSE 3838

#CMD ["bash"]
CMD ["/usr/bin/shiny-server"]

