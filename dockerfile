FROM dao0312/statgarten:latest_core

# Install TeX
RUN apt-get install -y texlive-full
RUN tlmgr install setspace

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]