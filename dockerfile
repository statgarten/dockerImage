FROM dao0312/statgarten:latest_core

# Install TeX
RUN apt-get update
RUN apt-get install -y texlive-full

# Edit Permission
RUN chown shiny.shiny -R /usr/local/lib/R/site-library/door

EXPOSE 3838

#CMD ["bash"]
CMD ["/usr/bin/shiny-server"]

