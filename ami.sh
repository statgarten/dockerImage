sudo apt-get update && sudo apt-get install -y git

# Install R
sudo apt-get install -y dirmngr gnupg apt-transport-https ca-certificates software-properties-common
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
wget -O- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo gpg --dearmor | sudo tee /usr/share/keyrings/cran.gpg
echo deb [signed-by=/usr/share/keyrings/cran.gpg] https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/ | sudo tee /etc/apt/sources.list.d/cran.list
sudo apt-get update
sudo apt-get install -y r-base=4.2.3-1.2204.0

# Give Permission to R library path
sudo chmod o+w /usr/local/lib/R/site-library

# Install Statgarten Linux Dependencies
sudo apt-get install -y cmake
sudo apt-get install -y libxml2-dev libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev libpng-dev libtiff5-dev libjpeg-dev gdal-bin libgdal-dev
sudo apt-get install -y libcurl4-openssl-dev libssl-dev # May not necessary?

# Install Statgarten
R -e 'install.packages("remotes")'
#R -e 'install.packages("terra")'
R -e 'remotes::install_github("vqv/ggbiplot")'
#R -e 'install.packages("sortable")' # Dependencies for Scissor
R -e 'remotes::install_github("statgarten/statgarten")'

# Running Statgarten
#sudo R -e "door::run_app(options=list(port=80))"

# Install RStudio-server
sudo su - \ -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
sudo apt-get install -y gdebi-core
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.20.1002-amd64.deb
sudo gdebi -n shiny-server-1.5.20.1002-amd64.deb
rm shiny-server-1.5.20.1002-amd64.deb

# Edit Shiny-server.conf
sudo sed -i 's/run_as shiny/run_as ubuntu/g' /etc/shiny-server/shiny-server.conf
sudo sed -i 's/listen 3838/listen 80/g' /etc/shiny-server/shiny-server.conf

sudo rm -r /srv/shiny-server/*
sudo echo "library(door)" > /srv/shiny-server/app.R
sudo echo "door::run_app()" >> /srv/shiny-server/app.R

sudo /usr/bin/shiny-server