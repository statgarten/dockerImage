#!/bin/bash

## Set defaults for environmental variables in case they are undefined
USER=${USER:=zarathu}
PASSWORD=${PASSWORD:=zarathu}
ROOT=${ROOT:=TRUE}
PERUSER=${PERUSER:=FALSE}

## USER ADD 
adduser ${USER} --gecos 'First Last,RoomNumber,WorkPhone,HomePhone' --disabled-password 
sh -c 'echo ${USER}:${PASSWORD} | sudo chpasswd' 

## Package library: /usr/local/lib/R/site-library
addgroup ${USER} staff

if [ "$ROOT" == "TRUE" ]; then
    usermod -aG sudo ${USER}
fi

## ShinyApps
cp -R /srv/shiny-server /home/${USER}/ShinyApps
chmod -R 777 /home/${USER}/ShinyApps

## Encoding
echo -e LANG=en_US.UTF-8 > /home/shiny/.Renviron
echo -e LANG=en_US.UTF-8 > /home/${USER}/.Renviron

## Permission
groupadd shiny-apps 
usermod -aG shiny-apps ${USER} 
usermod -aG shiny-apps shiny 
cd /home/${USER}/ShinyApps 
chown -R ${USER}:shiny-apps . 
chmod g+w . && \
chmod g+s . 