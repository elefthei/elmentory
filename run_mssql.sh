#!/bin/bash
sudo docker run -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=myPassw0rd' \
   -p 1433:1433 --name sql1 \
   -d microsoft/mssql-server-linux

