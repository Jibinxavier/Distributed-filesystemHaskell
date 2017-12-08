#!/bin/bash

if [ "$1" == "all" ]; then
  echo "Removing dangling images and containers"
  docker rmi $(docker images -f dangling=true -q)
  docker volume rm $(docker volume ls -f dangling=true -q)
fi
docker rm -f $(docker ps -a -q)