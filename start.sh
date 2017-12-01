
#!/bin/bash
# Starting up services  

docker build -t distr-filesystem-base .

docker pull fpco/stack-build:lts-7.12
docker pull fpco/stack-run  
docker pull mongo:3.0.2
# transactionservice directoryservice fileserver
services=( auth-server lockService  )
echo "Starting services"
# creating containers and starting them
for serv in ${services[@]}
do  
    
    echo $serv  
    cd $serv
    stack clean
    stack image container      # creating an image 
    nohup docker-compose up &  #putting it to background
    cd ..
done

cd client
docker-compose up