
#!/bin/bash
# Starting up services  
# exporting environment variables so that they are available 
# when doing docker compose

export $(cat ./all_env.env | xargs)

DEFAULT_NUM_FS=3

docker build -t distr-filesystem-base .

docker pull fpco/stack-build:lts-7.12
docker pull fpco/stack-run 
docker pull mongo:3.0.2
if [ -z "$1" ]
  then
   
    python create_fileservers.py $DEFAULT_NUM_FS
else
    python create_fileservers.py $1
fi

# transactionservice 
services=( auth-server lockService  directoryservice fileserver)
echo "Starting services"
# creating containers and starting them
for serv in ${services[@]}
do  
    
    echo $serv  
    cd $serv
    stack clean
    stack build
    stack image container      # creating an image 
    nohup docker-compose up >log.out 2>&1 &  #putting it to background
    cd ..
done

cd client
nohup docker-compose up &