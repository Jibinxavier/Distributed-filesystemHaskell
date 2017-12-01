
FROM python:3.6-alpine

# python and pip

RUN apk update && apk upgrade && \
    apk add --no-cache bash git openssh

COPY requirements.txt  /requirements.txt

RUN pip install --no-cache-dir -r /requirements.txt 

 
ADD . /