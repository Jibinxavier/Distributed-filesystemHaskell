#Base image
FROM python:2.7-alpine

# python and pip
 
COPY requirements.txt  /requirements.txt
COPY helper.py  /helper.py
RUN pip install --no-cache-dir -r /requirements.txt 

