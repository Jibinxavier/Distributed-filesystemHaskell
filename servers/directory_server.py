import random
import json
import time
import string
import os
import sys 
from flask import Flask
from flask import jsonify
from flask import request
from pymongo import MongoClient 
from flask import Flask
from flask_pymongo import PyMongo

from helper import decrypt_msg, SHARED_SECRET, decrypt_token,encrypt_msg

# The reads go secondary copies. Writes go to primary copy
#
#
#
# db =  mongo_conn("directory_db_1:27017")
# db.directory.drop()
# db.files.drop()
app = Flask(__name__)
@app.route('/dir/listdirs', methods=['GET']) 
def listdirs():
    """
        returns: 
        directory_name
        primary_copy_address
        files

         dir_name  :: String
                      , primary :: Maybe FServer
                      ,  fservers    :: [FServer]
    """
    token = request.headers.get('token')
    client_key = decrypt_token(token)
    if client_key:
        content = list(db.directory.find({}))  
        return jsonify(encrypt_msg(content, client_key))
    else:
        return jsonify({"result": ""})
 
    
@app.route('/dir/listdircontents', methods=['GET']) 
def listdircontents():
    
    token = request.headers.get('token')
    client_key = decrypt_token(token)
    dir_name = request.headers.get('dir_name')
    if client_key:

        content = list(db.directory.find({"dir_name": dir_name}))  
        return jsonify(encrypt_msg(content, client_key))
    else:
        return jsonify({"result": ""})
    
@app.route('/dir/filesearch', methods=['POST']) 
def filesearch():
    """
        path dir fileid ip port timestamp
        ip, port of replica
    """
    decrypt_msg(content, token)
    data = request.get_json(force=True)  
    req , client_key= decrypt_msg(data.get('msg'), data.get('token'))
    ######################
    pass

@app.route('/dir/updateUploadInfo', methods=['POST']) 
def updateUploadInfo():
    """
        updates uploaded files information
    """
    data = request.get_json(force=True)  
    req , client_key= decrypt_msg(data.get('msg'), data.get('token'))
    if req:
    dir_name = req['dir_name']
    pass
@app.route('/dir/add_dir', methods=['POST']) 
def add_dir():
    data = request.get_json(force=True)  
    req , client_key= decrypt_msg(data.get('msg'), data.get('token'))
    pass
@app.route('/dir/uploadToShadowDir', methods=['POST']) 
def uploadToShadowDir():
    data = request.get_json(force=True)  
    req , client_key= decrypt_msg(data.get('msg'), data.get('token'))
    pass

@app.route('/dir/commitDirChanges', methods=['POST']) 
def commitDirChanges():
    pass
@app.route('/dir/abortDirChanges', methods=['POST']) 
def abortDirChanges():
    pass
@app.route('/dir/heartbeat', methods=['POST']) 
def heartbeat():
    pass

@app.route('/dir/getAllReplicas', methods=['POST']) 
def heartbeat():
    pass