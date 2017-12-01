import os
import sys 
import json
from flask import Flask
from flask import jsonify
from flask import request
from pymongo import MongoClient 
from flask import Flask
from flask_pymongo import PyMongo
 
from helper import decrypt_msg, SHARED_SCERET
app = Flask(__name__)
locked_files = {}
user_details = {}
@app.route('/file/lock', methods=['POST']) 
def lock_file():
    
    data = request.get_json(force=True)  
    req, _ = decrypt_msg(data.get('msg'), data.get('token'))
    

    if req is None:
        return   jsonify({'success': False,'inqueue': False, "message": "Authentication failed."})

    user_n = req.get("user_n")
    fpath = req.get("fpath")
    
    if( fpath in locked_files):
        locked = locked_files[fpath]["status"]
        if locked:
            locked_files[fpath]["queue"]= locked_files[fpath]["queue"] +[user_n]

            # store user details for callback
            user_details[user_n] = request.remote_addr
            return jsonify({'success': True, 'inqueue': True, "message": "inqueue"})
 
    # if there is no file or the queue is empty
    locked_files[fpath]["status"] = True 
    locked_files[fpath]["queue"] = []
      
    return jsonify({'success': True, 'inqueue': False, "message": "locked successfully"})
    
       

@app.route('/file/unlock', methods=['POST']) 
def unlock():
    data = request.get_json(force=True)  
    req, _ = decrypt_msg(data.get('msg'), data.get('token')) 
    if req is None:
        return   jsonify({'success': False, "message": "Authentication failed."})
    
    user_n = req.get("user_n")
    fpath = req.get("fpath")
    
    if( fpath in locked_files):
        locked = locked_files[fpath]["status"]
        if locked:
            if len(locked_files[fpath]["queue"]) != 0:
                callback_user = locked_files[fpath]["queue"].pop()
                notify_user(callback_user,fpath) # assign lock to someone else
                 
            else: # otherwise just unlock
                locked_files[fpath]["status"] = False

 
            return jsonify({'success': True, "message": "file unlocked"})
        
    return jsonify({'success': False, "message": "no file"})
def notify_user(user_n,fpath): 
    user_addr = user_details[user_n] 
    data = {'success': True, "message": "file unlocked","fpath":fpath}
    url = "http://{}/file/lockavailable".format(user_addr)
    
    send_post_msg(url, data)


if __name__ == "__main__":
    app.run(host='0.0.0.0', port=8001)