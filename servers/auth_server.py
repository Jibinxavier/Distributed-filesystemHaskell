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
import threading 
import requests
from Crypto.Cipher import AES
from Crypto.Cipher import PKCS1_OAEP

import base64
import hashlib 
from helper import generate_key_pair,mongo_conn,DateTimeEncoder,decrypt_msg, SHARED_SECRET
import datetime
from AESCipher import AESCipher

# db =  mongo_conn("authserver_db_1:27017")
# db.users.drop()

"""Need generate auth server public and private keys
"""
 

app = Flask(__name__)
@app.route('/client/signup', methods=['POST']) 
def register_user():
    db = mongo_conn()
    data = request.get_json(force=True)
    # encrypts with auth server's public key
    data = decrypt(data.get('msg'))
    user_n = data.get('user_n')
    
    # client send a hash of password
    if db.users.find({"user_n":user_n}).count() ==0:
        user_pass = data.get('pass')
        client_pub_key = data.get('client_pub_key')
        privatekey = get_private_key()
        cipher = PKCS1_OAEP.new(privatekey) 
        pass_wrd = cipher.decrypt(user_pass)
        
        db.users.insert(
            {"user_n": user_n,
            "pass":pass_wrd,
            "gen_session_key": "",
            "client_pub_key":client_pub_key
            }
        )
        return jsonify({'success': True, "message": "signed up successfully"})
    else: 
        return jsonify({'success': True, "message": "already signed up"})

@app.route('/client/login', methods=['POST']) 
def login():
    db = mongo_conn()
    data = request.get_json(force=True)
     # encrypts with auth server's public key
    data = decrypt(data.get('msg'))
    user_n = data.get('user_n')
    
    # client send a hash of password
    user = db.users.find_one({"user_n":user_n})
    if len(user) ==0: 
        return jsonify({'success': False, "message": "User not found sign up"})
    else: 
        user_pass = data.get('pass')
        privatekey = get_private_key()
        cipher = PKCS1_OAEP.new(privatekey) 
        pass_wrd = cipher.decrypt(user_pass)
        if user ["pass"] == pass_wrd:
            gen_session_key = ''.join(
             random.SystemRandom().choice(string.ascii_uppercase + string.digits) for _ in range(16))
            user['gen_session_key'] = gen_session_key
            db.users.update({'user_n': user_n}, user, upsert=True) 
            token = generate_token(user)  
            return jsonify({'success': True, 'ticket': token})
        else:
            return jsonify({'success': False, "message": "Invalid password"})

def generate_token(user):

    """
        token contains: (generated session key, expiry date) encrypted with system wide shared secret
        ticket contains: ((generated session key), (generated session key, expiry date)) encrypted 
            with clients public key
    """
    
    expiry_date =(datetime.datetime.now() +  datetime.timedelta(minutes = 10)).timestamp()
    
    token = {"gen_session_key":" ", "expiry_date":expiry_date}
    token_serialised = json.dumps(token,cls=DateTimeEncoder)
   
 
    
    cipher = AESCipher(SHARED_SECRET)
    encode_hash_session_key = cipher.encrypt(token_serialised )

    
    ticket = json.dumps({'gen_session_key': user['gen_session_key'] 
                            , 'token': encode_hash_session_key})


    # encrypting the whole ticket with client's password or public key
    cipher = PKCS1_OAEP.new(user['public_key'])
    encode_hash_ticket = cipher.encrypt( str.encode(ticket)) 
    return encode_hash_ticket
def decrypt(msg):
    privatekey = load_server_privkey("./private.key")
    cipher = PKCS1_OAEP.new(privatekey)

    msg = cipher.decrypt(msg)
    return json.loads(msg)
def decrypt_token(ticket, privatekey):

    """
        token contains: (generated session key, expiry date) encrypted with 
        ticket contains: 
    """
    privkey_path = "./client.key"
    pubkey_path = "./client_pub.key" 
 

    # encrypting the whole ticket with client's password or public key
     
    cipher = PKCS1_OAEP.new(privatekey)

    ticket = cipher.decrypt(ticket)

    ticket = json.loads(ticket)
    print(ticket)

    from AESCipher import AESCipher
    cipher = AESCipher(SHARED_SECRET)
    
    token =  cipher.decrypt( ticket['token'])
    token = json.loads(token)


    print(token, ticket)
    return 
# t = json.loads(json.dumps(datetime.datetime.now().timestamp() ,cls=DateTimeEncoder))
# m = datetime.datetime.fromtimestamp(t)
# print(type(m))
# if __name__ == "__main__":
#     from helper import generate_key_pair


#     privkey_path = "./client.key"
#     pubkey_path = "./client_pub.key"
#     priv_key,pub_key =  generate_key_pair(privkey_path,pubkey_path)
#     user=  {"gen_session_key":  ''.join(
#                 random.SystemRandom().choice(string.ascii_uppercase + string.digits) for _ in range(32))
                
#                 ,"public_key":pub_key }
#     t = generate_token(user)

#     decrypt_token(t, priv_key)

