from pymongo import MongoClient
from bson import json_util
client = MongoClient("localhost",27222)
print(client.database_names())
db = client.USEHASKELLDB

cursor = db.Directory_RECORD.find({})
for document in cursor:
        print(document)
print("filservers")
cursor = db.DirHealth_RECORD.find({})
for document in cursor:
        print(document)
cursor = db.Files_RECORD.find({})
for document in cursor:
        print(document)
        


# client = MongoClient("localhost",27890)
# print(client.database_names())
# db = client.USEHASKELLDB

# cursor = db.LockService_RECORD.find({})
# for document in cursor:
#         print(document)

# print("client record")
 
allports = {"authserver": 27000, "client":27017,"directoryservice":27222 ,"lock": 27890,"fileserver_0":27090, "fileserver_1": 27091, "fileserver_2":27092, "transactionservice":27984}
for x in allports:      
        print(x)
        client = MongoClient("localhost",allports[x])

        
        db = client.USEHASKELLDB
        print(db.collection_names())
        print("\n \n")

print("\n checking client db")
client = MongoClient("localhost",27890) 

db = client.USEHASKELLDB


cursor = db.LockService_RECORD.find({})
for document in cursor:
        print(document)
 



client = MongoClient("localhost",27017)


db = client.USEHASKELLDB
print(db.collection_names())
cursor = db.LockAvailability_RECORD.find({})
for document in cursor:
        print(document)
 
        
# cursor = db.jobs.find({"completed": False,})
# print("\n \n \n")
# for document in cursor:
#         print(document)
# print(db.jobs.find({}).count())
# print("left to do {}". format(list(db.jobs.find({"completed": False,}))))
# print("left to do {}". format(db.jobs.find({"completed": False,}).count()))

# res = db.jobs.aggregate([ 
#     { "$group": { "_id": {},"max": { "$max": "$assigned_time" },"min": { "$min": "$assigned_time" } 
#     }}
# ])
# print(list(res)) 