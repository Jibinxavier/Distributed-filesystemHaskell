# Distributed-filesystem
This system is constructed using REST services, written in Haskell using the Servant library.

## Usage
Current configuration has two clients. The start.sh accepts parameter that will determine number fileserver.
``` bash
./start.sh # start all the services with 1 primary fileserver and 2 secondary
cd client
docker-compose run --service-ports client1 # start client 1
docker-compose run --service-ports client2 # start client 2
```
### Distributed Transparent File Access
The system is modeled based on upload/download.Downloads only occur if files are modified and writes are pushed upstream. Users interact with system through the client service. Authenticated users can write to files, and these writes go to a primary copy, which in turn distributes copies of the files to replica nodes.
  
### Client Service
Act as a proxy between user interface and filesystem. Client will allow users to read file content and write to it. The write encapsulates both opening the file and writing to it. As a result at no point the client will keep the file open.  Files are locked before writes are propagated 

### Security Service

1. Client asks for the public key, (ideally the client would already know). It uses this key to encrypt its first interactions with authentication server.
2. A user signs up with a user_name and password
3. It then authenticates itself by calling "log in", which in turn returns a message encrypted with client pass. 

    3.1 The message contains a ticket. The ticket contains the generated key and an expiry date which is encrypted with system wide shared secret. encCLIENT_PASS (generated session key ,encSHARED_SCERET(generated session key, expiry date)) 
4. The client uses this token to communicate with other services
### Lock service
1. Authenticates user
2. Locks file if available and returns a tuple (inqueue, lockavailable)
3. If it is already locked, the user is added to the queue
4. When a user unlocks, the server assigns the lock to the user in the queue if there is one, and notifies it.

However, the whole system is susceptible deadlocks, as files could be left locked and other clients waiting for the lock. 
### Directory service 
The directory service is the most important components. It has a full view of the status of files, this includes: where files are stored, information about primary and secondary fileservers, health of fileserver, and primary server election.  Unfortunately, this means that directory service is a single point of failure and would have to handle lot of traffic.

##### File accesses
 When a client does a filesearch, the directory service returns metadata of the file. It also includes address of a replica fileserver. This is because reads go to a replica fileserver.
 
 Writes on the otherhand go to the primary fileserver, which in turn pushes the changes to the replica. It retrieves the information about the replicas from the directory service

##### Managing fileservers
Fileservers when they start up, register themselves with the directory server (DS). Periodically they send a heart beat to the DS. If heartbeats do not reach the DS within a threshold it considers the filserver is dead. If it is a primary copy the DS will randomly pick a server from the replicas as the primary copy.
##### Handling transactions
Similar to the fileserver the directory server also has its own shadow copy, which is merged at a successful commit.
### Replication service
As described above reads go to the secondary copies and writes to the primary copy.
### Caching
When a client introduces an new file to the system, the directory server creates metadata for the file. It includes timestamp of when it was updated. During reads, the client checks with the directory service if the local copy is dated, if so it will retrieve the latest from the fileserver.
### Fileserver service
 The directory will point client to primary server for writes and reads to the secondary ones.
1. Stores the files
2. Starts by registering itself then it regularly sends heartbeats to the directory service. 
3. Each fileserver represent a directory. The directory name is passed in an environment variable
4. Asynchronously sends the copy (if primary copy)

### Transaction Service
1. To initiate a transaction the client makes a call to the Transaction Server(TS), which returns a transaction id.
2.  Client uses this id and sends file changes. TS locks the files and stores the changes.
3. If the file is locked, transaction waits till it is unlocked. 
4. Once the client executes commit, the file changes are transfered to the fileserver's shadow copy.
5. Upon receiving the shadow copy, fileservers will send a ready to commit signal
6. If all the servers send ready to commit. Transaction server instructs servers to update their true copy.

### Commands
`signup user userspassword` Signs up a user

`login user userspassword` Returns a token used for communicating with other services

`readfile f_1/testfile user`  Displays file contents. Requires: "remote dir/fname (filepath)" "username" 
`write f_1/testfile user hello` Writes to a file. Requires:"remote dir/fname (filepath)" "username" "content to add"

`startTrans user` Starts transaction. Requires:  "username"

`writeT f_1/testfile user hello2` Write to transaction. Requires:"remote dir/fname (filepath)" "username" "content to add"

`commit user ` Completes transaction. Requires "username"

`abort user` Aborts transaction. Requires "username"

`lockfile f_1/testfile user ` Locks file. Requires "remote dir/filename"   "username"

`unlockfile f_1/testfile user ` Unlocks file. Requires "remote dir/filename"   "username"

`listdirs user`Lists all available remote directories. Requires:  "username"

`lsdircontents user f1`Lists contents of a remote directories. Requires: "remote dir/filename" "username"


![](./distributed_filesystem.png/?raw=true "Filesystem")