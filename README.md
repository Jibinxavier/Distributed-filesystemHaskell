# Distributed-filesystem CS7NS1
This system is constructed using REST services, written in Haskell using the Servant library.

## Usage
Current configuration has two clients. The start.sh accepts parameter that will determine number of fileservers.
``` bash
./start.sh # start all the services with 1 primary  and 2 secondary fileservers
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

There are many possible point of failures, example being where the servers not responding with ready to commit signal. With current implementation the transaction stalls. A better solution would take into account a timeout before canceling the transaction

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

### Example
![](./distributed_filesystem.png/?raw=true "Filesystem")
#### Registration
0.0. As the system starts up the fileserver register themselves to the directory service. Heart beats are sent periodically
#### Login in
1.0 Client signs up first

1.1 Client encrpts its message using the authentication servers public key and sends credential.

1.2 Authentication server returns a token 
#### Write and read
2.1 The client locks the file before writing to it. If locked it stays in queue and waits for the lock

2.1.1 Client will have the file metadata from the directory service


2.2 After acquiring the lock. It writes the file and update the information in directory server

2.3 It unlocks the file

2.4 Filserver distributes the copy

3.1-3.2 For reads, it checks if the local copy is uptodate by checking against the metadata from from the directory server

3.3 Downloads the file if it is outdated

#### Tranasaction 
4.1-4.2 Calls the Transaction server to initiate a transaction and it returns a transaction id

4.3 File content is written to the transaction

4.4 Locks the file. If it is already locked it joins the queue

4.5 - 4.6 Client calls commit which triggers transaction details to be pushed to the fileservers' shadow copy. Fileservers send back ready to commit signal, if all servers respond TS instructs the filservers to update their copies.