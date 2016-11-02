When handling a high number of requests, we need a task/ download management system that will lock the database, queue tasks in the proper order,
and also create a writer and reader model of actors that can obtain a lock on a particular file that is being served to the client.

We can detail some of the proposed conditions on our reader and writer agents, and also some details of how the queueing system will work.

(Writer Lock):
-> A writer lock is obtained when there are no pending readers for the file that the client is trying to lock in the task queue.
-> When a writer lock is obtained and subsequently performed, the writer will be responsible for:
    -> hashing the file that is being downloaded in a seperate thread/conduit
    -> in the same hashing conduit, writing that file to disk on the ipfs cache
    -> updating references inside the IPNS/multi-keyed database/locking queue manager
    -> deleting the file if it does not match the hash requested by the user/there are "trusted agents" saying that the file is not the file requested (as can be determined by the information that the client has supplied and by the headers supplied in the request)
    -> in a seperate conduit, transparently provide the file to the client as if the client were downloading the file from the original source
    -> write trailing headers detailing whether the file was successfully written to the database
    -> persist the connection until the hash of the file can be confirmed and written into the trailing headers
    -> releasing the lock on the reference in the database once it is done with the file. 

-> A reader lock is obtained when there is already a writer lock on the requested file in the database. A reader will:
    -> not hash the file from the database. 
    -> TBC...
