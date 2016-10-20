# Forge-Package-Archiving

Content addressable storage system for packages.

Issues:
https://github.com/wrengr/stm-chans/issues/1

Development Notes:
Upstream Failure Test Cases:

1. No Content-Length Header and No Chunked Encoding - and sends stream - in this case, we just have to accept it, and compare against hash - is already working
2. Content-Length Header - sends stream that is less than the content length
3. Content-Length Header - sends stream that is more than the content length
4. Chunked Encoding - malformed chunked encoding
^^ USE pathod for testing

We need to consider how requests that happen in parallel will hit the cache while the first request is still downloading the cache. This needs to be done by using a queue of readers and writers that have locks, where these locks are stored in the multi keyed database. We will need to consider using a locking manager such as etcd, zookeeper or software transactional memory.

We might also need to consider the actor model in implementation.
See the following resources:

https://en.wikipedia.org/wiki/Distributed_lock_manager

https://github.com/dmbarbour/haskell-vcache
 GitHub
dmbarbour/haskell-vcache
haskell-vcache - large, persistent, memcached values and structure sharing for Haskell 
 
 

zookeeper vs etcd

https://github.com/jepst/distributed-process-global
 GitHub
jepst/distributed-process-global
distributed-process-global - Distributed locks, RPC, and global registration for Cloud Haskell 
 
 

http://book.realworldhaskell.org/read/software-transactional-memory.html

http://haskell-distributed.github.io/

http://adit.io/posts/2013-05-15-Locks,-Actors,-And-STM-In-Pictures.html
adit.io
Locks, Actors, And STM In Pictures - adit.io
Aditya Bhargava's personal blog. 

http://stackoverflow.com/questions/17721243/haskell-actor-based-mutability
 stackoverflow.com
Haskell - Actor based mutability
I'm working on a haskell network application and I use the actor pattern to manage multithreading. One thing I came across is how to store for example a set of client sockets/handles. Which of course 
 

https://mnot.github.io/I-D/httpbis-retry/
mnot.github.io
Retrying HTTP Requests
HTTP allows requests to be automatically retried under certain circumstances. This draft explores how this is implemented, requirements for similar functionality from other parts of the stack, and potential future improvements. 
