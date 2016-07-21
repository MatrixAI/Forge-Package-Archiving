#Overview
IPFS is a p2p filesystem designed as a replacement to HTTP; it aims to provide permalinks everywhere, using "content addressable block storage" and "content addressable hyperlinks". 

The underlying data structure that is part of the IPFS is called the Merkle DAG (Directed Acyclic Graph, also called a Merkle Tree, or hash tree). The idea behind this data structure is that every non leaf node of the tree is a hash of the hashes of it's children. An example diagram of this:
![Binary Hash Tree Diagram](https://upload.wikimedia.org/wikipedia/commons/thumb/9/95/Hash_Tree.svg/1024px-Hash_Tree.svg.png)

The other underlying parts of IPFS include a distributed hash table, "incentivised block exchange" and "self-certifying namespace".

The underlying idea behind IPFS is to __model all data__ using the Merkle DAG.

Git is a good example of distributed filesystem design, also utilising a content addressable Merkle DAG. __IPFS seeks to explore how such designs could be integrated as part of the web itself.__

Here are some of the keypoints of what makes IPFS different:
* websites can be completely distributed
* websites don't have an origin server
* websites don't have any servers to talk to


So you have some content that you want to access: you only know the hash for that content, and perhaps some semantic name for human access. The goal of IPFS is, a request to the network consists of:
* Does anybody have this content hash?
* Node that has corresponding content hash returns content to the user.

Content addressing occurs at the HTTP layer. Every file is guaranteed unique by it's content hash unless it is the same file (in which case it has the same content hash).

IPFS paths consist of a root object, and then a path inside the root object. You don't talk to servers, everything starts with your root object.

#Finding and retrieving files
1. Identify the file with content addressing
2. Find the nodes who have content with your content hash
3. Download content (blocks)

#What is an IPFS object?
IPFS is fundamentally a mechanism for retrieving containers called __IPFS objects__. These consist of:
* A blob of data (less than 256 kB)
* An array of links to other IPFS objects; a link is:
  * a name (the name of the link)
  * the hash of the IPFS object that the link points to
  * the cumulative size of the linked IPFS objects, following its links.
