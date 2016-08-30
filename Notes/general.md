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

In IPFS, every data blob that is greater than 256kB in size gets broken down into a parent IPFS object, which contains links to blobs of data that makeup the original data blob; it also contains some minimal data that says that the IPFS object represents a large file.

Furthermore, you can use the same kind of structure, to represent directories; the links contain names of files or other directories, and they themselves are represented as IPFS objects.

No matter where an IPFS object is referenced, as long as it has the same content, it also has the same content hash; this means that it doesn't matter where the file is stored or how it is logically organised in terms of it's directory structure.

so the ipfs command line tool can treet any of these objects as the root object, and then traverse the root object like any filesystem, no matter where the files behind the root object are actually stored. All files that have the same content hash are resolved to the same thing. THis is called __deduplication__.

#Concerns
IPFS breaks down all its content into blobs of data 256kB or less. When we hash the content, we will need IPFS to combine all the data, into a blob; we hash the package blob, and when we decompress it, we can perhaps share identitical components across packages. So for instance, we may not even need to hash the whole file 

##Seperate filesystems
The forge package archiving system should be able to support packages stored in multiple formats; for example, packages in tar.gz format, or packages that are obtained from a git repository in source format. Will the forge package archiving have support for building the package? 
##Semantic labelling of packages
Although, under the IPFS system, packages will be addressed by their content hash, similar to how nixOS addresses packages in its nix store, we will also want to label packages by version; i.e. we want to provide a mechanism for allowing developers to assign mutable names to specific packages. This means that users can address packages through their semantic name rather than their content hash. Probably the simplest way to do this is to keep a seperate database of package names/versions, and resolve them to their content hash in the system...

Furthermore, we will probably want to provide multiple index keys to our packages. So we can index by version number, latest names, etc; there should be no requirement that a package have only one key that can be used to address that package.

We will also want to look into the application of IPNS in particular; perhaps it can help with the requirement to address packages without having to write our own method of addressing.
##Deduplication (block deduplication)
##Reserve node issue (preserving packages)
It is necessary to ensure that people who participate in our IPFS package network are guaranteed that any package they upload will be stored permanently in the network. For this purpose, we suggest that there will be a reserve node that will serve to store all packages uploaded to the forge package archive IPFS; normal access will happen through traditional distributed method, but having a reserve node ensures that a package cannot disappear from the network. It is still to be determined whether the reserve node will crawl the IPFS network periodically to ensure that all packages added to the IPFS are still available, or whether the tracking of new packages will happen through the Forge package archiving tool when the package is uploaded to the network.

Further inspiration for preserving packages could come from FileCoin, a cryptocurrency based on file storage. The white paper on FileCoin mentions a "proof-of-retrievability" component, which could come in handy. Link [here](http://filecoin.io/filecoin.pdf)

##Removal of old packages
After a period of time, their should be a tool that maintains the IPFS network to remove old packages (or at least mark them as unsupported. At the most basic level, this could occur by removing these packages from the reserve node based on a specific list of criteria i.e. age, ....)
FFI from haskell/elixir

* Nested Deduplication at the package, file system level; advantages?

