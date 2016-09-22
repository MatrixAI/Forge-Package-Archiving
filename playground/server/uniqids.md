we can use flake to produce unique ids for the routing aspect of our web server

the idea is that we will have permalinks of the form 

packaage-archive.com/packages/ -> returns listing of packages
package-archive.com/package/UNIQ_ID -> permalink, where UNIQ_ID is an id consisting of timestamp+workerid+autoincrementingvariable
package-archive.com/package?uri=mypackage.tar.gz -> query strings on the package list for one package
