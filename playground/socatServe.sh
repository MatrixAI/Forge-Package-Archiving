#!/bin/bash 
socat -d -d -v TCP-LISTEN:80,crlf,reuseaddr,fork SYSTEM:"
	echo HTTP/1.1 200 OK;
       	echo Content-Type\: text/plain;
	echo Content-Length\: 6000000;
       	echo;
       	curl 'http://mirror.internode.on.net/pub/test/5meg.test1'"
	# sleep 3s && kill %1 && exit 1;
