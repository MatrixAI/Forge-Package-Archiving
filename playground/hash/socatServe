#!/bin/bash 
socat TCP-LISTEN:80,crlf,reuseaddr,fork SYSTEM:"echo HTTP/1.1 200 OK; echo Content-Type\: text/plain;
 echo; cat ./downloadFile"

