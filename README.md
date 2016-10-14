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


