0.7.0 (2016-07-17):
* remove dependency on ctypes
* support named pipes on Win32
* report Win32 errors
* client: don't clunk the fid after remove
* client: deallocate_fid should clunk before markind fid as free
* client: remove: always mark the fid as free
* client: don't clunk a fid if walk fails
* server: fix deadlock on Eof which prevents connection cleanup
* use the channel module for buffered packet reading
* unix: don't allocate per request, use a per-connection 32KiB buffer

0.6.0 (2016-04-10):
* server: supply no exception converter by default
* dependency on lambda-term (for the shell) is now optional
* add support for Win32
* use logs library
* reject messages longer than 640 KiB

0.5.0 (2016-03-01):
* remove an unnecessary copy in the read path
* add documentation for Server_unix_9p
* add a `shell` subcommand for the CLI with history and line-editing
* server: allow per-connection state
* unix: transform Unix.EPIPE into `Eof
* when pretty-printing, don't print payloads
* add wstat/update to client API
* lofs: sequential read/write increased by 9-10x
* server: handle errors in the dispatcher loop
* client: allow clients to handle I/O errors and recover

0.4.0 (2016-01-25):
* Remove OASIS from build system
* Remove use of -pack, now use the index module Protocol_9p with aliases
* Expose previously hidden Response.sizeof_header
* Expose previously hidden Response.Read.sizeof_header
* Add Request.sizeof_header
* Add Request.Write.sizeof_header

0.3 (2016-01-20):
* Add version/attach mount debug messages
* Pass initial connection attach to receive callback handler

0.2 (2016-01-04):
* Respect negotiated msize in read
* Add LICENSE file

0.1 (2015-12-13):
* Initial version
