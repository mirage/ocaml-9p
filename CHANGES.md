## v2.0.2 (2021-01-16)

* Update to Cstruct.length in cstruct.6.0.0 and use Fmt.str
  rather than Fmt.strf (@djs55)

## v2.0.1 (2019-03-26)

* Use cstruct-sexp introduced in cstruct.4.0.0 (@avsm)

## v2.0.0 (2019-03-10)

* Remove old `KV_RO` interface in the client as it has
  been superseded by mirage-kv-2.0.  The new interface
  will be re-added in a future release (@avsm)

## v1.0.1 (2019-02-07)

* Use modern `io-page-unix` instead of `io-page.unix` (@avsm)

## v1.0.0 (2019-02-03)

* Upgrade metadata to opam 2.0 format (@avsm)
* Port to dune (@avsm)
* Fix warnings for unused variables and opens and record binds (@avsm)

## v0.12.1 (2018-06-14)

* Remove (broken) support for named pipes.
* Remove unnecessary dependency on `cmdliner` in the core library.

## v0.12.0 (2017-11-05)

* Remove unnecessary dependency on `ppx_deriving`

## v0.11.3 (2017-09-15)

* Remove unnecessary Unix dependency from core library

## v0.11.2 (2016-06-17)
* Add dependency on io-page-unix
* Modernise travis and appveyor

## v0.11.1 (2017-06-07)
* Protect Flow_lwt_unix.write against End_of_file exceptions
* Add topkg-jbuilder support

## v0.11.0 (2017-05-07)

* protocol-9p-unix: add missing dependency on io-page.unix
* protocol-9p-unix: add optional periodic ping thread to keep connections alive
* protocol-9p-unix: add prometheus metrics integration

## v0.10.0 (2017-04-28)

* Update to lwt.3.0.0
* Switch build to `jbuilder`
* Split into 3 opam packages: protocol-9p, protocol-9p-unix, protocol-9p-tool
* The Unix modules are inside the `Protocol_9p_unix` module

## v0.9.0 (2017-02-14)

* Update to Mirage 3 APIs (114, @avsm and @samoht)

## v0.8.0 (2016-12-12)

* Add a `max_fids` optional argument to `connect` to set the maximal number
  of fids a client can open in parallel (the default remains 100).
  (#108. @samoht)

## v0.7.4 (2016-11-10)

* Protect flow reads from EPIPE exceptions (this can happens on Windows)
  (#104, @samoht)
* Log a message when FID pool is exhausted (#98, @talex5)
* Close socket if we get an error trying to connect (#97, @talex5)

## v0.7.3 (2016-07-18)

* remove the remaining uses of stringext (in the CLI example)

## v0.7.2 (2016-07-15)

* fix more dependency issues in the META file

## v0.7.1 (2016-07-15)

* switch to topkg
* make protocol-9p.unix depends on io-page.unix to workaround an issue
  the channel implementation
* remove the dependency to stringext
* fix missing dependencies causing a compilation issue on 4.03

## v0.7.0 (2016-07-12)

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

## v0.6.0 (2016-04-10)

* server: supply no exception converter by default
* dependency on lambda-term (for the shell) is now optional
* add support for Win32
* use logs library
* reject messages longer than 640 KiB

## v0.5.0 (2016-03-01)

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

## v0.4.0 (2016-01-25)

* Remove OASIS from build system
* Remove use of -pack, now use the index module Protocol_9p with aliases
* Expose previously hidden Response.sizeof_header
* Expose previously hidden Response.Read.sizeof_header
* Add Request.sizeof_header
* Add Request.Write.sizeof_header

## v0.3 (2016-01-20)

* Add version/attach mount debug messages
* Pass initial connection attach to receive callback handler

## v0.2 (2016-01-04)

* Respect negotiated msize in read
* Add LICENSE file

## v0.1 (2015-12-13)

* Initial version
