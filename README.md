# dragons
This is a Common Lisp DNS client. You can use it to query a DNS server for information, typically to resolve hostnames into IP addresses.

## 1. Introduction
DNS is the standard system for mapping IP addresses to hostnames. It can also be used for other purposes, such as SRV records which can be used by clients to locate services on the network.

## 2. Usage
The main function is `QUERY` which accepts a question or list of questions to send to the DNS.
It returns `(values answers authorities additionals questions)`. 

The ANSWERS, AUTHORITIES and ADDITIONALS are represented as  resource record structures (Lisp name RR).

### 2.1 Resource records
These are how the DNS returns information from the query. 
```
(defstruct rr 
  name          ;; string naming the resource
  type          ;; symbol naming the type of resource
  class         ;; symbol naming the class of resource, always :IN (internet)
  ttl           ;; time to live, number of seconds the record should be considered valid
  rdata)        ;; variable data. Type of data depends on TYPE slot.
```

#### 2.1.1 Resource type RDATA 

* :A (address) a octet vector storing the IP address
* :CNAME (canonical name) a string naming an alias for the name in the record
* :SRV (service record) a plist with fields :PRIORITY :PORT :WEIGHT :TARGET

For all other types the RDATA slot contains an opaque octet vector as returned by the DNS response. 

### 2.2 Queries

Query your DNS using `QUERY` function, it accepts a one or more questions to be answered.

```
CL-USER> (dragons:query (dragons:question "google.com"))
```

### 2.3 Inverse queries

Use `IQUERY` function with one or more answers. The DNS will return the questions which result with those answers.
Not all DNS servers support inverse queries.

```
CL-USER> (dragons:iquery (dragons:answer #(216 258 10 100)))
```

### 2.4 Get host by name

Equivalents to gethostbyname and gethostbyaddr are `get-host-by-name` and `get-host-by-addr`:

```
;; Resolve a hostname to an internet address
CL-USER> (dns:get-host-by-name "mymachine")
(#S(FSOCKET:SOCKADDR-IN :ADDR #(10 1 2 10) :PORT 0))

;; attempt to resolve an internet address into a hostname 
CL-USER> (dns:get-host-by-addr (car *))
("mymachine")

```

### 2.5 Configuration

* The default is to contact the DNS server using UDP. You can choose to use TCP instead by providing :TCP as
the protocol parameter to the `QUERY` function.

* The default address of the DNS server is retrieved by calling `FSOCKET:GET-NAME-SERVERS`. You can choose
a different address by either providing one as the `ADDR` parameter to `QUERY` or by setting `*DNS-ADDR*`.

* The default database path is to the current home directory, but this can be changed by modifying `*DATABASE-PATH*
before calling any other functions. The database is opened on the first call.

### 2.6 Example

```
CL-USER> (query (list (question "google.com")))
(#S(DRAGONS::RR
    :NAME "google.com"
    :TYPE :A
    :CLASS :IN
    :TTL 119
    :RDATA #(216 58 209 238)))
NIL
NIL
((:NAME "google.com" :TYPE :A :CLASS :IN))

;; a SRV record request
CL-USER> (dragons:query (dragons:question "_http._tcp.example.com" :srv))
NIL
(#S(DRAGONS::RR
    :NAME "example.com"
    :TYPE :NS
    :CLASS :IN
    :TTL 61239
    :RDATA "b.iana-servers.net")
 #S(DRAGONS::RR
    :NAME "example.com"
    :TYPE :NS
    :CLASS :IN
    :TTL 61239
    :RDATA "a.iana-servers.net"))
(#S(DRAGONS::RR
    :NAME "b.iana-servers.net"
    :TYPE :A
    :CLASS :IN
    :TTL 61239
    :RDATA #(199 43 133 53))
 #S(DRAGONS::RR
    :NAME "a.iana-servers.net"
    :TYPE :A
    :CLASS :IN
    :TTL 61239
    :RDATA #(199 43 132 53)))
((:NAME "_http._tcp.example.com" :TYPE :SRV :CLASS :IN))

;; kerberos KDC discovery
CL-USER> (dragons:query (dragons:question "_kerberos._tcp.my.domain.com" :srv))
(#S(DRAGONS::RR
    :NAME "_kerberos._tcp.my.domain.com"
    :TYPE :SRV
    :CLASS :IN
    :TTL 600
    :RDATA (:PRIORITY 0 :WEIGHT 100 :PORT 88 :TARGET
            "myDC.my.domain.com")))
NIL
(#S(DRAGONS::RR
    :NAME "myDC.my.domain.com"
    :TYPE :A
    :CLASS :IN
    :TTL 1200
    :RDATA #(10 1 1 47)))
((:NAME "_kerberos._tcp.my.domain.com" :TYPE :SRV :CLASS :IN))
```

## 3. Errors
If the DNS server returns an error status, a `DNS-ERROR` will be signalled. If UDP is used and no response
is received within `TIMEOUT` seconds, an error indicating a timeout has occured is signalled.

## 4. Database
To reduce the number of queries that must be made over the network, a persistent cache is used
to store records. This file is created by the file named by `*DATABASE-PATH*` its first usage.
By default this is named by `(merge-pathnames "dragons.dat" (user-homedir-pathname))`, you should change this
before calling any functions to change its location. 

### 4.1 Inserting/deleting records
The result of any DNS query is automatically inserted into the database, it will be deleted once its TTL has 
expired. You may insert permanant entries:
```
CL-USER> (dns:insert-record "foo.com" "bar.com" :cname)
```

You may delete an entry
```
CL-USER> (dns:remove-record "foo.com" "bar.com" :cname)
```

## 5. TODO
- [x] Support SRV queries.
- [ ] Decode all the flags properly.
- [x] Some sort of cache, possibly persistent.
- [ ] Long term: write a DNS server.

## 6. License
Licensed under the terms of the MIT License.

Frank James 
May 2015.
