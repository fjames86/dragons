# dragons
Common Lisp DNS client.

## 1. Introduction
DNS is the standard system for mapping IP addresses to hostnames.

## 2. Usage
The only function is `QUERY` which accepts a question or list of questions to send to the DNS.
It returns `(values answers authorities additionals questions)`. 

The ANSWERS, AUTHORITIES and ADDITIONALS are represented as  resource record structures (Lisp name RR).

```
CL-USER> (query (list (question "google.com")) :host "10.1.100.100")
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
```

## 3. License
Licensed under the terms of the MIT License.

Frank James 
May 2015.
