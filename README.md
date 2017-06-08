wPOSIX
======


wPOSIX run only on Windows. This library requires Win32Ada to be
installed first.


Installation
------------

To configure, compile and install wPOSIX (requires Cygwin):

```
$ make setup
$ make
$ make install
```

This will install wPOSIX into the current GNAT compiler standard
location.

To configure, compile and install wPOSIX from Windows command shell:

```
c:> install c:\gnatpro\6.3.1
```


Usage
-----

To include wPOSIX into your application you just need to import
the wPOSIX project file.

```
with "wposix.gpr";
project Proj is
   ...
end Proj;
```
