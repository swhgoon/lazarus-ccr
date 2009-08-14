BeepFp: BEEP network protocol framework component for Free Pascal
Version 1
1 September 2009

Copyright (C) 2009, Wimpie Nortje <wimpienortje@gmail.com>

Introduction
------------
BEEP is a network application framework protocol. BEEP is not a complete protocol, it only provides the building blocks common to most network protocols to ease custom protocol design and speed up implementation. To understand exactly what BEEP is, see www.beepcore.org and RFC3080.

BeepFp is a Free Pascal componentset to allow development of custom protocols using BEEP in native Object Pascal.

BeepFp consists of a number of parts:
* libaxl        - shared library providing XML processing
* libvortex     - shared library providing the BEEP implementation
* axl.pas       - Pascal translation of the libaxl .h files
* vortex.pas    - Pascal translation of the libvortex .h files 
* beepXXXXXX.pas- Object Pascal BEEP implementation built on top of libvortex and libaxl.
* tests         - To test the basic functionality of the shared libraries
* examples      - Usage examples of the Object Pascal classes.

LibAXL (another XML library) is an XML library implemented in C. To learn more about LibAXL see www.aspl.es/axl

LibVortex is a BEEP (RFC3080/RFC3081) implementation in C. To learn more about LibVortex see www.aspl.es/vortex

Features
--------
* Event driven communication
* Blocking and non-blocking modes
* Requires only a few event handlers to implement full-blown network protocol

Installing
----------
BeepFp depends on a working libaxl and libvortex installation. Installation instructions are included in both downloads. The included test programs must be run to confirm a proper installation.

BeepFp does not need any installation. Simply add the beepfp package to the project and include the necessary units.

Obtaining the libraries
-----------------------
LibAxl can be downloaded from www.aspl.es/axl.
LibVortex can be downloaded from www.aspl.es/vortex. NB SEE NOTE

NOTE: BeepFp requires at least vortex svn r4012, ie LibVortex release 1.1.1 from 2009/04/27 is too old. Release 1.1.2 is not yet available at the time of writing.

Using BeepFp
------------
Vortex is a multithreaded library. In unix this requires that the cthread unit be the first thread in the main program, even if the pascal program doesn't use any thread.

The events fired by the BeepFp classes are triggered by events from the C library. These all happen in different threads, which implies that the pascal event handlers also run in these secondary threads. This means that the event handlers cannot access the GUI for the same reason that TThread objects must use Synchronize to acces the GUI. The difference here is that there is not Synchronize function available to these threads. Get data from the event handlers onto the GUI remains a problem to be solved.

There are a number of options available to use BEEP in an application.
1. The easiest is to use TBeepServer and TBeepClient components like in the example programs BEEP_Listener and BEEP_Client.

2. If these two classes do not provide the required functionality, one can use the lower set of classes TBeepConnection, TBeepChannel etc to build a client and server.

3. The lowest level (and hardest work) is to use the C library directy via Vortex.pas. This provides all the functionality as the C library.

Documentation
-------------
At the moment BeepFp doesn't include its own documentation.

Vortex is thoroughly documented. This should be sufficient to answer most questions.

Work completed
--------------
All basic functionality have been completed. 
This includes:
* a server that can accept multiple connections, initiate channels and channel pools.
* a client that can make a single connection with multiple channels and multiple channel pools.
* easy creation of required event handlers for implementing the profiles (protocol specifics)

To do
-----
* SASL (authentication)
* TLS (encryption)
* Advanced profile features, ie 2nd layer handling or profile events
* XML-RPC profile support
* Tunneling profile support
* HTTP CONNECT profile support

Portability
-----------
Axl and Vortex are supported on at least Windows and Linux. Probably some other OSes too.

The Axl and Vortex translations have only been tested on Linux. There may be a few small changes required to get it to work on windows and the other platforms supported by Vortex.

Getting the library's threads (created in C) to work reliably with Free Pascal was quite complex. Porting Vortex.pas to other platforms would most likely require at least a review of this part.

BeepFp uses only plain, system independant object pascal. That said, it is only tested on Linux so far. With all portability issues solved for Axl and Vortex, BeepFp may or may not work out of the box on other platforms.

Credits and license
-------------------
Thanks to Advanced Software Production Line (ASPL) for making Vortex and Axl libraries available, and for their support.

Axl library, Vortex library, Axl unit and Vortex unit are all covered by the LGPL license.

BeepFp is covered by a modified LGPL license.
