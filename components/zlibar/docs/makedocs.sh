#!/bin/sh
# change these as needed
BASE_XCT_DIR=/home/andrew/programming/help/

RTL_XCT=$BASE_XCT_DIR"rtl.xct"
FCL_XCT=$BASE_XCT_DIR"fcl.xct"

CSS=/home/andrew/programming/lazarus-svn/docs/html/fpdoc.css
TITLE="ZLib Archive Component"

fpdoc --package=zlibar 			\
--format=chm 				\
--descr=zlibar.xml 			\
--hide-protected			\
--output=zlibar.chm 			\
--input=../zlibar.pas 			\
--auto-toc 				\
--auto-index 				\
--make-searchable 			\
--import=$RTL_XCT,ms-its:rtl.chm::/ 	\
--import=$FCL_XCT,ms-its:fcl.chm::/ 	\
--css-file=$CSS 			\
--chm-title="$TITLE"
