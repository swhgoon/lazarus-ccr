/*
system.h

C/C++ header for the LCL Exports library

LICENSE: The same modifyed LGPL as the Free Pascal Runtime Library
         and the Lazarus Component Library
         
Copyright (C) 2008 Felipe Monteiro de Carvalho
*/

/* Basic Pascal declarations */

#ifndef LAZARUS_SYSTEM_H
#define LAZARUS_SYSTEM_H

#ifdef __cplusplus
extern "C" {
#endif

/* Cross-platform entry-point */

#ifdef __Win32__ 

#define APPBEGIN() \
 int WINAPI WinMain (HINSTANCE hThisInstance, \
                    HINSTANCE hPrevInstance, \
                    LPSTR lpszArgument, \
                    int nFunsterStil)

#else 

#define APPBEGIN() \
int main(int argc, char *argv[]) 

#endif

typedef void* TObjectH;

/* 32-bit types */

typedef int Integer;
typedef unsigned int Cardinal;

/* Variable width types */

typedef int PtrInt;
typedef unsigned int PtrUInt;

#ifdef __cplusplus
}
#endif

#endif	/* !LAZARUS_SYSTEM_H */
