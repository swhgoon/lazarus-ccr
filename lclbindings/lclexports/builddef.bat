PATH=c:\Programas\Dev-Cpp\bin\
pexports.exe lclexports.dll > liblclexports.def
dlltool.exe --def liblclexports.def --dllname lclexports.dll --output-lib liblclexports.a
