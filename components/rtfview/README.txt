TRTFView component

License.

    This component is released under Modified LGPL license (the same as
    the Lazarus LCL license) for details see the included COPYING.LGPL and 
    COPYING.modifiedLGPL files.

Notes.

    This component uses the rtf parser provided with fpc, versions previous
    to fpc 2.1.1 revision 6507 had some bugs that might affect th appareance
    of some rtf documents. In order to use this component with earlier versions
    of fpc compiler, a patched version of the rtf parser is included in file 
    rtfparspre211.pp.

    If you want to use the most current version of rtf parser, remove the 
    -dUsePre211RTFPars compiler option located at package's 
    "Compiler options->Other->Custom Options".

    This option is to be removed after the release of FPC 2.2.

Install.

    this component requires TRichView to be installed first.
    see http://wiki.lazarus.freepascal.org/RichView

    In lazarus choose: Components->Open Package File (*.lpk)
    and look for file rtfviewpkg.lpk
    
    press compile button if all went ok
    press Install button.
    
    After succesful install a new icon should appear in RichView Tab in 
    Lazarus Component Palette.
    

Use.

    See the sample directory to for an example.



    