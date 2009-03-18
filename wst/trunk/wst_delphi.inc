{$IFDEF DELPHI}
  type
  {$IFDEF CompilerVersion}
    {$IF ( CompilerVersion > 16.0 )}
      QWord = UInt64;
    {$IFEND}
  {$ENDIF CompilerVersion}
  {$IF Not Declared(QWord) }
    QWord = type Int64;
  {$IFEND
  }  
    DWORD = LongWord;
    PtrInt = Integer;
    PByteArray = ^ByteArray;
    ByteArray = array[0..$effffff] of Byte;
    PtrUInt = Cardinal;
    SizeInt = Longint;
    UnicodeChar = WideChar;
{$ENDIF DELPHI}
