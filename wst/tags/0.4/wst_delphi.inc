{$IFNDEF HAS_QWORD}
  type
    QWord = type Int64;
    DWORD = LongWord;
    PtrInt = Cardinal;
    PByteArray = ^ByteArray;
    ByteArray = array[0..$effffff] of Byte;
{$ENDIF}