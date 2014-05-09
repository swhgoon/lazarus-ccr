{$DEFINE HAS_QWORD}
{$DEFINE HAS_COMP}

unit simple_record_test;
interface

type
  TTestSmallRecord = record
    fieldSmallint : Smallint;
    fieldWord : Word;
    fieldString : string;
  end;
  
  TTestRecord = record
    fieldByte : Byte;
    fieldShortInt : ShortInt;
    fieldSmallint : Smallint;
    fieldWord : Word;
    fieldInteget : Integer;
    fieldLongWord : LongWord;
    fieldInt64 : Int64;
  {$IFDEF HAS_QWORD}
    fieldQWord : QWord;
  {$ENDIF}
  {$IFDEF HAS_COMP}
    fieldComp : Comp;
  {$ENDIF}
    fieldSingle : Single;
    fieldDouble : Double;
    fieldExtended : Extended;
    fieldCurrency : Currency;
    fieldBoolean : Boolean;
    fieldString : string;
    fieldRecord : TTestSmallRecord;
  end;

implementation

end. 