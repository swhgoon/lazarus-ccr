{   This file is part of the Web Service Toolkit
    Copyright (c) 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_date_utils;

interface
uses
  SysUtils,
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFrameWork,
{$ENDIF}
  date_utils;

type

  TTest_DateUtils = class(TTestCase)
  published
    procedure xsd_TryStrToDate_date_only();
    procedure xsd_TryStrToDate_date_time();
    procedure xsd_TryStrToDate_date_bad_separator();
    procedure xsd_TryStrToDate_date_time_bad_separator();
    procedure xsd_TryStrToDate_date_time_timezone_z();
    procedure xsd_TryStrToDate_date_time_timezone_zero();
    procedure xsd_TryStrToDate_date_time_timezone_1();
    procedure xsd_TryStrToDate_date_time_timezone_2();

    procedure xsd_DateTimeToStr_1();
    procedure xsd_DateTimeToStr_2();
    procedure xsd_DateTimeToStr_timezone_1();
  end;

implementation

{ TTest_DateUtils }

procedure TTest_DateUtils.xsd_DateTimeToStr_1();
const
  sDATE_1 = '1976-10-12T23:34:56Z';
  sDATE_2 = '0987-06-12T20:34:56Z';
var
  d : TDateTimeRec;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  FillChar(d,SizeOf(d),#0);
  d.Date := EncodeDate(1976,10,12) + EncodeTime(23,34,56,0);
  CheckEquals(sDATE_1, xsd_DateTimeToStr(d));

  FillChar(d,SizeOf(d),#0);
  d.Date := EncodeDate(987,06,12) - EncodeTime(20,34,56,0);
  CheckEquals(sDATE_2, xsd_DateTimeToStr(d));
end;

procedure TTest_DateUtils.xsd_DateTimeToStr_2();
const
  sDATE_1 = '1976-10-12T23:34:56Z';
  sDATE_2 = '0987-06-12T20:34:56Z';
var
  d : TDateTime;
begin
  d := EncodeDate(1976,10,12) + EncodeTime(23,34,56,0);
  CheckEquals(sDATE_1, xsd_DateTimeToStr(d));

  d := EncodeDate(987,06,12) - EncodeTime(20,34,56,0);
  CheckEquals(sDATE_2, xsd_DateTimeToStr(d));
end;

procedure TTest_DateUtils.xsd_DateTimeToStr_timezone_1();
  //2002-10-10T12:00:00+05:00 is 2002-10-10T07:00:00Z
var
  d : TDateTimeRec;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  FillChar(d,SizeOf(d),#0);
  d.Date := EncodeDate(2002,10,10) + EncodeTime(12,0,0,0);
  d.HourOffset := 5;
  CheckEquals('2002-10-10T07:00:00Z', xsd_DateTimeToStr(d));
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_bad_separator();
const
  DATE_STR = '1976;10;12';
var
  d : TDateTimeRec;
begin
  CheckEquals(False,xsd_TryStrToDate(DATE_STR,d),Format('"%s" is not a valid date.',[DATE_STR]));
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_only();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12';
  d := xsd_StrToDate(s);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,0,'Hour');
      CheckEquals(mn,0,'Minute');
      CheckEquals(ss,0,'Second');
    CheckEquals(0,d.HourOffset,'HourOffset');
    CheckEquals(0,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56';
  d := xsd_StrToDate(s);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(0,d.HourOffset,'HourOffset');
    CheckEquals(0,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time_bad_separator();
const
  DATE_STR = '1976-10-12T23/34:56';
var
  d : TDateTimeRec;
begin
  CheckEquals(False,xsd_TryStrToDate(DATE_STR,d),Format('"%s" is not a valid date.',[DATE_STR]));
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time_timezone_1();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56+12:34';
  d := xsd_StrToDate(s);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(12,d.HourOffset,'HourOffset');
    CheckEquals(34,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time_timezone_2();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56-01:23';
  d := xsd_StrToDate(s);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(-1,d.HourOffset,'HourOffset');
    CheckEquals(-23,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time_timezone_z();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56Z';
  d := xsd_StrToDate(s);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(0,d.HourOffset,'HourOffset');
    CheckEquals(0,d.MinuteOffset,'MinuteOffset');
end;

procedure TTest_DateUtils.xsd_TryStrToDate_date_time_timezone_zero();
var
  s : string;
  d : TDateTimeRec;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56+00:00';
  d := xsd_StrToDate(s);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(0,d.HourOffset,'HourOffset');
    CheckEquals(0,d.MinuteOffset,'MinuteOffset');

  s := '1976-10-12T23:34:56-00:00';
  d := xsd_StrToDate(s);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(0,d.HourOffset,'HourOffset');
    CheckEquals(0,d.MinuteOffset,'MinuteOffset');
end;

initialization
  RegisterTest('Support',TTest_DateUtils.Suite);

end.
