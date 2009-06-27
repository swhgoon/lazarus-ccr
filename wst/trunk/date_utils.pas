{   This file is part of the Web Service Toolkit
    Copyright (c) 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit date_utils;

interface
uses
  SysUtils;
  
type

  TDateTimeRec = packed record
    Date : TDateTime;
    HourOffset : Shortint;
    MinuteOffset : Shortint;
  end;

const
  ZERO_DATE : TDateTimeRec = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

  function xsd_TryStrToDate(const AStr : string; out ADate : TDateTimeRec) : Boolean;
  function xsd_StrToDate(const AStr : string) : TDateTimeRec;

  function xsd_DateTimeToStr(const ADate : TDateTimeRec) : string;overload;
  function xsd_DateTimeToStr(const ADate : TDateTime) : string;overload;

  function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64): TDateTime;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime;{$IFDEF USE_INLINE}inline;{$ENDIF}

  function NormalizeToUTC(const ADate : TDateTimeRec) : TDateTime;
  function DateEquals(const AA,AB: TDateTimeRec) : Boolean;

resourcestring
  SERR_InvalidDate = '"%s" is not a valid date.';

implementation

uses Math, DateUtils;

function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64): TDateTime;
begin
  Result := DateOf(AValue) + DateUtils.IncHour(TimeOf(AValue),ANumberOfHours);
end;

function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime;
begin
  Result := DateOf(AValue) + DateUtils.IncMinute(TimeOf(AValue),ANumberOfMinutes);
end;

function NormalizeToUTC(const ADate : TDateTimeRec) : TDateTime;
begin
  Result := ADate.Date;
  if ( ADate.HourOffset <> 0 ) then
    Result := IncHour(Result,-ADate.HourOffset);
  if ( ADate.MinuteOffset <> 0 ) then
    Result := IncMinute(Result,-ADate.MinuteOffset);
end;

{$HINTS OFF}
function DateEquals(const AA,AB: TDateTimeRec) : Boolean;
var
  e, a : TDateTime;
  e_y, e_m, e_d, e_h, e_mn, e_ss, e_ms : Word;
  a_y, a_m, a_d, a_h, a_mn, a_ss, a_ms : Word;
begin
  e := NormalizeToUTC(AA);
  a := NormalizeToUTC(AB);
  DecodeDateTime(e, e_y, e_m, e_d, e_h, e_mn, e_ss, e_ms);
  DecodeDateTime(a, a_y, a_m, a_d, a_h, a_mn, a_ss, a_ms);
  Result := ( e_y = a_y ) and ( e_m = a_m ) and ( e_d = a_d ) and
            (e_h = a_h ) and ( e_mn = a_mn ) and ( e_ss = a_ss ) and ( e_ms = a_ms );
end;
{$HINTS ON}

function xsd_TryStrToDate(const AStr : string; out ADate : TDateTimeRec) : Boolean;
const
  DATE_SEP_CHAR = '-'; TIME_MARKER_CHAR = 'T'; TIME_SEP_CHAR = ':';
var
  buffer : string;
  bufferPos, bufferLen : Integer;

  function ReadInt(out AValue : Integer; const ASeparatorAtEnd : Char) : Boolean;
  var
    locStartPos : Integer;
  begin
    while ( bufferPos <= bufferLen ) and ( buffer[bufferPos] < #33 ) do begin
      Inc(bufferPos);
    end;
    locStartPos := bufferPos;
    if ( bufferPos <= bufferLen ) and ( buffer[bufferPos] in ['-','+'] ) then
      Inc(bufferPos);
    while ( bufferPos <= bufferLen ) and ( buffer[bufferPos] in ['0'..'9'] ) do begin
      Inc(bufferPos);
    end;
    Result := ( bufferPos > locStartPos ) and
              ( ( ASeparatorAtEnd = #0 ) or
                ( ( bufferPos <= bufferLen ) and
                  ( buffer[bufferPos] = ASeparatorAtEnd )
                )
              );
    if Result then
      Result := TryStrToInt(Copy(buffer,locStartPos,(bufferPos-locStartPos)),AValue);
  end;

  function ReadMiliSeconds(out AValue : Integer; const ASeparatorAtEnd : Char) : Boolean;
  var
    locDigitCount, locRes, itemp, locErcode : Integer;
  begin
    while ( bufferPos <= bufferLen ) and ( buffer[bufferPos] < #33 ) do begin
      Inc(bufferPos);
    end;
    locRes := 0;
    locDigitCount := 0;
    while ( locDigitCount < 3 ) and ( bufferPos <= bufferLen ) and ( buffer[bufferPos] in ['0'..'9'] ) do begin
      Val(buffer[bufferPos],itemp,locErcode);
      locRes := ( locRes * 10 ) + itemp;
      Inc(bufferPos);
      Inc(locDigitCount);
    end;
    Result := ( locDigitCount > 0 );
    if Result then begin
      if ( locDigitCount < 3 ) and ( locRes > 0 ) then begin
        while ( locDigitCount < 3 ) do begin
          locRes := locRes * 10;
          Inc(locDigitCount);
        end;
      end;
      AValue := locRes;
    end;
  end;

var
  d, m, y : Integer;
  hh, mn, ss, ssss : Integer;
  tz_hh, tz_mn : Integer;
  tz_negative : Boolean;
  ok : Boolean;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?

  buffer := Trim(AStr);
  bufferPos := 1;
  bufferLen := Length(buffer);
  if ( bufferLen > 0 ) then begin
    Result := False;
    FillChar(ADate,SizeOf(ADate),#0);

    if ReadInt(y,DATE_SEP_CHAR) then begin
      Inc(bufferPos);
      if ReadInt(m,DATE_SEP_CHAR) then begin
        Inc(bufferPos);
        if ReadInt(d,#0) then begin
          Inc(bufferPos);
          tz_hh := 0;
          tz_mn := 0;
          if ( bufferPos > bufferLen ) then begin
            hh := 0;
            mn := 0;
            ss := 0;
            ssss := 0;
            ok := True;
          end else begin
            ok := ( buffer[bufferPos -1] = TIME_MARKER_CHAR ) and ReadInt(hh,TIME_SEP_CHAR);
            if ok then begin
              Inc(bufferPos);
              ok := ReadInt(mn,TIME_SEP_CHAR);
              if ok then begin
                Inc(bufferPos);
                ok := ReadInt(ss,#0);
                if ok and ( bufferPos < bufferLen ) and ( buffer[bufferPos] = '.' ) then begin
                  Inc(bufferPos);
                  ok := ReadMiliSeconds(ssss,#0);
                end else begin
                  ssss := 0;
                end;
                if ok and ( bufferPos < bufferLen ) then begin
                  tz_negative := ( buffer[bufferPos] = '-' );
                  Inc(bufferPos);
                  ok := ReadInt(tz_hh,TIME_SEP_CHAR);
                  if ok then begin
                    Inc(bufferPos);
                    ok := ReadInt(tz_mn,#0);
                    if ok and tz_negative then begin
                      tz_hh := -tz_hh;
                      tz_mn := -tz_mn;
                    end;
                  end;
                end;
              end;
            end;
          end;
          if ok then begin
            if ( ( y + m + d + hh + mn + ss + ssss ) = 0 ) then
              ADate.Date := 0
            else
              ADate.Date := EncodeDate(y,m,d) + EncodeTime(hh,mn,ss,ssss);
            ADate.HourOffset := tz_hh;
            ADate.MinuteOffset := tz_mn;
            Result := True;
          end;
        end;
      end;
    end;
  end else begin
    FillChar(ADate,SizeOf(ADate),#0);
    Result := True;
  end;
end;

function xsd_StrToDate(const AStr : string) : TDateTimeRec;
begin
  if not xsd_TryStrToDate(AStr,Result) then
    raise EConvertError.CreateFmt(SERR_InvalidDate,[AStr]);
end;

function xsd_DateTimeToStr(const ADate : TDateTimeRec) : string;
var
  locDate : TDateTime;
  s, buffer : string;
  d, m, y : Word;
  hh, mn, ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  locDate := ADate.Date;
  if ( ADate.HourOffset <> 0 ) then
    locDate := IncHour(locDate,-ADate.HourOffset);
  if ( ADate.MinuteOffset <> 0 ) then
    locDate := IncMinute(locDate,-ADate.MinuteOffset);
  DecodeDate(locDate,y,m,d);
    s := IntToStr(y);
    buffer := IntToStr(m);
    if ( Length(s) < 4 ) then
      s := StringOfChar('0', ( 4 - Length(s) ) ) + s;
    if ( m < 10 ) then
      buffer := '0' + buffer;
    s := Format('%s-%s',[s,buffer]);

    buffer := IntToStr(d);
    if ( d < 10 ) then
      buffer := '0' + buffer;
    s := Format('%s-%s',[s,buffer]);

  DecodeTime(locDate,hh,mn,ss,ssss);
    buffer := IntToStr(hh);
    if ( hh < 10 ) then
      buffer := '0' + buffer;
    s := Format('%sT%s',[s,buffer]);

    buffer := IntToStr(mn);
    if ( mn < 10 ) then
      buffer := '0' + buffer;
    s := Format('%s:%s',[s,buffer]);

    buffer := IntToStr(ss);
    if ( ss < 10 ) then
      buffer := '0' + buffer;
    s := Format('%s:%s',[s,buffer]);

    if ( ssss > 0 ) then begin
      buffer := IntToStr(ssss);
      case ssss of
        0..9   : buffer := '00' + buffer;
        10..99 : buffer := '0' + buffer;
      end;
      s := Format('%s.%s',[s,buffer]);
    end;

  Result := s + 'Z';
end;

function xsd_DateTimeToStr(const ADate : TDateTime) : string;
var
  tmpDate : TDateTimeRec;
begin
  FillChar(tmpDate,SizeOf(TDateTimeRec),#0);
  tmpDate.Date := ADate;
  Result := xsd_DateTimeToStr(tmpDate);
end;

end.
