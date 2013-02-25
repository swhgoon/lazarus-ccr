{ RxDBGrid sort engine module for FBDataSet

  Copyright (C) 2011 BugMaker from freepascal.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit exsortsql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, RxDBGrid;

type

  { TSQLQuerySortEngine }

  TSQLQuerySortEngine = class(TRxDBGridSortEngine)
  public
    procedure Sort(Field:TField; ADataSet:TDataSet; Asc:boolean; SortOptions:TRxSortEngineOptions);override;
  end;

implementation
uses SQLDB, synRegExpr, strUtils;


procedure TSQLQuerySortEngine.Sort(Field: TField; ADataSet: TDataSet;
  Asc: boolean; SortOptions: TRxSortEngineOptions);
var cmd:string;
  strLen:Integer;
  RegExpr: TRegExpr;
  Mask:String;
  s:String;
begin
  if Assigned(ADataSet) and ADataset.Active then begin
    cmd:=(ADataSet as TSQLQuery).SQL.Text;
    strlen:=length(cmd);
    //Регулярное выражение позволяет найти уже имеющуюся конструкцию ORDER BY,
    //если она написана одной строкой, и между словами не понапихали комментариев :)
    //Работоспособные примеры:
    //ORDER BY FIELD1, FIELD2 DESC, FIELD100500
    //oRdeR bY    fielD1   ,   FiElD2,FieLD100500 DESC
    //Неработоспособный:
    //ORDER BY FIELD1,
    //FIELD2,
    //FIELD100500
    mask:='(?i)(^|\s)\s*order\s+by\s+\S+\.?\S*(\s+desc)?\s*(,\s*\S+\.?\S*(\s+desc)?(^|s*))*';
    with TRegExpr.Create do begin

      Expression := mask;
      if Exec(cmd) then begin
        s:=LeftStr(cmd,MatchPos[0]-1)
        +slinebreak+'order by '
        +Field.FieldName
        +' ';
        if not asc then s:=s+'DESC';
        s:=s+slineBreak
        +RightStr(cmd, strlen-MatchPos[0]-MatchLen[0]+1);
      end
      else
      begin
        s:=cmd+slinebreak+'order by '
        +Field.FieldName
        +' ';
        if not asc then s:=s+'DESC';
        s:=s+slineBreak
      end;
      ADataSet.Active:=False;
      (ADataSet as TSQLQuery).SQL.Text:=s;
      ADataSet.Active:=True;

      Free;
    end;
  end;
end;

initialization
  RegisterRxDBGridSortEngine(TSQLQuerySortEngine, 'TSQLQuery');
end.
