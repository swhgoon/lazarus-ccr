unit TableData;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  db,
  BufDataset;

function CreateDataset: TBufDataset;

implementation

const
  BondArray: array[0..2] of string = (
    'Dr. No',
    'From Russia with Love',
    'Goldfinger'
  );

function CreateDataset: TBufDataset;
var
  i: Integer;
begin
  result := TBufDataset.Create(nil);
  result.FieldDefs.Add('ID',ftInteger);
  result.FieldDefs.Add('Name',ftString,25);
  result.CreateDataset;
  result.FilterOptions:=[foCaseInsensitive];
  result.Open;
  for i := 0 to high(BondArray) do
    begin
    result.Append;
    result.FieldByName('id').AsInteger:=i+1;
    result.FieldByName('Name').AsString:=BondArray[i];
    end;
  result.Post;
end;

end.

