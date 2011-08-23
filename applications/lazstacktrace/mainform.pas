unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, process;

type

  { TForm1 }

  TForm1 = class(TForm)
    buttonResolveSymbols: TButton;
    editGDBPath: TFileNameEdit;
    editProgramPath: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    memoStacktrace: TMemo;
    procedure buttonResolveSymbolsClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.buttonResolveSymbolsClick(Sender: TObject);
var
  GDBProcess: TProcess;
  GDBOutput: TStringList;
  Str, StackStr, lCurAddress: string;
  lAddressPos: SizeInt;
  i: Integer;
  StrGdbOutput: String;
begin
  GDBProcess := TProcess.Create(nil);
  GDBOutput := TStringList.Create;
  memoStacktrace.Lines.BeginUpdate;
  try
    for i := 0 to memoStacktrace.Lines.Count - 1 do
    begin
      // Obtain the stack address or skip this line
      StackStr := memoStacktrace.Lines.Strings[i];
      lAddressPos := Pos('$', StackStr);
      if lAddressPos <= 0 then Continue;
      lCurAddress := Copy(StackStr, lAddressPos+1, 8);

      // Run GDB to get the symbol name
      Str := Format('%s --batch "--eval-command=info symbol 0x%s" %s', [editGDBPath.FileName, lCurAddress, editProgramPath.FileName]);
      GDBProcess.CommandLine := Str;
      GDBProcess.Options := GDBProcess.Options + [poWaitOnExit, poUsePipes, poStderrToOutPut];
      GDBProcess.Execute;
      GDBOutput.LoadFromStream(GDBProcess.Output);
      if GDBOutput.Count >= 1 then StrGdbOutput := GDBOutput.Strings[0]
      else StrGdbOutput := '';

      // Add the symbol name to the memo
      memoStacktrace.Lines.Strings[i] := Format('%s %s', [StackStr, StrGdbOutput]);
    end;
  finally
    GDBProcess.Free;
    GDBOutput.Free;
    memoStacktrace.Lines.EndUpdate;
  end;
end;

end.

