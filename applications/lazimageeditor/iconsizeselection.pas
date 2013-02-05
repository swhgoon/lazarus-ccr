unit iconsizeselection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TSelectIconSizeForm }

  TSelectIconSizeForm = class(TForm)
    Button1: TButton;
    IconSizeComboBox: TComboBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SelectIconSizeForm: TSelectIconSizeForm;

implementation

{$R *.lfm}

end.

