unit ViewCode;

interface

   uses
      Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
      Dialogs, StdCtrls, LResources;

   type
      TfrmViewCode = 
      class(TForm)
         memo: TMemo;
         procedure FormActivate(Sender: TObject);
         
         private
         { Private declarations }
         public
         { Public declarations }
      end;

implementation
{.$R *.dfm}

   procedure TfrmViewCode.FormActivate(Sender: TObject);
   var
      r  : TRect;
   begin
      {get size of desktop}
      SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0);
      Height := r.Bottom-Top;
      Width  := r.Right-Left;
   end;

initialization
  {$I ViewCode.lrs}

end.
