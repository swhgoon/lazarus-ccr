
Program testaws;

Uses aws;

Var TMPAWS: TAWSAccess;

Begin
  tmpaws := tawsaccess.createRequest('pearl jam', ' vs');
  tmpaws.sendrequest;
  Repeat
  Until tmpaws.data_ready;
  tmpaws.albumcovertofile('test.jpg');
  Repeat
  Until tmpaws.data_ready;
End.
