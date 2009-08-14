(*******************************************************************************
 *  test_client.pas: App to test BeepFp classes
 *  Copyright (C) 2009,  Wimpie Nortje <wimpienortje@gmail.com>
 *
 *  This file is part of BeepFp.
 *
 *  BeepFp is free software: you can redistribute it and/or modify it under the
 *  terms of the GNU Lesser General Public License as published by the Free
 *  Software Foundation, either version 3 of the License, or (at your option)
 *  any later version.
 *
 *  BeepFp is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 *  more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with BeepFp.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  BeepFp is further covered by a special exception as described in the file
 *  COPYING.modifiedLGPL.txt which should have been included in the
 *  distribution. If not, see
 *  <http://svn.freepascal.org/svn/lazarus/trunk/COPYING.modifiedLGPL.txt>
 *******************************************************************************
 *  test_client is an application used to test the BeepFp classes, mainly
 *  used during development. To see how to use the classes in a real application,
 *  see BEEP_Client and BEEP_Listen
 ******************************************************************************)
program test_listener;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  //{$IFDEF UseCThreads}
  cthreads,
  //{$ENDIF}
  {$ENDIF}
  Forms, LResources, Interfaces,  TestListenMain,
  BeepChannel, BeepConnection, BeepContext, BeepFrame, BeepListener,
  BeepProfile;

{
cthread   cmem    stack   crash
N         N       N       N
N         N       Y       Y sodra listen
Y         N       Y       Y sodra listen
N         Y       Y       Y sodra listen
Y         Y       Y       Y sodra listen
}

{$IFDEF WINDOWS}{$R test_listener.rc}{$ENDIF}

begin
  {$I test_listener.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

