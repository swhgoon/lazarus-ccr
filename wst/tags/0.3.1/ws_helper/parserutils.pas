{
    This unit is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit parserutils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

const
  sNEW_LINE = {$ifndef Unix}#13#10{$else}#10{$endif};

  function IsStrEmpty(Const AStr : String):Boolean;
  
implementation

function IsStrEmpty(Const AStr : String):Boolean;
begin
  Result := ( Length(Trim(AStr)) = 0 );
end;

end.

