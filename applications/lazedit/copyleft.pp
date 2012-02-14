unit CopyLeft;

{
  EPlus: a text editor with built-in features for HTML editing and
  Syntax Highlighting for several text formats
  (html, xml, css, javascript, pascal, c/c++, perl, python, php, bat, ini, diff)

  Copyright (C) 2011 by Bart Broersma & Flying Sheep Inc.
  http://home.tiscali.nl/~knmg0017/software.htm

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


{$mode objfpc}{$H+}

interface

//uses GPL;

const
  AppName = 'EPlus';
  AboutTitle = 'Over ' + AppName;
  AppVersion = 'Versie 1.0.0 RC1';
  CopyLeftStatement = 'Copyright (c) 2011, 2012 by Bart Broersma & FlyingSheep Inc.';
  MetaGeneratorName = AppName + #32 + AppVersion + ' by Bart Broersma and Flying Sheep Inc.';
  AuthorWebName = 'Bart & Mariska''s Webstek';
  AuthorWebUrl = 'http://home.tiscali.nl/~knmg0017/';
  LicenseText = ''; //LGPL_Text;
  LicenseUrl = '';//LGPL_Url;
  LicenseName = 'Gnu LGPL';

implementation

end.

