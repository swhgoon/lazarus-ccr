(*
  TDI - Tabbed Document Interface for Lazarus - Show multiple forms in Tabs
  Copyright (C) 2012  Daniel Simões de Almeida

  You can get the latest version of this file in Lazarus CCR, located in:
  https://lazarus-ccr.svn.sourceforge.net/svnroot/lazarus-ccr/components/tdi

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
  You can also get a copy of the license accessing the address:
  http://www.opensource.org/licenses/lgpl-license.php

  Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br
       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170
*)

unit TDIConst ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils ;

resourcestring
  sOwnerIsNotWinControl = 'TDINoteBook.Owner is not a TWinControl descendant';
  sFormNotAssigned = 'Parameter AForm not Assigned';
  sMainMenuNotAssigned = 'TTDINoteBook.MainMenu not assigned';
  sActionTabsMenu = 'Tabs';
  sActionCloseTab = 'Close Tab';
  sActionCloseAllTabs = 'Close All Tabs';
  sActionNextTab = 'Next Tab';
  sActionPreviousTab = 'Previous Tab';

implementation

end.

