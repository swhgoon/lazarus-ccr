{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the iPhone Laz Extension                            *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit xcodetemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

procedure PrepareTemplateFile(Src, TemplateValues: TStrings; BuildSettings: TFPStringHashTable);

const
  XCodeProjectTemplateIconID : AnsiString ='0AE3FFA610F3C9AF00A9B007,';
  XCodeProjectTemplateIcon   : AnsiString =
    '0AE3FFA610F3C9AF00A9B007 /* Icon.png */ = {isa = PBXFileReference; lastKnownFileType = image.png; path = Icon.png; sourceTree = "<group>"; };';

  XCodeIconFile : AnsiString = '0A2C67AE10F3CFB800F48811,';
  XCodeIconFileRef : AnsiString =
    '0A2C67AE10F3CFB800F48811 /* Icon.png in Resources */ = {isa = PBXBuildFile; fileRef = 0AE3FFA610F3C9AF00A9B007 /* Icon.png */; };';


  XCodeProjectTemplate : AnsiString =
    '// !$*UTF8*$!'#10+
    '{'#10+
    '	archiveVersion = 1;'#10+
    '	classes = {'#10+
    '	};'#10+
    '	objectVersion = 45;'#10+
    '	objects = {'#10+
    '  '#10+
    '  ??iconfileref'#10+
    '  '#10+
    '/* Begin PBXFileReference section */'#10+
    '		0A85A8AE10F0D28700AB8400 /* ??bundle */ = {isa = PBXFileReference; explicitFileType = wrapper.application; includeInIndex = 0; path = ??bundle; sourceTree = BUILT_PRODUCTS_DIR; };'#10+
    '		0A85A8B110F0D28700AB8400 /* ??plist */ = {isa = PBXFileReference; explicitFileType = text.plist.xml; name = ??plist; path = ??plist; sourceTree = SOURCE_ROOT; };'#10+
    '   ??icon'#10+
    '/* End PBXFileReference section */'#10+
    ''#10+
    '/* Begin PBXGroup section */'#10+
    '		0A52AE8110F0D05300478C4F = {'#10+
    '			isa = PBXGroup;'#10+
    '			children = ('#10+
    '       ??iconid'#10+
    '				0A85A8AF10F0D28700AB8400 /* Products */,'#10+
    '				0A85A8B110F0D28700AB8400 /* ??plist */,'#10+
    '			);'#10+
    '			sourceTree = "<group>";'#10+
    '		};'#10+
    '		0A85A8AF10F0D28700AB8400 /* Products */ = {'#10+
    '			isa = PBXGroup;'#10+
    '			children = ('#10+
    '				0A85A8AE10F0D28700AB8400 /* ??bundle */,'#10+
    '			);'#10+
    '			name = Products;'#10+
    '			sourceTree = "<group>";'#10+
    '		};'#10+
    '/* End PBXGroup section */'#10+
    ''#10+
    '/* Begin PBXNativeTarget section */'#10+
    '		0A85A8AD10F0D28700AB8400 = {'#10+
    '			isa = PBXNativeTarget;'#10+
    '			buildConfigurationList = 0A85A8B410F0D28800AB8400 /* Build configuration list for PBXNativeTarget */;'#10+
    '			buildPhases = ('#10+
    '				0A85A8B810F0D2D400AB8400 /* ShellScript */,'#10+
    '       0A2C67A610F3CEFA00F48811 /* Resources */,'#10+
    '			);'#10+
    '			buildRules = ('#10+
    '			);'#10+
    '			dependencies = ('#10+
    '			);'#10+
    '			name = ??targetname;'#10+
    '			productName = ??productname;'#10+
    '			productReference = 0A85A8AE10F0D28700AB8400 /* ??bundle */;'#10+
    '			productType = "com.apple.product-type.application";'#10+
    '		};'#10+
    '/* End PBXNativeTarget section */'#10+
    ''#10+
    '/* Begin PBXProject section */'#10+
    '		0A52AE8310F0D05300478C4F /* Project object */ = {'#10+
    '			isa = PBXProject;'#10+
    '			buildConfigurationList = 0A52AE8610F0D05300478C4F /* Build configuration list for PBXProject "project1" */;'#10+
    '			compatibilityVersion = "Xcode 3.1";'#10+
    '			hasScannedForEncodings = 0;'#10+
    '			mainGroup = 0A52AE8110F0D05300478C4F;'#10+
    '			productRefGroup = 0A85A8AF10F0D28700AB8400 /* Products */;'#10+
    '			projectDirPath = "";'#10+
    '			projectRoot = "";'#10+
    '			targets = ('#10+
    '				0A85A8AD10F0D28700AB8400,'#10+
    '			);'#10+
    '		};'#10+
    '/* End PBXProject section */'#10+
    ''#10+
    '/* Begin PBXResourcesBuildPhase section */'#10+
    '		0A2C67A610F3CEFA00F48811 /* Resources */ = {'#10+
    '			isa = PBXResourcesBuildPhase;'#10+
    '			buildActionMask = 2147483647;'#10+
    '			files = ('#10+
    '				??iconfile'#10+
    '			);'#10+
    '			runOnlyForDeploymentPostprocessing = 0;'#10+
    '		};'#10+
    '/* End PBXResourcesBuildPhase section */'#10+
    ''#10+
    '/* Begin PBXShellScriptBuildPhase section */'#10+
    '		0A85A8B810F0D2D400AB8400 /* ShellScript */ = {'#10+
    '			isa = PBXShellScriptBuildPhase;'#10+
    '			buildActionMask = 2147483647;'#10+
    '			files = ('#10+
    '			);'#10+
    '			inputPaths = ('#10+
    '			);'#10+
    '			outputPaths = ('#10+
    '			);'#10+
    '			runOnlyForDeploymentPostprocessing = 0;'#10+
    '			shellPath = /bin/sh;'#10+
    '			shellScript = "if [ x\"$ACTION\" != \"xbuild\" ]; then\n  # in case running scripts during cleaning gets fixed\n  exit 0\nfi\n\necho $FPC_COMPILER_PATH $FPC_COMPILER_OPTIONS $FPC_MAIN_FILE\n\n$FPC_COMPILER_PATH $FPC_COMPILER_OPTIONS $FPC_MAIN_FILE";'#10+
    '		};'#10+
    '/* End PBXShellScriptBuildPhase section */'#10+
    ''#10+
    '/* Begin XCBuildConfiguration section */'#10+
    '		0A52AE8510F0D05300478C4F /* Release */ = {'#10+
    '			isa = XCBuildConfiguration;'#10+
    '			buildSettings = {'#10+
    '				ARCHS = "$(ARCHS_STANDARD_32_BIT)";'#10+
    '                           "ARCHS[sdk=iphonesimulator*]" = "$(ARCHS_STANDARD_32_BIT)";'#10+
    '				COPY_PHASE_STRIP = YES;'#10+
    '				FPC_OUTPUT_FILE = $BUILT_PRODUCTS_DIR/$EXECUTABLE_PATH;'#10+
    '				FPC_COMPILER_OPTIONS = "-Parm -o$FPC_OUTPUT_FILE $FPC_CUSTOM_OPTIONS";'#10+
    '                           "FPC_COMPILER_OPTIONS[sdk=iphonesimulator*]" = "-Tiphonesim -Pi386 -o$FPC_OUTPUT_FILE $FPC_CUSTOM_OPTIONS";'#10+
    '				FPC_COMPILER_PATH = ;'#10+
    '				FPC_CUSTOM_OPTIONS = ;'#10+
    '                           "FPC_CUSTOM_OPTIONS[sdk=iphonesimulator*]" = ;'#10+
    '				FPC_MAIN_FILE = ;'#10+
    '				SDKROOT = iphoneos2.0;'#10+
    '				VALID_ARCHS = "armv6 armv7";'#10+
    '			};'#10+
    '			name = Release;'#10+
    '		};'#10+
    '		0A85A8B310F0D28800AB8400 /* Release */ = {'#10+
    '			isa = XCBuildConfiguration;'#10+
    '			buildSettings = {'#10+
    '				ALWAYS_SEARCH_USER_PATHS = YES;'#10+
    '				COPY_PHASE_STRIP = YES;'#10+
    '				DEBUG_INFORMATION_FORMAT = "dwarf-with-dsym";'#10+
    '				GCC_ENABLE_FIX_AND_CONTINUE = NO;'#10+
    '				GCC_PRECOMPILE_PREFIX_HEADER = YES;'#10+
    '				GCC_PREFIX_HEADER = "$(SYSTEM_LIBRARY_DIR)/Frameworks/UIKit.framework/Headers/UIKit.h";'#10+
    '				INFOPLIST_FILE = ??plist;'#10+
    '				INSTALL_PATH = "$(HOME)/Applications";'#10+
    '				OTHER_LDFLAGS = ('#10+
    '					"-framework",'#10+
    '					Foundation,'#10+
    '					"-framework",'#10+
    '					UIKit,'#10+
    '				);'#10+
    '				PREBINDING = NO;'#10+
    '				PRODUCT_NAME = ??productname;'#10+
    '				SDKROOT = iphoneos2.0;'#10+
    '				ZERO_LINK = NO;'#10+
    '			};'#10+
    '			name = Release;'#10+
    '		};'#10+
    '/* End XCBuildConfiguration section */'#10+
    '    '#10+
    '/* Begin XCConfigurationList section */'#10+
    '		0A52AE8610F0D05300478C4F /* Build configuration list for PBXProject "project1" */ = {'#10+
    '			isa = XCConfigurationList;'#10+
    '			buildConfigurations = ('#10+
    '				0A52AE8510F0D05300478C4F /* Release */,'#10+
    '			);'#10+
    '			defaultConfigurationIsVisible = 0;'#10+
    '			defaultConfigurationName = Release;'#10+
    '		};'#10+
    '		0A85A8B410F0D28800AB8400 /* Build configuration list for PBXNativeTarget */ = {'#10+
    '			isa = XCConfigurationList;'#10+
    '			buildConfigurations = ('#10+
    '				0A85A8B310F0D28800AB8400 /* Release */,'#10+
    '			);'#10+
    '			defaultConfigurationIsVisible = 0;'#10+
    '			defaultConfigurationName = Release;'#10+
    '		};'#10+
    '/* End XCConfigurationList section */'#10+
    '	};'#10+
    '	rootObject = 0A52AE8310F0D05300478C4F /* Project object */;'#10+
    '}'#10;

implementation

function GetValueName(const Source: String; idx: Integer): String;
var
  i : integer;
  InQuote: boolean;
const
  //todo: expand symbols charset
  Symbols: set of char = [#9, #32, #10,#13,
    '=',':',';','-','+','*','/','\','!','@','#',
    '$','%','^','&','(',')','~','`',''''];
begin
  InQuote:=false;
  for i:=idx to length(Source) do begin
    if InQuote then begin
      if Source[i]='"' then InQuote := false;
    end else if Source[i] = '"' then
      InQuote := true
    else if Source[i] in Symbols then begin
      Result:=Copy(Source, idx, i-idx);
      Exit;
    end;
  end;
  Result:=Copy(Source, idx, length(Source)-idx+1);
end;

function ChangeValues(const Prefix, Source: String; Values: TStrings): String;
var
  i   : integer;
  nm  : string;
  v   : string;
begin
  Result:=Source;
  i:=Pos(Prefix, Result);
  while i>0 do begin
    nm:=GetValueName(Result, i+length(Prefix));
    Delete(Result, i, length(Prefix)+length(nm));
    v:=Values.Values[nm];

    if Pos(Prefix, v) <= 0 then  // don't allow circular prefix used, to avoid infinite loops
      Insert(v, Result, i);
    i:=Pos(Prefix, Result);
  end;
end;

procedure PrepareTemplateFile(Src, TemplateValues: TStrings; BuildSettings: TFPStringHashTable);
//todo: Better code to update XCode project file!
var
  i, j       : Integer;
  nm, s, v   : String;
  isSettings : Boolean;

begin
  if not Assigned(Src) then Exit;

  if Assigned(TemplateValues) then
    for i:=0 to Src.Count-1 do
      Src[i]:=ChangeValues('??', Src[i], TemplateValues);

  isSettings:=false;

  if Assigned(BuildSettings) and (BuildSettings.Count>0) then begin

    for i:=0 to Src.Count-1 do begin
      if not isSettings then
        isSettings:=Pos('buildSettings', Src[i])>0
      else begin
        if Trim(Src[i])='};' then
          isSettings:=false
        else begin
          j:=1;
          s:=Src[i];
          while (j<=length(s)) and (s[j] in [#9, #32]) do
            inc(j);

          nm:=GetValueName(s, j);

          if Assigned(BuildSettings.Find(nm)) then begin
            v:=BuildSettings.Items[nm];
            Src[i]:=Copy(Src[i], 1, j-1)+nm+ ' = ' + v + ';';
          end;
        end;
      end; {of else}
    end;
  end;
end;


end.

