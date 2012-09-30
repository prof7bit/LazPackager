{ LazDebian stores the setttings and does the debian packaging

  Copyright (C) 2012 Bernd Kreuss prof7bit@gmail.com

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  A copy of the GNU General Public License is available on the World Wide
  Web at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite
  330, Boston, MA 02111-1307, USA.
}

unit lazdebiansettings;

{$mode objfpc}{$H+}

interface

const
  TAB = #9;
  LF = #10;

  DEFAULT_EXPORT
    = 'cp *.lpi ?TEMPFOLDER?' + LF
    + 'cp *.lpr ?TEMPFOLDER?' + LF
    + 'cp *.pas ?TEMPFOLDER?' + LF
    + 'cp *.lfm ?TEMPFOLDER?' + LF
    + 'cp *.ico ?TEMPFOLDER?' + LF
    ;

  DEFAULT_MAKEFILE
    = 'PREFIX = /usr/local'+ LF
    + LF
    + '# debuild will set DESTDIR to the fakeroot path and' + LF
    + '# in the override rules we will change PREFIX to /usr' + LF
    + 'BINDIR = $(DESTDIR)$(PREFIX)/bin'+ LF
    + LF
    + '.PHONY : all'+ LF
    + 'all:'+ LF
    + TAB + 'lazbuild ?PROJECT?'+ LF
    + LF
    + '.PHONY : clean'+ LF
    + 'clean:'+ LF
    + TAB + '$(RM) -r lib'+ LF
    + TAB + '$(RM) *.res'+ LF
    + TAB + '$(RM) ?EXECUTABLE?'+ LF
    + LF
    + '.PHONY : install'+ LF
    + 'install:'+ LF
    + TAB + 'mkdir -p $(BINDIR)'+ LF
    + TAB + 'install -s ?EXECUTABLE? $(BINDIR)/'+ LF
    ;

  DEFAULT_CONTROL
    = 'Source: ?PACKAGE_NAME?'+ LF
    + 'Maintainer: ?MAINTAINER? <?MAINTAINER_EMAIL?>'+ LF
    + 'Section: misc'+ LF
    + 'Priority: optional'+ LF
    + 'Standards-Version: 3.9.3'+ LF
    + 'Build-Depends: fpc, lcl, lcl-utils, lazarus, debhelper (>= 8)'+ LF
    + LF
    + 'Package: ?PACKAGE_NAME?'+ LF
    + 'Architecture: any'+ LF
    + 'Depends: ${shlibs:Depends}, ${misc:Depends},'+ LF
    + 'Description: ?DESCRIPTION?'+ LF
    + ' ?DESCRIPTION_LONG?'+ LF
    ;

  DEFAULT_RULES
    = '#!/usr/bin/make -f' + LF
    + LF
    + '# see http://www.debian.org/doc/manuals/maint-guide/dreq.en.html' + LF
    + LF
    + 'override_dh_auto_build:' + LF
	  + TAB + 'dh_auto_build -- PREFIX=/usr' + LF
    + LF
    + 'override_dh_auto_install:' + LF
    + TAB + 'dh_auto_install -- PREFIX=/usr' + LF
    + LF
    + '%:' + LF
    + TAB + 'dh $@' + LF
    ;

  DEFAULT_CHANGELOG
    = '?PACKAGE_NAME? (?FULLVERSION?) ?SERIES?; urgency=low' + LF
    + LF
    + '  * Original version ?VERSION? packaged with lazdebian' + LF
    + LF
    + ' -- ?MAINTAINER? <?MAINTAINER_EMAIL?>  ?DATE?' + LF
    ;

  DEFAULT_COPYRIGHT
    = 'Format: http://www.debian.org/doc/packaging-manuals/copyright-format/1.0/' + LF
    + LF
    + 'Files: *' + LF
    + 'Copyright: ?COPYRIGHT?' + LF
    + 'License: GPL-2+' + LF
    + ' This program is free software; you can redistribute it and/or modify' + LF
    + ' it under the terms of the GNU General Public License as published by' + LF
    + ' the Free Software Foundation; either version 2 of the License, or' + LF
    + ' at your option) any later version.' + LF
    + ' .' + LF
    + ' This program is distributed in the hope that it will be useful,' + LF
    + ' but WITHOUT ANY WARRANTY; without even the implied warranty of' + LF
    + ' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the' + LF
    + ' GNU General Public License for more details.' + LF
    + ' .' + LF
    + ' You should have received a copy of the GNU General Public License along' + LF
    + ' with this program; if not, write to the Free Software Foundation, Inc.,' + LF
    + ' 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.' + LF
    + ' .' + LF
    + ' On Debian systems, the full text of the GNU General Public' + LF
    + ' License version 2 can be found in the file' + LF
    + ' /usr/share/common-licenses/GPL-2' + LF
    ;

type
  { TSettings }

  TSettings = class
    AuthorCopyright: String;
    Description: String;
    DescriptionLong: String;
    Maintainer: String;
    MaintainerEmail: String;
    Series: String;
    PackageName: String;
    ExportCommands: String;
    PPA: String;
    Makefile: String;
    Control: String;
    Rules: String;
    Changelog: String;
    Copyright: String;
    constructor Create;
    destructor Destroy; override;
    procedure Save;
    procedure Load;
  private
    procedure SaveValue(Key, Value: String);
    function LoadValue(Key, DefaultValue: String): String;
  public
    function GetVersion: String;
    function GetDateFormatted: String;
    function GetExecutableFilenameRelative: String;
    function GetProjectFilenameRelative: String;
    function GetOrigFolderNameOnly: String;
    function GetTempPathAbsolute: String;
    function GetOrigTarNameOnly: String;
    function GetProjectPathAbsolute: String;
    function GetDebuildPathAbsolute: String;
    function GetDebuildSrcPathAbsolute: String;
    function GetDebuildSrcDebianPathAbsolute: String;
    function FillTemplate(Template: String): String;
  private
    procedure CreateBuildScript(Binary, Sign, Upload: Boolean);
    procedure CreateDebianFiles;
    procedure RunBuildScript(Data: PtrInt);
    procedure RunBuildScriptAsync;
  public
    procedure DoMakePackage(Binary, Sign, Upload: Boolean);
  end;

implementation
uses
  Classes,
  sysutils,
  Forms,
  process,
  FileUtil,
  LazIDEIntf,
  ProjectResourcesIntf,
  MacroIntf,
  //W32VersionInfo,
  IDEExternToolIntf;

type

  { TMyAbstractProjectResources }
  TMyAbstractProjectResources = class(TAbstractProjectResources)
    // need this to call a protected class function
    class function GetList: TList;
  end;

  TFileProductVersion = array[0..3] of word;

  {This should have the same memory layout as the original one}
  TSameLayoutAsTProjectVersionInfo = class(TAbstractProjectResource)
    FAutoIncrementBuild: boolean;
    FHexCharSet: string;
    FHexLang: string;
    FStringTable: TObject;
    FUseVersionInfo: boolean;
    FVersion: TFileProductVersion;
    // more fields follow but we are not
    // interested anymore, only need FVersion
  end;


procedure CreateFile(FullPathName, Contents: String);
var
  S: TFileStream;
begin
  try
    S := TFileStream.Create(FullPathName, fmCreate);
    S.Write(Contents[1], Length(Contents));
  finally
    S.Free;
  end;
end;

{ TAbstractProjectResources1 }

class function TMyAbstractProjectResources.GetList: TList;
begin
  Result := GetRegisteredResources;
end;

{ TSettings }

constructor TSettings.Create;
begin
  Load;
end;

destructor TSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TSettings.Save;
begin
  SaveValue('lazdebian_copyright', AuthorCopyright);
  SaveValue('lazdebian_description', Description);
  SaveValue('lazdebian_description_long', DescriptionLong);
  SaveValue('lazdebian_maintainer', Maintainer);
  SaveValue('lazdebian_maintainer_email', MaintainerEmail);
  SaveValue('lazdebian_series', Series);
  SaveValue('lazdebian_package_name', PackageName);
  SaveValue('lazdebian_export_cmd', ExportCommands);
  SaveValue('lazdebian_ppa', PPA);

  SaveValue('lazdebian_tpl_makefile', Makefile);
  SaveValue('lazdebian_tpl_control', Control);
  SaveValue('lazdebian_tpl_rules', Rules);
  SaveValue('lazdebian_tpl_changelog', Changelog);
  SaveValue('lazdebian_tpl_copyright', Copyright);
end;

procedure TSettings.Load;
begin
  AuthorCopyright := LoadValue('lazdebian_copyright', '2012 Jane Doe');
  Description := LoadValue('lazdebian_description', 'this is a program');
  DescriptionLong := LoadValue('lazdebian_description_long', 'long description may not be empty!');
  Maintainer := LoadValue('lazdebian_maintainer', 'John Doe');
  MaintainerEmail := LoadValue('lazdebian_maintainer_email', 'john_doe@example.invalid');
  Series := LoadValue('lazdebian_series', 'precise');
  PackageName := LoadValue('lazdebian_package_name', 'debian-package-name');
  ExportCommands := LoadValue('lazdebian_export_cmd', DEFAULT_EXPORT);
  PPA := LoadValue('lazdebian_ppa', 'ppa:johndoe/use-your-own');

  Makefile := LoadValue('lazdebian_tpl_makefile', DEFAULT_MAKEFILE);
  Control := LoadValue('lazdebian_tpl_control', DEFAULT_CONTROL);
  Rules := LoadValue('lazdebian_tpl_rules', DEFAULT_RULES);
  Changelog := LoadValue('lazdebian_tpl_changelog', DEFAULT_CHANGELOG);
  Copyright := LoadValue('lazdebian_tpl_copyright', DEFAULT_COPYRIGHT);
end;

procedure TSettings.SaveValue(Key, Value: String);
begin
  LazarusIDE.ActiveProject.CustomData.Values[Key] := Value;
  LazarusIDE.ActiveProject.Modified := True;
end;

function TSettings.GetVersion: String;
var
  ResList: TAbstractProjectResources;
  Resource: TAbstractProjectResource;
  ResClass: TAbstractProjectResourceClass;
  ResClassList: TList;
  P: Pointer;
  VerInfo: TSameLayoutAsTProjectVersionInfo;

begin
  ResList := LazarusIDE.ActiveProject.Resources as TAbstractProjectResources;
  ResClassList := TMyAbstractProjectResources.GetList;
  for P in ResClassList do begin
    ResClass := TAbstractProjectResourceClass(p);
    Resource := ResList.Resource[ResClass];
    if Resource.ClassName = 'TProjectVersionInfo' then begin
      VerInfo := TSameLayoutAsTProjectVersionInfo(Resource);
      Result := Format('%d.%d.%d.%d', [VerInfo.FVersion[0],
                                       VerInfo.FVersion[1],
                                       VerInfo.FVersion[2],
                                       VerInfo.FVersion[3]]);
      break;
    end;
  end;
end;

function TSettings.GetDateFormatted: String;
var
  P: TProcess;
  N: Integer;
begin
  P := TProcess.Create(nil);
  P.Executable := 'date';
  P.Parameters.Add('-R');
  P.Options := [poUsePipes, poWaitOnExit];
  try
    P.Execute;
    SetLength(Result, 31);
    // needs to look like this; "Thu, 27 Sep 2012 19:19:14 +0200"
    // exactly 31 characters long, no more, no less.
    N := P.Output.Read(Result[1], 31);
    if N < 31 then
      Result := '### date -R gave wrong data ###';
  except
    Result := '#### error calling date -R ####';
  end;
  P.Free;
end;

function TSettings.GetExecutableFilenameRelative: String;
begin
  Result:='$(TargetFile)';
  if not IDEMacros.SubstituteMacros(Result) then
    raise Exception.Create('unable to retrieve target file of project');
  Result := CreateRelativePath(Result, GetProjectPathAbsolute);
end;

function TSettings.GetProjectFilenameRelative: String;
begin
  Result := LazarusIDE.ActiveProject.ProjectInfoFile;
  Result := CreateRelativePath(Result, GetProjectPathAbsolute);
end;

function TSettings.GetOrigFolderNameOnly: String;
begin
  Result := Format('%s-%s', [PackageName, GetVersion]);
end;

function TSettings.GetTempPathAbsolute: String;
begin
  Result := ConcatPaths([GetTempDir, GetOrigFolderNameOnly]);
end;

function TSettings.GetOrigTarNameOnly: String;
begin
  Result := Format('%s_%s.orig.tar.gz', [PackageName, GetVersion]);
end;

function TSettings.GetProjectPathAbsolute: String;
begin
  Result := ExtractFileDir(LazarusIDE.ActiveProject.ProjectInfoFile);
end;

function TSettings.GetDebuildPathAbsolute: String;
begin
  Result := ConcatPaths([GetProjectPathAbsolute, 'DEBUILD']);
end;

function TSettings.GetDebuildSrcPathAbsolute: String;
begin
  Result := ConcatPaths([GetDebuildPathAbsolute, GetOrigFolderNameOnly]);
end;

function TSettings.GetDebuildSrcDebianPathAbsolute: String;
begin
  Result := ConcatPaths([GetDebuildSrcPathAbsolute, 'debian']);
end;

function TSettings.LoadValue(Key, DefaultValue: String): String;
begin
  Result := LazarusIDE.ActiveProject.CustomData.Values[Key];
  if Result = '' then begin
    Result := DefaultValue;
    SaveValue(Key, Result);
  end;
end;

function TSettings.FillTemplate(Template: String): String;
var
  Version: String;

  procedure Replace(R: array of String);
  var
    I,J: Integer;
  begin
    for I := 0 to High(R) div 2 do begin
      J := I shl 1;
      Template := StringReplace(Template, R[J], R[J+1], [rfReplaceAll]);
    end;
  end;

begin
  Version := GetVersion;

  Replace(['?COPYRIGHT?',         AuthorCopyright
          ,'?DESCRIPTION?',       Description
          ,'?DESCRIPTION_LONG?',  DescriptionLong
          ,'?MAINTAINER?',        Maintainer
          ,'?MAINTAINER_EMAIL?',  MaintainerEmail
          ,'?SERIES?',            Series
          ,'?PACKAGE_NAME?',      PackageName
          ,'?VERSION?',           Version
          ,'?FULLVERSION?',       Version + '-1'
          ,'?DATE?',              GetDateFormatted
          ,'?EXECUTABLE?',        GetExecutableFilenameRelative
          ,'?PROJECT?',           GetProjectFilenameRelative
          ,'?TEMPFOLDER?',        GetTempPathAbsolute
          ]);

  Result := Template;
end;

procedure TSettings.CreateBuildScript(Binary, Sign, Upload: Boolean);
var
  S: String;
  SName: String;
  DEBUILD: STRING;
begin
  s := '#!/bin/sh' + LF
    + LF
    + 'set -v' + LF
    + 'set -e' + LF
    + Format('cd "%s"', [GetProjectPathAbsolute]) + LF
    + Format('mkdir -p %s', [GetTempPathAbsolute]) + LF
    + FillTemplate(ExportCommands) + LF
    + LF
    + Format('cd %s', [GetTempPathAbsolute]) + LF
    + 'rm -rf DEBUILD' + LF
    + 'rm -f DEBUILD.sh' + LF
    + LF
    + 'cd ..' + LF
    + Format('tar czf %s %s', [GetOrigTarNameOnly, GetOrigFolderNameOnly]) + LF
    + Format('mv %s "%s"', [GetOrigFolderNameOnly, GetDebuildPathAbsolute]) + LF
    + Format('mv %s "%s"', [GetOrigTarNameOnly, GetDebuildPathAbsolute]) + LF
    + LF
    + Format('cd "%s"', [GetDebuildSrcPathAbsolute]) + LF
    + 'mkdir -p debian/source' + LF
    + 'echo "1.0" > debian/source/format' + LF
    + 'echo "8" > debian/compat' + LF
    + 'mv ../control debian/' + LF
    + 'mv ../rules debian/' + LF
    + 'chmod +x debian/rules' + LF
    + 'mv ../changelog debian/' + LF
    + 'mv ../copyright debian/' + LF
    + 'mv ../Makefile ./' + LF
    + LF;

  if Binary then
    S += 'debuild -d -us -uc' + LF
  else
    S += 'debuild -S -us -uc' + LF;

  if Sign then begin
    S += 'cd ..' + LF;
    S += 'xterm -e "debsign *.changes"' + LF;
  end;

  if Upload then begin
    S += Format('dput %s *.changes', [PPA]) + LF;
  end;

  SName := ConcatPaths([GetProjectPathAbsolute, 'DEBUILD.sh']);
  CreateFile(SName, S);
end;

procedure TSettings.CreateDebianFiles;
var
  DirDebuild: String;
begin
  DirDebuild := GetDebuildPathAbsolute;
  if DirectoryExists(DirDebuild) then
    DeleteDirectory(DirDebuild, False);
  MkDir(DirDebuild);
  CreateFile(ConcatPaths([DirDebuild, 'control']), FillTemplate(Control));
  CreateFile(ConcatPaths([DirDebuild, 'rules']), FillTemplate(Rules));
  CreateFile(ConcatPaths([DirDebuild, 'changelog']), FillTemplate(Changelog));
  CreateFile(ConcatPaths([DirDebuild, 'copyright']), FillTemplate(Copyright));
  {$warning FIXME: respect the setting "Makefile / Use Existing", implement this!}
  CreateFile(ConcatPaths([DirDebuild, 'Makefile']), FillTemplate(Makefile));
end;

procedure TSettings.RunBuildScript(Data: PtrInt);
var
  Tool: TIDEExternalToolOptions;
begin
  Tool := TIDEExternalToolOptions.Create;
  Tool.Filename := '/bin/sh';
  Tool.CmdLineParams := 'DEBUILD.sh';
  Tool.WorkingDirectory := GetProjectPathAbsolute;
  Tool.ShowAllOutput := True;
  RunExternalTool(Tool);
  Tool.Free;
  Self.Free;
end;

procedure TSettings.RunBuildScriptAsync;
begin
  Application.QueueAsyncCall(@RunBuildScript, 0);
end;

procedure TSettings.DoMakePackage(Binary, Sign, Upload: Boolean);
begin
  CreateDebianFiles;
  CreateBuildScript(Binary, Sign, Upload);
  RunBuildScriptAsync;
end;


end.

