unit lazdebianprocess;

{$mode objfpc}{$H+}

interface
uses
  lazdebiansettings;

procedure StartMakeBinaryPackage(Settings: TSettings; Sign: Boolean);
procedure StartMakeSourcePackage(Settings: TSettings; Sign: Boolean; Upload: Boolean);

var
  IsBuildingPackage: Boolean = False;

implementation
uses
  Classes,
  SysUtils,
  Forms,
  FileUtil,
  LCLType;


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

procedure CreateBuildScript(Settings: TSettings);
var
  S: String;
  SName: String;
  DEBUILD: STRING;
begin
  DEBUILD := ConcatPaths([Settings.GetProjectDir, 'DEBUILD']);
  s := '#!/bin/sh' + LF
    + LF
    + Format('cd "%s"', [Settings.GetProjectDir]) + LF
    + Format('mkdir -p %s', [Settings.Tempfolder]) + LF
    + Settings.FillTemplate(Settings.ExportCommands) + LF
    + LF
    + Format('cd %s', [Settings.Tempfolder]) + LF
    + 'rm -rf DEBUILD' + LF
    + 'rm -f DEBUILD.sh' + LF
    + LF
    + 'cd ..' + LF
    + Format('tar czf %s %s', [Settings.GetOrigTarName, Settings.GetOrigFolderName]) + LF
    + Format('mv %s "%s"', [Settings.GetOrigFolderName, DEBUILD]) + LF
    + Format('mv %s "%s"', [Settings.GetOrigTarName, DEBUILD]) + LF
    + LF
    + Format('cd "%s"', [ConcatPaths([DEBUILD, Settings.GetOrigFolderName])]) + LF
    + 'mkdir debian' + LF
    + 'mv ../control debian/' + LF
    + 'mv ../rules debian/' + LF
    + 'mv ../changelog debian/' + LF
    + 'mv ../copyright debian/' + LF
    + 'mv ../compat debian/' + LF
    + 'mv ../Makefile ./' + LF
    + 'debuild -S -us -uc' + LF
    ;

    SName := ConcatPaths([Settings.GetProjectDir, 'DEBUILD.sh']);
    CreateFile(SName, S);

end;

procedure CreateDebianFiles(Settings: TSettings);
var
  DirDebuild: String;
begin
  DirDebuild :=ConcatPaths([Settings.GetProjectDir, 'DEBUILD']);
  if DirectoryExists(DirDebuild) then
    DeleteDirectory(DirDebuild, False);
  MkDir(DirDebuild);
  CreateFile(ConcatPaths([DirDebuild, 'compat']), '8');
  CreateFile(ConcatPaths([DirDebuild, 'control']), Settings.FillTemplate(Settings.Control));
  CreateFile(ConcatPaths([DirDebuild, 'rules']), Settings.FillTemplate(Settings.Rules));
  CreateFile(ConcatPaths([DirDebuild, 'changelog']), Settings.FillTemplate(Settings.Changelog));
  CreateFile(ConcatPaths([DirDebuild, 'copyright']), Settings.FillTemplate(Settings.Copyright));
  {$warning FIXME: respect the setting "Makefile / Use Existing", implement this!}
  CreateFile(ConcatPaths([DirDebuild, 'Makefile']), Settings.FillTemplate(Settings.Makefile));
end;


procedure DebuildSource(Settings: TSettings);
var
  SourceDir: String;
  Script : TStringList;
begin
  SourceDir := ConcatPaths([Settings.GetProjectDir, 'DEBUILD', Settings.GetOrigFolderName]);
  Script := TStringList.Create;
  Script.Add('debuild -S -us -uc');
  //RunShellCommands(SourceDir, Script);
  Script.Free;
end;

procedure DebuildBinary(Settings: TSettings);
var
  SourceDir: String;
  Script : TStringList;
begin
  SourceDir := ConcatPaths([Settings.GetProjectDir, 'DEBUILD', Settings.GetOrigFolderName]);
  Script := TStringList.Create;
  Script.Add('debuild -d -us -uc');
  //RunShellCommands(SourceDir, Script);
  Script.Free;
end;


procedure WarnStillRunning;
begin
  Application.MessageBox('LazDebian still running', 'LazDebian', MB_OK + MB_ICONWARNING);
end;

procedure StartMakeBinaryPackage(Settings: TSettings; Sign: Boolean);
begin
end;

procedure StartMakeSourcePackage(Settings: TSettings; Sign: Boolean; Upload: Boolean);
begin
  CreateDebianFiles(Settings);
  CreateBuildScript(Settings);
end;


end.

