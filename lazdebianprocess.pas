unit lazdebianprocess;

{$mode objfpc}{$H+}

interface
uses
  lazdebiansettings;

procedure DoMakeBinaryPackage(Settings: TSettings; Sign: Boolean);
procedure DoMakeSourcePackage(Settings: TSettings; Sign: Boolean; Upload: Boolean);

implementation
uses
  Classes,
  SysUtils,
  Forms,
  process;

procedure RunShellCommands(Directory: String; Commands: TStringList);
var
  P: TProcess;
  Cmd: String;

  procedure Exec(C: String);
  begin
    C += LF;
    P.Input.Write(C[1], Length(C));
  end;

begin
  P := TProcess.Create(nil);
  P.Executable := '/bin/sh';
  P.CurrentDirectory := Directory;
  P.Options := [poUsePipes];
  try
    try
      P.Execute;
      for Cmd in Commands do
        Exec(Cmd);
      Exec('exit');
      P.WaitOnExit;
    finally
      P.Free;
    end;
  except
    on E: Exception do begin
      Application.ShowException(E);
    end;
  end;
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

procedure CopyOrig(Settings: TSettings);
var
  Script: TStringList;
  DEBUILD: STRING;
begin
  DEBUILD := ConcatPaths([Settings.GetProjectDir, 'DEBUILD']);
  Script := TStringList.Create;
  Script.Text := Settings.FillTemplate(Settings.ExportCommands);
  Script.Insert(0, 'rm -rf DEBUILD');
  // in between here are all the configured cp Script
  Script.Append('mkdir DEBUILD');
  Script.Append(Format('cd %s', [ConcatPaths([Settings.Tempfolder, '..'])]));
  Script.Append(Format('tar czf %s %s', [Settings.GetOrigTarName, Settings.GetOrigFolderName]));
  Script.Append(Format('mv %s "%s"', [Settings.GetOrigFolderName, DEBUILD]));
  Script.Append(Format('mv %s "%s"', [Settings.GetOrigTarName, DEBUILD]));
  RunShellCommands(Settings.GetProjectDir, Script);
  Script.Free;
end;

procedure CreateDebianFolder(Settings: TSettings);
var
  Source: String;
  Debian: String;
  S: TFileStream;
begin
  Source := ConcatPaths([Settings.GetProjectDir, 'DEBUILD', Settings.GetOrigFolderName]);
  Debian := ConcatPaths([Source, 'debian']);
  MkDir(Debian);
  CreateFile(ConcatPaths([Debian, 'compat']), '8');
  CreateFile(ConcatPaths([Debian, 'control']), Settings.FillTemplate(Settings.Control));
  CreateFile(ConcatPaths([Debian, 'rules']), Settings.FillTemplate(Settings.Rules));
  CreateFile(ConcatPaths([Debian, 'changelog']), Settings.FillTemplate(Settings.Changelog));
  CreateFile(ConcatPaths([Debian, 'copyright']), Settings.FillTemplate(Settings.Copyright));
  {$warning FIXME: respect the setting "Makefile / Use Existing", implement this!}
  CreateFile(ConcatPaths([Source, 'Makefile']), Settings.FillTemplate(Settings.Makefile));
end;

procedure Prepare(Settings: TSettings);
begin
  CopyOrig(Settings);
  CreateDebianFolder(Settings);
end;

procedure DebuildSource(Settings: TSettings);
var
  SourceDir: String;
  Script : TStringList;
begin
  SourceDir := ConcatPaths([Settings.GetProjectDir, 'DEBUILD', Settings.GetOrigFolderName]);
  Script := TStringList.Create;
  Script.Add('xterm -e "debuild -S -us -uc"');
  RunShellCommands(SourceDir, Script);
  Script.Free;
end;

procedure DoMakeBinaryPackage(Settings: TSettings; Sign: Boolean);
begin
  Prepare(Settings);
  Settings.Free;
end;

procedure DoMakeSourcePackage(Settings: TSettings; Sign: Boolean; Upload: Boolean);
begin
  Prepare(Settings);
  DebuildSource(Settings);
  Settings.Free;
end;

end.

