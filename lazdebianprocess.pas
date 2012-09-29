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
  LCLType,
  LazIDEIntf,
  IDEExternToolIntf;



procedure RunBuildScript(Settings: TSettings);
begin
  Application.QueueAsyncCall(@Settings.RunBuildScript, 0);
end;

procedure WarnStillRunning;
begin
  Application.MessageBox('LazDebian still running', 'LazDebian', MB_OK + MB_ICONWARNING);
end;

procedure StartMakeBinaryPackage(Settings: TSettings; Sign: Boolean);
begin
  Settings.CreateDebianFiles;
  Settings.CreateBuildScript(True, Sign, False);
  RunBuildScript(Settings);
end;

procedure StartMakeSourcePackage(Settings: TSettings; Sign: Boolean; Upload: Boolean);
begin
  Settings.CreateDebianFiles;
  Settings.CreateBuildScript(False, Sign, Upload);
  RunBuildScript(Settings);
end;

end.

