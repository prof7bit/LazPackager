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
  SysUtils;

procedure DoMakeBinaryPackage(Settings: TSettings; Sign: Boolean);
begin
  Settings.Free;
end;

procedure DoMakeSourcePackage(Settings: TSettings; Sign: Boolean; Upload: Boolean);
begin
  Settings.Free;
end;


end.

