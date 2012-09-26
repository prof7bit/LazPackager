unit lazdebiansettings;

{$mode objfpc}{$H+}

interface

const
  MAKEFILE_DEFAULT =
    '.PHONY : all'#10 +
    'all:'#10 +
    #9'lazbuild ?PROJECT'#10 +
    #10 +
    '.PHONY : clean'#10 +
    'clean:'#10 +
    #9'$(RM) -r lib'#10 +
    #9'$(RM) *.res'#10 +
    #9'$(RM) ?EXECUTABLE'#10 +
    #10 +
    '.PHONY : install'#10 +
    'install:'#10 +
    #9'mkdir -p $(DESTDIR)($PREFIX)/bin'#10 +
    #9'install ?EXECUTABLE $(DESTDIR)($PREFIX)/bin/'#10 +
    #10;

type
  { TSettings }

  TSettings = class
    Makefile: String;
    Control: String;
    Rules: String;
    Changelog: String;
    Copyright: String;
    constructor Create;
    destructor Destroy; override;
    procedure Save;
    procedure Load;
  end;


implementation
uses
  sysutils,
  LCLProc,
  BaseIDEIntf,
  LazConfigStorage;

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
const
  Version = 1;
var
  Config: TConfigStorage;
begin
  try
    Config:=GetIDEConfigStorage('/home/bernd/Desktop/lazdebian.xml', false);
    try
      Config.SetDeleteValue('Version', Version, 0);
      Config.SetDeleteValue('templates/Makefile', Makefile, MAKEFILE_DEFAULT);
      Config.SetDeleteValue('templates/debian/control', Control, MAKEFILE_DEFAULT);
      Config.SetDeleteValue('templates/debian/rules', Rules, MAKEFILE_DEFAULT);
      Config.SetDeleteValue('templates/debian/changelog', Changelog, MAKEFILE_DEFAULT);
      Config.SetDeleteValue('templates/debian/copyright', Copyright, MAKEFILE_DEFAULT);
    finally
      Config.Free;
    end;

  except
    on E: Exception do begin
      DebugLn(['Saving mysettings.xml failed: ',E.Message]);
    end;
  end;
end;

procedure TSettings.Load;
begin

end;

end.

