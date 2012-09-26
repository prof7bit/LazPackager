unit lazdebiansettings;

{$mode objfpc}{$H+}

interface

const
  DEFAULT_MAKEFILE =
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

  DEFAULT_CONTROL = '';
  DEFAULT_RULES = '';
  DEFAULT_CHANGELOG = '';
  COPYRIGHT_DEFAULT = '';

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
    procedure SaveProperty(Key, Value: String);
    function LoadProperty(Key, DefaultValue: String): String;
  end;


implementation
uses
  LazIDEIntf;

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
  SaveProperty('lazdebian_makefile', Makefile);
  SaveProperty('lazdebian_control', Control);
  SaveProperty('lazdebian_rules', Rules);
  SaveProperty('lazdebian_changelog', Changelog);
  SaveProperty('lazdebian_copyright', Copyright);
end;

procedure TSettings.Load;
begin
  Makefile := LoadProperty('lazdebian_makefile', DEFAULT_MAKEFILE);
  Control := LoadProperty('lazdebian_control', DEFAULT_CONTROL);
  Rules := LoadProperty('lazdebian_rules', DEFAULT_RULES);
  Changelog := LoadProperty('lazdebian_changelog', DEFAULT_CHANGELOG);
  Copyright := LoadProperty('lazdebian_copyright', COPYRIGHT_DEFAULT);
end;

procedure TSettings.SaveProperty(Key, Value: String);
begin
  LazarusIDE.ActiveProject.CustomData.Values[Key] := Value;
  LazarusIDE.ActiveProject.Modified := True;
end;

function TSettings.LoadProperty(Key, DefaultValue: String): String;
begin
  Result := LazarusIDE.ActiveProject.CustomData.Values[Key];
  if Result = '' then begin
    Result := DefaultValue;
    SaveProperty(Key, Result);
  end;

end;

end.

