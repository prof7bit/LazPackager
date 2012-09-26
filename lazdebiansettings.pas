unit lazdebiansettings;

{$mode objfpc}{$H+}

interface

const
  TAB = #9;
  LF = #10;

  DEFAULT_MAKEFILE
    = '.PHONY : all'+ LF
    + 'all:'+ LF
    + TAB + 'lazbuild ?PROJECT'+ LF
    + LF
    + '.PHONY : clean'+ LF
    + 'clean:'+ LF
    + TAB + '$(RM) -r lib'+ LF
    + TAB + '$(RM) *.res'+ LF
    + TAB + '$(RM) ?EXECUTABLE'+ LF
    + LF
    + '.PHONY : install'+ LF
    + 'install:'+ LF
    + TAB + 'mkdir -p $(DESTDIR)($PREFIX)/bin'+ LF
    + TAB + 'install ?EXECUTABLE $(DESTDIR)($PREFIX)/bin/'+ LF;

  DEFAULT_CONTROL
    = 'Source: ?PACKAGE_NAME'+ LF
    + 'Maintainer: ?AUTHOR <?EMAIL>'+ LF
    + 'Section: misc'+ LF
    + 'Priority: optional'+ LF
    + 'Standards-Version: 3.9.3'+ LF
    + 'Build-Depends: fpc, lcl, lcl-utils, lazarus, debhelper (>= 8)'+ LF
    + LF
    + 'Package: ?PACKAGE_NAME'+ LF
    + 'Architecture: any'+ LF
    + 'Depends: ${shlibs:Depends}, ${misc:Depends},'+ LF
    + 'Description: ?DESCRIPTION'+ LF
    + ' ?DESCRIPTION_LONG'+ LF;

  DEFAULT_RULES
    = '#!/usr/bin/make -f' + LF
    + LF
    + '%:' + LF
	  + TAB + 'dh $@' + LF;

  DEFAULT_CHANGELOG
    = '?PACKAGE_NAME (?FULLVERSION) ?SERIES; urgency=low' + LF
    + LF
    + '  * Upstream release.' + LF
    + LF
    + ' -- ?AUTHOR <?EMAIL>  ?DATE' + LF;

  DEFAULT_COPYRIGHT
    = 'Format: http://www.debian.org/doc/packaging-manuals/copyright-format/1.0/' + LF
    + LF
    + 'Files: *' + LF
    + 'Copyright: ?COPYRIGHT' + LF
    + 'License: ?LICENSE' + LF
    + ' ?LICENSE_DETAIL' + LF;

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
  Copyright := LoadProperty('lazdebian_copyright', DEFAULT_COPYRIGHT);
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

