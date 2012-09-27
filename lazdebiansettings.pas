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
    + 'cp *.ico ?TEMPFOLDER?' + LF;

  DEFAULT_MAKEFILE
    = '.PHONY : all'+ LF
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
    + TAB + 'mkdir -p $(DESTDIR)($PREFIX)/bin'+ LF
    + TAB + 'install ?EXECUTABLE? $(DESTDIR)($PREFIX)/bin/'+ LF;

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
    + ' ?DESCRIPTION_LONG?'+ LF;

  DEFAULT_RULES
    = '#!/usr/bin/make -f' + LF
    + LF
    + '%:' + LF
	  + TAB + 'dh $@' + LF;

  DEFAULT_CHANGELOG
    = '?PACKAGE_NAME? (?FULLVERSION?) ?SERIES?; urgency=low' + LF
    + LF
    + '  * Original version ?VERSION? packaged with lazdebian' + LF
    + LF
    + ' -- ?MAINTAINER? <?MAINTAINER_EMAIL?>  ?DATE?' + LF;

  DEFAULT_COPYRIGHT
    = 'Format: http://www.debian.org/doc/packaging-manuals/copyright-format/1.0/' + LF
    + LF
    + 'Files: *' + LF
    + 'Copyright: ?COPYRIGHT?' + LF
    + 'License: ?LICENSE?' + LF
    + ' ?LICENSE_LONG?' + LF;

type
  { TSettings }

  TSettings = class
    AuthorCopyright: String;
    License: String;
    LicenseLong: String;
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
    procedure SaveValue(Key, Value: String);
    function LoadValue(Key, DefaultValue: String): String;
    function FillTemplate(Template: String): String;
  end;


implementation
uses
  sysutils,
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
  SaveValue('lazdebian_copyright', AuthorCopyright);
  SaveValue('lazdebian_license', License);
  SaveValue('lazdebian_license_long', LicenseLong);
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
  License := LoadValue('lazdebian_license', 'GPL-2');
  LicenseLong := LoadValue('lazdebian_license_long', '/usr/share/common-licenses/GPL-2');
  Maintainer := LoadValue('lazdebian_maintainer', 'John Doe');
  MaintainerEmail := LoadValue('lazdebian_maintainer_email', 'john_doe@example.invalid');
  Series := LoadValue('lazdebian_series', 'precise');
  PackageName := LoadValue('lazdebian_package_name', 'some-name');
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

function TSettings.LoadValue(Key, DefaultValue: String): String;
begin
  Result := LazarusIDE.ActiveProject.CustomData.Values[Key];
  if Result = '' then begin
    Result := DefaultValue;
    SaveValue(Key, Result);
  end;
end;

function TSettings.FillTemplate(Template: String): String;
begin
  Result := StringReplace(StringReplace(StringReplace(StringReplace(
            StringReplace(StringReplace(StringReplace(Template,
            '?COPYRIGHT?',        AuthorCopyright,  [rfReplaceAll]),
            '?LICENSE?',          License,          [rfReplaceAll]),
            '?LICENSE_LONG?',     LicenseLong,      [rfReplaceAll]),
            '?MAINTAINER?',       Maintainer,       [rfReplaceAll]),
            '?MAINTAINER_EMAIL?', MaintainerEmail,  [rfReplaceAll]),
            '?SERIES?',           Series,           [rfReplaceAll]),
            '?PACKAGE_NAME?',     PackageName,      [rfReplaceAll]);

  // version
  // fullversion
  // date
  // description
  // description_long
  // executable
  // project
  // tempfolder
end;

end.

