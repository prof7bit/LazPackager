unit lazdebianmain;

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


procedure Register;
function LoadMakefileTemplate: String;
procedure SaveMakefileTemplate(Makefile: String);

implementation
uses
  Forms,
  MenuIntf,
  frmDebianOptions;

procedure OpenConfigDialog(Sender: TObject);
begin
  FDebianOptions := TFDebianOptions.Create(Application.MainForm);
  FDebianOptions.ShowModal;
end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmProjectWindowSection, 'debianopts','Debian Options ...', nil, @OpenConfigDialog);
  {$IFDEF LINUX}
  RegisterIDEMenuCommand(itmProjectSaveSection, 'debianmakesource','Publish Debian Source Package ...', nil, @OpenConfigDialog);
  RegisterIDEMenuCommand(itmRunBuilding, 'debianmakebin','Build Debian Binary Package ...', nil, @OpenConfigDialog);
  {$ENDIF}
end;

function LoadMakefileTemplate: String;
begin

end;

procedure SaveMakefileTemplate(Makefile: String);
begin

end;

end.

