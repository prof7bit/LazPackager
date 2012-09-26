unit lazdebianmain;

{$mode objfpc}{$H+}

interface

procedure Register;

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


end.

