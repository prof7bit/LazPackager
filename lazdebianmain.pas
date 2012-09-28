unit lazdebianmain;

{$mode objfpc}{$H+}

interface

procedure Register;

implementation
uses
  Forms,
  MenuIntf,
  frmDebianOptions,
  frmdebianmakepackage;

procedure OpenConfigDialog(Sender: TObject);
begin
  FDebianOptions := TFDebianOptions.Create(Application.MainForm);
  FDebianOptions.ShowModal;
end;

procedure MakeBinaryPackage(Sender: TObject);
begin
  FMakePackage := TFMakePackage.Create(Application.MainForm);
  FMakePackage.SetType(debBinary);
  FMakePackage.ShowModal;
end;

procedure MakeSourcePackage(Sender: TObject);
begin
  FMakePackage := TFMakePackage.Create(Application.MainForm);
  FMakePackage.SetType(debSource);
  FMakePackage.ShowModal;
end;


procedure Register;
begin
  RegisterIDEMenuCommand(itmProjectWindowSection, 'debianopts','Debian Options ...', nil, @OpenConfigDialog);
  {$IFDEF LINUX}
  RegisterIDEMenuCommand(itmProjectSaveSection, 'debianmakesource','Publish Debian Source Package ...', nil, @MakeSourcePackage);
  RegisterIDEMenuCommand(itmRunBuilding, 'debianmakebin','Build Debian Binary Package ...', nil, @MakeBinaryPackage);
  {$ENDIF}
end;


end.

