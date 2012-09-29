unit frmdebianmakepackage;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls;

type
  TPackageType = (debBinary, debSource);

  { TFMakePackage }

  TFMakePackage = class(TForm)
    btnCancel: TButton;
    btnCreate: TButton;
    chkSign: TCheckBox;
    chkUpload: TCheckBox;
    procedure btnCreateClick(Sender: TObject);
    procedure chkSignChange(Sender: TObject);
    procedure chkUploadChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FTyp: TPackageType;
  public
    procedure SetType(Typ: TPackageType);
  end;

var
  FMakePackage: TFMakePackage;

implementation
uses
  lazdebiansettings;

{$R *.lfm}

{ TFMakePackage }

procedure TFMakePackage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFMakePackage.chkUploadChange(Sender: TObject);
begin
  if chkUpload.Checked then
    chkSign.Checked := True;
end;

procedure TFMakePackage.chkSignChange(Sender: TObject);
begin
  if not chkSign.Checked then
    chkUpload.Checked := False;
end;

procedure TFMakePackage.btnCreateClick(Sender: TObject);
var
  Settings: TSettings;
begin
  Settings := TSettings.Create;
  Settings.DoMakePackage(FTyp=debBinary, chkSign.Checked, chkUpload.Checked);
  // settings will free itself after it is done
end;

procedure TFMakePackage.SetType(Typ: TPackageType);
begin
  FTyp := Typ;
  if Typ = debBinary then begin
    Caption := 'Create .deb';
    chkUpload.Enabled := False;
  end
  else begin
    Caption := 'Create source package';
  end;
end;

end.

