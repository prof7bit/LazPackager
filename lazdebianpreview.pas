unit lazdebianpreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs;

type

  { TFFilePreview }

  TFFilePreview = class(TForm)
    EdPreview: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetText(Title, Txt: String);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FFilePreview: TFFilePreview;

implementation

{$R *.lfm}

{ TFFilePreview }

procedure TFFilePreview.SetText(Title, Txt: String);
begin
  Caption := Format('Preview of %s', [Title]);
  EdPreview.Text := Txt;
end;

procedure TFFilePreview.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.

