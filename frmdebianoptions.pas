unit frmDebianOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynBeautifier, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls;

type

  { TFDebianOptions }

  TFDebianOptions = class(TForm)
    BtnOK: TButton;
    BtnPreviewMakefile: TButton;
    BtnResetMakefile: TButton;
    EdMakefile: TSynEdit;
    MakefileOptions: TRadioGroup;
    Tab: TPageControl;
    PageOptions: TTabSheet;
    PageControl: TTabSheet;
    PageRules: TTabSheet;
    PageChangelog: TTabSheet;
    PageCopyright: TTabSheet;
    PageMakefile: TTabSheet;
    procedure InitSynEdits;
    procedure BtnOKClick(Sender: TObject);
    procedure BtnResetMakefileClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MakefileOptionsSelectionChanged(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FDebianOptions: TFDebianOptions;

implementation
uses
  lazdebianMakefile;

const
  IDX_MAKE_USE_EXISTING = 0;
  IDX_MAKE_FROM_TEMPLATE = 1;

{$R *.lfm}

{ TFDebianOptions }


procedure TFDebianOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFDebianOptions.FormCreate(Sender: TObject);
begin
  InitSynEdits;
  MakefileOptionsSelectionChanged(nil);
end;

procedure TFDebianOptions.MakefileOptionsSelectionChanged(Sender: TObject);
begin
  case MakefileOptions.ItemIndex of
    IDX_MAKE_USE_EXISTING:
    begin
      BtnPreviewMakefile.Enabled := False;
      BtnResetMakefile.Enabled := False;
      EdMakefile.Visible := False;
    end;
    IDX_MAKE_FROM_TEMPLATE:
    begin
      BtnPreviewMakefile.Enabled := True;
      BtnResetMakefile.Enabled := True;
      EdMakefile.Enabled := True;
      EdMakefile.Visible := True;
    end;
  end;
end;

procedure TFDebianOptions.InitSynEdits;
var
  B: TSynBeautifier;

  procedure InitSynEdit(S: TSynEdit);
  begin
    S.Options := [
      eoAutoIndent,
      eoBracketHighlight,
      eoGroupUndo,
//      eoShowSpecialChars,
      eoTabIndent,
      eoTrimTrailingSpaces
    ];
    S.Options2 := [
      eoCaretSkipTab,
      eoFoldedCopyPaste,
      eoOverwriteBlock
    ];
    S.BlockIndent := 0;
    S.BlockTabIndent := 1;
    S.Beautifier := B;
  end;

begin
  B := TSynBeautifier.Create(Self);
  B.AutoIndent := True;
  B.IndentType := sbitCopySpaceTab;
  InitSynEdit(EdMakefile);
end;

procedure TFDebianOptions.BtnOKClick(Sender: TObject);
begin
  //
end;

procedure TFDebianOptions.BtnResetMakefileClick(Sender: TObject);
begin
  EdMakefile.Text := GetMakefileDefaultTemplate;
end;

end.

