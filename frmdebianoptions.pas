unit frmDebianOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynBeautifier, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, lazdebiansettings;

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
    procedure FormDestroy(Sender: TObject);
    procedure InitSynEdits;
    procedure BtnOKClick(Sender: TObject);
    procedure BtnResetMakefileClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MakefileOptionsSelectionChanged(Sender: TObject);
  private
    Settings: TSettings;
  end;

var
  FDebianOptions: TFDebianOptions;

implementation
uses
  lazdebianmain;

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
  Settings := TSettings.Create;
  EdMakefile.Text := Settings.Makefile;
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
    S.Font.Quality := fqCleartype;
  end;

begin
  B := TSynBeautifier.Create(Self);
  B.AutoIndent := True;
  B.IndentType := sbitCopySpaceTab;
  InitSynEdit(EdMakefile);
end;

procedure TFDebianOptions.FormDestroy(Sender: TObject);
begin
  Settings.Free;
end;

procedure TFDebianOptions.BtnOKClick(Sender: TObject);
begin
  Settings.Makefile := EdMakefile.Text;
  Settings.Save;
end;

procedure TFDebianOptions.BtnResetMakefileClick(Sender: TObject);
begin
  EdMakefile.Text := DEFAULT_MAKEFILE;
end;

end.

