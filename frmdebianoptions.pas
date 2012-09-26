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
    BtnPreviewControl: TButton;
    BtnPreviewRules: TButton;
    BtnPreviewChangelog: TButton;
    BtnPreviewCopyright: TButton;
    BtnResetControl: TButton;
    BtnResetRules: TButton;
    BtnResetChangelog: TButton;
    BtnResetCopyright: TButton;
    BtnResetMakefile: TButton;
    EdControl: TSynEdit;
    EdRules: TSynEdit;
    EdChangelog: TSynEdit;
    EdCopyright: TSynEdit;
    EdMakefile: TSynEdit;
    MakefileOptions: TRadioGroup;
    TabCtrl: TPageControl;
    PageOptions: TTabSheet;
    PageControl: TTabSheet;
    PageRules: TTabSheet;
    PageChangelog: TTabSheet;
    PageCopyright: TTabSheet;
    PageMakefile: TTabSheet;
    procedure BtnPreviewMakefileClick(Sender: TObject);
    procedure BtnPreviewControlClick(Sender: TObject);
    procedure BtnPreviewRulesClick(Sender: TObject);
    procedure BtnPreviewChangelogClick(Sender: TObject);
    procedure BtnPreviewCopyrightClick(Sender: TObject);
    procedure BtnResetMakefileClick(Sender: TObject);
    procedure BtnResetControlClick(Sender: TObject);
    procedure BtnResetRulesClick(Sender: TObject);
    procedure BtnResetChangelogClick(Sender: TObject);
    procedure BtnResetCopyrightClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InitSynEdits;
    procedure ShowPreview(Title, Txt: String);
    procedure BtnOKClick(Sender: TObject);
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
  lazdebianmain,
  lazdebianpreview;

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
  TabCtrl.ActivePage := PageOptions;
  InitSynEdits;
  Settings := TSettings.Create;
  EdMakefile.Text := Settings.Makefile;
  EdControl.Text := Settings.Control;
  EdRules.Text := Settings.Rules;
  EdChangelog.Text := Settings.Changelog;
  EdCopyright.Text := Settings.Copyright;
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
  InitSynEdit(EdControl);
  InitSynEdit(EdRules);
  InitSynEdit(EdChangelog);
  InitSynEdit(EdCopyright);
end;

procedure TFDebianOptions.ShowPreview(Title, Txt: String);
begin
  FFilePreview := TFFilePreview.Create(Self);
  FFilePreview.SetText(Title, Txt);
  FFilePreview.ShowModal;
end;

procedure TFDebianOptions.FormDestroy(Sender: TObject);
begin
  Settings.Free;
end;

procedure TFDebianOptions.BtnPreviewMakefileClick(Sender: TObject);
begin
  ShowPreview('Makefile', EdMakefile.Text);
end;

procedure TFDebianOptions.BtnPreviewControlClick(Sender: TObject);
begin
  ShowPreview('debian/control', EdControl.Text);
end;

procedure TFDebianOptions.BtnPreviewRulesClick(Sender: TObject);
begin
  ShowPreview('debian/rules', EdRules.Text);
end;

procedure TFDebianOptions.BtnPreviewChangelogClick(Sender: TObject);
begin
  ShowPreview('debian/changelog', EdChangelog.Text);
end;

procedure TFDebianOptions.BtnPreviewCopyrightClick(Sender: TObject);
begin
  ShowPreview('debian/copyright', EdCopyright.Text);
end;

procedure TFDebianOptions.BtnResetMakefileClick(Sender: TObject);
begin
  EdMakefile.Text := DEFAULT_MAKEFILE;
end;

procedure TFDebianOptions.BtnResetControlClick(Sender: TObject);
begin
  EdControl.Text := DEFAULT_CONTROL;
end;

procedure TFDebianOptions.BtnResetRulesClick(Sender: TObject);
begin
  EdRules.Text := DEFAULT_RULES;
end;

procedure TFDebianOptions.BtnResetChangelogClick(Sender: TObject);
begin
  EdChangelog.Text := DEFAULT_CHANGELOG;
end;

procedure TFDebianOptions.BtnResetCopyrightClick(Sender: TObject);
begin
  EdCopyright.Text := DEFAULT_COPYRIGHT;
end;

procedure TFDebianOptions.BtnOKClick(Sender: TObject);
begin
  Settings.Makefile := EdMakefile.Text;
  Settings.Control := EdControl.Text;
  Settings.Rules := EdRules.Text;
  Settings.Changelog := EdChangelog.Text;
  Settings.Copyright := EdCopyright.Text;
  Settings.Save;
end;

end.

