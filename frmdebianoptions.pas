{ LazDebian settings dialog for package creation

  Copyright (C) 2012 Bernd Kreuss prof7bit@gmail.com

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  A copy of the GNU General Public License is available on the World Wide
  Web at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite
  330, Boston, MA 02111-1307, USA.
}

unit frmDebianOptions;

{$mode objfpc}{$H+}

interface

uses
  SynEdit, SynBeautifier, Forms, Graphics,
  ComCtrls, StdCtrls, ExtCtrls,
  lazdebianpackagerbase,
  lazdebianpackagerdeb;

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
    GrpAuthor: TGroupBox;
    GrpMaintainer: TGroupBox;
    lblCopyright: TLabel;
    lblExportCommands: TLabel;
    lblDecription: TLabel;
    lblDescriptionLong: TLabel;
    lblLicense: TLabel;
    lblMore: TLabel;
    lblPackageName: TLabel;
    lblMaintainer: TLabel;
    lblMaintainerEmail: TLabel;
    lblPPA: TLabel;
    lblSeries: TLabel;
    lblVersion: TLabel;
    lblVersionExplain: TLabel;
    MakefileOptions: TRadioGroup;
    TabCtrl: TPageControl;
    PageOptions: TTabSheet;
    PageControl: TTabSheet;
    PageRules: TTabSheet;
    PageChangelog: TTabSheet;
    PageCopyright: TTabSheet;
    PageMakefile: TTabSheet;
    txtCopyright: TEdit;
    txtExportCommands: TMemo;
    txtDescription: TEdit;
    txtDescriptionLong: TMemo;
    txtPackageName: TEdit;
    txtMaintainerEmail: TEdit;
    txtMaintainerName: TEdit;
    txtPPA: TEdit;
    txtSeries: TEdit;
    txtVersion: TEdit;
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
    procedure FormResize(Sender: TObject);
    procedure InitSynEdits;
    procedure UpdateSettings(SaveToProject: Boolean);
    procedure ShowPreview(Title, Template: String);
    procedure BtnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MakefileOptionsSelectionChanged(Sender: TObject);
  private
    Packager: TPackagerDebian;
  end;

var
  FDebianOptions: TFDebianOptions;

implementation
uses
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
  Packager := TPackagerDebian.Create;
  with Packager do begin
    txtCopyright.Text := AuthorCopyright;
    txtVersion.Text := GetOriginalProjectVersion;
    txtDescription.Text := Description;
    txtDescriptionLong.Text := DescriptionLong;
    txtMaintainerName.Text := Maintainer;
    txtMaintainerEmail.Text := MaintainerEmail;
    txtSeries.Text := Series;
    txtPackageName.Text := PackageName;
    txtExportCommands.Text := ExportCommands;
    txtPPA.Text := PPA;

    if UseExistingMakefile then
      MakefileOptions.ItemIndex := IDX_MAKE_USE_EXISTING
    else
      MakefileOptions.ItemIndex := IDX_MAKE_FROM_TEMPLATE;

    EdMakefile.Text := Makefile;
    EdControl.Text := Control;
    EdRules.Text := Rules;
    EdChangelog.Text := Changelog;
    EdCopyright.Text := Copyright;
    MakefileOptionsSelectionChanged(nil);
  end;

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

procedure TFDebianOptions.UpdateSettings(SaveToProject: Boolean);
begin
  with Packager do begin
    AuthorCopyright := txtCopyright.Text;
    Description := txtDescription.Text;
    DescriptionLong := txtDescriptionLong.Text;
    Maintainer := txtMaintainerName.Text;
    MaintainerEmail := txtMaintainerEmail.Text;
    Series := txtSeries.Text;
    PackageName := txtPackageName.Text;
    ExportCommands := txtExportCommands.Text;
    PPA := txtPPA.Text;

    if MakefileOptions.ItemIndex = IDX_MAKE_USE_EXISTING then
      UseExistingMakefile := True
    else
      UseExistingMakefile := False;

    Makefile := EdMakefile.Text;
    Control := EdControl.Text;
    Rules := EdRules.Text;
    Changelog := EdChangelog.Text;
    Copyright := EdCopyright.Text;

    if SaveToProject then
      Save;
  end;

end;

procedure TFDebianOptions.ShowPreview(Title, Template: String);
begin
  UpdateSettings(False);
  FFilePreview := TFFilePreview.Create(Self);
  FFilePreview.SetText(Title, Packager.FillTemplate(Template));
  FFilePreview.ShowModal;
end;

procedure TFDebianOptions.FormDestroy(Sender: TObject);
begin
  Packager.Free;
end;

procedure TFDebianOptions.FormResize(Sender: TObject);
var
  HalfWidth: Integer;
begin
  HalfWidth := TabCtrl.Width div 2 - 8;
  GrpAuthor.Width := HalfWidth;
  GrpMaintainer.Width := HalfWidth;
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
  UpdateSettings(True);
end;

end.

