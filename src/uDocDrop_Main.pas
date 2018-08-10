unit uDocDrop_Main;

interface

uses
  uDocDrop_Settings,
  System.IniFiles,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListView, FMX.Layouts,
  FMX.ListBox, FMX.TabControl;

type
  TFrmDocumentDropper = class(TForm)
    OpenDialogFiles: TOpenDialog;
    tbctrlMain: TTabControl;
    tbitmSelectSources: TTabItem;
    lstbxSources: TListBox;
    btnSrcAddFiles: TButton;
    tbitmSelectDestinations: TTabItem;
    btnSrcAddDir: TButton;
    lstbxDestinations: TListBox;
    btnDestAddDir: TButton;
    tbitmCommon: TTabItem;
    btnSaveSelections: TButton;
    SaveDialog: TSaveDialog;
    OpenDialogLoad: TOpenDialog;
    btnLoadSelection: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSrcAddFilesClick(Sender: TObject);
    procedure lstbxSourcesKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure btnSrcAddDirClick(Sender: TObject);
    procedure btnDestAddDirClick(Sender: TObject);
    procedure btnSaveSelectionsClick(Sender: TObject);
    procedure lstbxDestinationsKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure btnLoadSelectionClick(Sender: TObject);
  private
    { Private-Deklarationen }
    mSettings: TframeDocDropSettings;
    function GetListBoxToString(const pListBox: TListBox): string;
    procedure FillListBox(const pListBox: TListBox; const pContent: string);
    procedure Split(const pDelimiter: Char; const pText: string;
      const pStrings: TStrings) ;
  public
    { Public-Deklarationen }
  end;

var
  FrmDocumentDropper: TFrmDocumentDropper;

implementation

{$R *.fmx}

procedure TFrmDocumentDropper.btnDestAddDirClick(Sender: TObject);
var
  lDir: string;
begin
  SelectDirectory('Select directory', '', lDir);
  if lDir <> '' then
    lstbxDestinations.Items.Add(lDir);
end;

procedure TFrmDocumentDropper.btnLoadSelectionClick(Sender: TObject);
var
  lIniFile: TIniFile;
begin
  if OpenDialogLoad.Execute() then
  begin
    lIniFile := TIniFile.Create(OpenDialogLoad.FileName);
    try
      FillListBox(lstbxSources, lIniFile.ReadString('Sources', 'Pathes', ''));
      FillListBox(lstbxDestinations, lIniFile.ReadString('Destinations', 'Pathes', ''));
    finally
      lIniFile.Free();
    end;
  end;
end;

procedure TFrmDocumentDropper.btnSaveSelectionsClick(Sender: TObject);
var
  lIniFile: TIniFile;
begin
  if SaveDialog.Execute() then
  begin
    lIniFile := TIniFile.Create(SaveDialog.FileName);
    try
      lIniFile.WriteString('Sources', 'Pathes', GetListBoxToString(lstbxSources));
      lIniFile.WriteString('Destinations', 'Pathes', GetListBoxToString(lstbxDestinations));
    finally
      lIniFile.Free();
    end;
  end;
end;

procedure TFrmDocumentDropper.btnSrcAddDirClick(Sender: TObject);
var
  lDir: string;
begin
  SelectDirectory('Select directory', '', lDir);
  if lDir <> '' then
    lstbxSources.Items.Add(lDir);
end;

procedure TFrmDocumentDropper.btnSrcAddFilesClick(Sender: TObject);
var
  lFile: string;
begin
  if OpenDialogFiles.Execute() then
  begin
    for lFile in OpenDialogFiles.Files do
      lstbxSources.Items.Add(lFile);
  end;
end;

procedure TFrmDocumentDropper.FillListBox(const pListBox: TListBox;
  const pContent: string);
begin
  Split(';', pContent, pListBox.Items);
end;

procedure TFrmDocumentDropper.FormCreate(Sender: TObject);
begin
  mSettings := TframeDocDropSettings.Create(Self);
  //mSettings.Parent := Self;
  //mSettings.Align := TAlignLayout.Client;
end;

procedure TFrmDocumentDropper.FormDestroy(Sender: TObject);
begin
  mSettings.Free();
end;

function TFrmDocumentDropper.GetListBoxToString(
  const pListBox: TListBox): string;
var
  i: Integer;
begin
  for i := 0 to pListBox.Items.Count - 1 do
  begin
    if i > 0 then
      Result := Result + ';' + pListBox.Items[i]
    else
      Result := pListBox.Items[i];
  end;
end;

procedure TFrmDocumentDropper.lstbxDestinationsKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = Ord(#8) then
    if lstbxDestinations.ItemIndex <> -1 then
      lstbxDestinations.Items.Delete(lstbxDestinations.ItemIndex);
end;

procedure TFrmDocumentDropper.lstbxSourcesKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = Ord(#8) then
    if lstbxSources.ItemIndex <> -1 then
      lstbxSources.Items.Delete(lstbxSources.ItemIndex);
end;

procedure TFrmDocumentDropper.Split(const pDelimiter: Char; const pText: string;
  const pStrings: TStrings);
begin
  pStrings.Clear;
  pStrings.Delimiter       := pDelimiter;
  pStrings.StrictDelimiter := True;
  pStrings.DelimitedText   := pText;
end;

end.
