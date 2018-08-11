unit uDocDrop_Main;

interface

uses
  uDocDrop_Settings,
  uDocDrop_Utils,
  System.IniFiles, System.IOUtils,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListView, FMX.Layouts,
  FMX.ListBox, FMX.TabControl, FMX.Edit;

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
    btnProcess: TButton;
    tbitmRules: TTabItem;
    lstbxRules: TListBox;
    edtRule: TEdit;
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
    procedure btnProcessClick(Sender: TObject);
    procedure edtRuleKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure lstbxRulesKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
  private
    { Private-Deklarationen }
    mSettings: TframeDocDropSettings;
  public
    { Public-Deklarationen }
  end;

var
  FrmDocumentDropper: TFrmDocumentDropper;

implementation

{$R *.fmx}
function Matchstrings(Source, pattern: string): Boolean;
var
  pSource: array [0..255] of Char;
  pPattern: array [0..255] of Char;

  function MatchPattern(element, pattern: PChar): Boolean;

    function IsPatternWild(pattern: PChar): Boolean;
    begin
      Result := StrScan(pattern, '*') <> nil;
      if not Result then Result := StrScan(pattern, '?') <> nil;
    end;
  begin
    if 0 = StrComp(pattern, '*') then
      Result := True
    else if (element^ = Chr(0)) and (pattern^ <> Chr(0)) then
      Result := False
    else if element^ = Chr(0) then
      Result := True
    else
    begin
      case pattern^ of
        '*': if MatchPattern(element, @pattern[1]) then
            Result := True
          else
            Result := MatchPattern(@element[1], pattern);
          '?': Result := MatchPattern(@element[1], @pattern[1]);
        else
          if element^ = pattern^ then
            Result := MatchPattern(@element[1], @pattern[1])
          else
            Result := False;
      end;
    end;
  end;
begin
  StrPCopy(pSource, Source);
  StrPCopy(pPattern, pattern);
  Result := MatchPattern(pSource, pPattern);
end;

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
      FillListBox(lstbxRules, lIniFile.ReadString('Rules', 'Rules', ''));
    finally
      lIniFile.Free();
    end;
  end;
end;

procedure TFrmDocumentDropper.btnProcessClick(Sender: TObject);
var
  lSource: string;
  lFiles: TStringDynArray;
  lFile: string;
  procedure CheckRules(const pFile: string);
  var
    lFile: string;
    lRule: string;
    lDest: string;
    lRuleSrc: string;
    lRuleDest: string;
    lRuleSrcDir: string;
    lRuleDestDir: string;
    lPos: Integer;
    lLength: Integer;
    lDirectories: TStringDynArray;
    lDirectory: string;
  begin
    for lRule in lstbxRules.Items do
    begin
      for lDest in lstbxDestinations.Items do
      begin
        lPos := Pos('->', lRule);
        lLength := Length('->');
        lDirectories := TDirectory.GetDirectories(lDest);

        if lPos = 0 then
          lRuleSrc := Trim(lRule)
        else
          lRuleSrc := Trim(Copy(lRule, 1, lPos - 1));
        if lPos <> 0 then
          lRuleDest := Trim(Copy(lRule, lPos + lLength, Length(lRule) - lPos))
        else
          lRuleDest := '';

        for lDirectory in lDirectories do
        begin
          lFile := ExtractFileName(pFile);
          lRuleSrcDir := StringReplace(lRuleSrc, '%dest%', ExtractFileName(lDirectory), [rfIgnoreCase, rfReplaceAll]);
          lRuleDestDir := StringReplace(lRuleDest, '%dest%', ExtractFileName(lDirectory), [rfIgnoreCase, rfReplaceAll]);

          if Matchstrings(lFile, lRuleSrcDir) then
          begin
            TFile.Move(pFile, IncludeTrailingPathDelimiter(lDirectory) + lFile);
            Exit();
          end;
        end;
      end;
    end;
  end;
begin
  for lSource in lstbxSources.Items do
  begin
    if ExtractFileExt(lSource) = '' then
    begin
      // Directory
      lFiles := TDirectory.GetFiles(lSource);
      for lFile in lFiles do
      begin
        CheckRules(lFile);
      end;
    end
    else
    begin
      // File
      CheckRules(lSource);
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
      lIniFile.WriteString('Rules', 'Rules', GetListBoxToString(lstbxRules));
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

procedure TFrmDocumentDropper.edtRuleKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = Ord(#13) then
  begin
    lstbxRules.Items.Add(edtRule.Text);
    edtRule.Text := '';
  end;
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

procedure TFrmDocumentDropper.lstbxDestinationsKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = Ord(#8) then
    if lstbxDestinations.ItemIndex <> -1 then
      lstbxDestinations.Items.Delete(lstbxDestinations.ItemIndex);
end;

procedure TFrmDocumentDropper.lstbxRulesKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = Ord(#8) then
    if lstbxRules.ItemIndex <> -1 then
      lstbxRules.Items.Delete(lstbxRules.ItemIndex);
end;

procedure TFrmDocumentDropper.lstbxSourcesKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = Ord(#8) then
    if lstbxSources.ItemIndex <> -1 then
      lstbxSources.Items.Delete(lstbxSources.ItemIndex);
end;

end.
