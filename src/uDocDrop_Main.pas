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
  FMX.ListBox, FMX.TabControl, FMX.Edit, FMX.ScrollBox, FMX.Memo;

type
  tRuleType = (rtStartsWith, rtEndsWith, rtContains, rtEquals);
const
  cRuleType: array[tRuleType] of string = (
    'starts with...', 'ends with...', 'contains...', 'equals...');
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
    tbitmRuleCreator: TTabItem;
    btnAddRule: TButton;
    lblRuleFilename: TLabel;
    edtRuleSrc: TEdit;
    cmbxRuleSrc: TComboBox;
    memInfo: TMemo;
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
    procedure btnAddRuleClick(Sender: TObject);
  private
    { Private-Deklarationen }
    mSettings: TframeDocDropSettings;
    procedure FillRuleComboBox(const pComboBox: TComboBox);
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

procedure TFrmDocumentDropper.btnAddRuleClick(Sender: TObject);
var
  lRuleType: tRuleType;
  lRule: string;
begin
  lRuleType := tRuleType(cmbxRuleSrc.ItemIndex);
  case lRuleType of
    rtStartsWith: lRule := edtRuleSrc.Text + '*';
    rtEndsWith:   lRule := '*' + edtRuleSrc.Text;
    rtContains:   lRule := '*' + edtRuleSrc.Text + '*';
    rtEquals:      lRule := edtRuleSrc.Text;
  end;
  lstbxRules.Items.Add(lRule);
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
  OpenDialogLoad.Filter := 'Document Dropper Settings (*.dds)|*.dds';
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

          lRuleSrcDir := StringReplace(lRuleSrcDir, '%year%', IntToStr(System.SysUtils.CurrentYear), [rfIgnoreCase, rfReplaceAll]);
          lRuleDestDir := StringReplace(lRuleDestDir, '%year%', IntToStr(System.SysUtils.CurrentYear), [rfIgnoreCase, rfReplaceAll]);

          if Matchstrings(lFile, lRuleSrcDir) or
            Matchstrings(ExtractFileName(lDirectory), Copy(lRuleDestDir, 1, Pos(TPath.DirectorySeparatorChar, lRuleDest) - 1)) then
          begin
            if lRuleDest = '' then
              TFile.Move(pFile, IncludeTrailingPathDelimiter(lDirectory) + lFile)
            else
            begin
              TFile.Move(pFile, IncludeTrailingPathDelimiter(lDirectory) +
                IncludeTrailingPathDelimiter(Copy(lRuleDestDir, Pos(TPath.DirectorySeparatorChar, lRuleDestDir) + 1, Length(lRuleDestDir))) + lFile);
            end;
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
  SaveDialog.Filter := 'Document Dropper Settings (*.dds)|*.dds';
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
  OpenDialogFiles.Filter := 'All files|*.*';
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

procedure TFrmDocumentDropper.FillRuleComboBox(const pComboBox: TComboBox);
var
  lRuleType: tRuleType;
begin
  pComboBox.Items.Clear();
  for lRuleType := Low(tRuleType) to High(tRuleType) do
  begin
    pComboBox.Items.Add(cRuleType[lRuleType]);
  end;
  pComboBox.ItemIndex := 0;
end;

procedure TFrmDocumentDropper.FormCreate(Sender: TObject);
begin
  mSettings := TframeDocDropSettings.Create(Self);
  FillRuleComboBox(cmbxRuleSrc);
  
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
