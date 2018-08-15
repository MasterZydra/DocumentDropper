unit uDocDrop_Main;

interface

uses
  uDocDrop_Settings,
  uDocDrop_Utils,
  System.IniFiles, System.IOUtils, System.StrUtils,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListView, FMX.Layouts,
  FMX.ListBox, FMX.TabControl, FMX.Edit, FMX.ScrollBox, FMX.Memo,
  System.ImageList, FMX.ImgList;

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
    SaveDialog: TSaveDialog;
    OpenDialogLoad: TOpenDialog;
    tbitmRules: TTabItem;
    tbitmRuleCreator: TTabItem;
    btnAddRule: TButton;
    ImageList: TImageList;
    memLog: TMemo;
    pnlBtns: TPanel;
    btnLoadSelection: TButton;
    btnSaveSelections: TButton;
    btnProcess: TButton;
    lstbxRules: TListBox;
    pnlRule: TPanel;
    lblRuleFilename: TLabel;
    cmbxRuleSrc: TComboBox;
    edtRuleSrc: TEdit;
    chkbxMoveTo: TCheckBox;
    edtRuleDest: TEdit;
    memInfo: TMemo;
    btnDeleteRule: TButton;
    btnUpdateRule: TButton;
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
    procedure lstbxRulesKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure btnAddRuleClick(Sender: TObject);
    procedure btnDeleteRuleClick(Sender: TObject);
    procedure btnUpdateRuleClick(Sender: TObject);
    procedure lstbxRulesItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
  private
    { Private-Deklarationen }
    mSettings: TframeDocDropSettings;
    mRules: TStringList;
    procedure FillRuleComboBox(const pComboBox: TComboBox);
    procedure AddRule(const pRule: string);
    procedure DeleteRule(const pIndex: Integer);
    procedure UpdateRuleView;
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

procedure TFrmDocumentDropper.AddRule(const pRule: string);
begin
  mRules.Add(pRule);
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
    rtEquals:     lRule := edtRuleSrc.Text;
  end;
  if chkbxMoveTo.IsChecked then
  begin
    lRule := lRule + ' -> ' + edtRuleDest.Text;
  end;
  AddRule(lRule);
  UpdateRuleView();
end;

procedure TFrmDocumentDropper.btnDeleteRuleClick(Sender: TObject);
begin
  DeleteRule(lstbxRules.ItemIndex div 5);
  UpdateRuleView();
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
      Split(';', lIniFile.ReadString('Rules', 'Rules', ''), mRules);
    finally
      lIniFile.Free();
    end;
    UpdateRuleView();
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
    lSubFolder: string;
    lDestPath: string;
    lFolder: string;
    lFolders: TStringList;
    lTempPath: string;
  begin
    for lRule in lstbxRules.Items do
    begin
      for lDest in lstbxDestinations.Items do
      begin
        lPos := Pos('->', lRule);
        lLength := Length('->');
        lDirectories := TDirectory.GetDirectories(lDest);

        if lPos = 0 then
        begin
          lRuleSrc := Trim(lRule);
          lRuleDest := '';
        end
        else
        begin
          lRuleSrc := Trim(Copy(lRule, 1, lPos - 1));
          lRuleDest := Trim(Copy(lRule, lPos + lLength, Length(lRule) - lPos));
        end;

        for lDirectory in lDirectories do
        begin
          lFile := ExtractFileName(pFile);
          lRuleSrcDir := StringReplace(lRuleSrc, '%dest%', ExtractFileName(lDirectory), [rfIgnoreCase, rfReplaceAll]);
          lRuleDestDir := StringReplace(lRuleDest, '%dest%', ExtractFileName(lDirectory), [rfIgnoreCase, rfReplaceAll]);

          lRuleSrcDir := StringReplace(lRuleSrcDir, '%year%', IntToStr(System.SysUtils.CurrentYear), [rfIgnoreCase, rfReplaceAll]);
          lRuleDestDir := StringReplace(lRuleDestDir, '%year%', IntToStr(System.SysUtils.CurrentYear), [rfIgnoreCase, rfReplaceAll]);

          if Matchstrings(lFile, lRuleSrcDir) and
            ((lRuleDestDir = '') or
            (lRuleDestDir <> '') and
            Matchstrings(ExtractFileName(lDirectory), Copy(lRuleDestDir, 1,
              Pos(TPath.DirectorySeparatorChar, lRuleDest) - 1))) then
          begin
            if lRuleDest = '' then
              TFile.Move(pFile, IncludeTrailingPathDelimiter(lDirectory) + lFile)
            else
            begin
              lSubFolder := IncludeTrailingPathDelimiter(Copy(lRuleDestDir, Pos(TPath.DirectorySeparatorChar, lRuleDestDir) + 1, Length(lRuleDestDir)));
              lDestPath := IncludeTrailingPathDelimiter(lDirectory) + lSubFolder;

              // Split -> For loop -> level down and create folders
              lFolders := TStringList.Create();
              try
                Split(TPath.DirectorySeparatorChar, lSubFolder, lFolders);
                lTempPath := '';
                for lFolder in lFolders do
                begin
                  if lFolder = '' then
                    Continue;
                  lTempPath := IncludeTrailingPathDelimiter(lTempPath) + lFolder;
                  if not DirectoryExists(IncludeTrailingPathDelimiter(lDirectory) + lTempPath) then
                    if not CreateDir(IncludeTrailingPathDelimiter(lDirectory) + lTempPath) then
                      ShowMessage('Subfolder could not be created!');
                end;
              finally
                lFolders.Free;
              end;
              TFile.Move(pFile, lDestPath + lFile);
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
      lIniFile.WriteString('Sources', 'Pathes', GetStringFromStrings(lstbxSources.Items));
      lIniFile.WriteString('Destinations', 'Pathes', GetStringFromStrings(lstbxDestinations.Items));
      lIniFile.WriteString('Rules', 'Rules', GetStringFromStrings(TStrings(lstbxRules)));
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

procedure TFrmDocumentDropper.btnUpdateRuleClick(Sender: TObject);
begin
  DeleteRule(lstbxRules.ItemIndex div 5);
  btnAddRuleClick(Sender);
end;

procedure TFrmDocumentDropper.DeleteRule(const pIndex: Integer);
begin
  mRules.Delete(pIndex);
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
  mRules := TStringList.Create();
  //mSettings.Parent := Self;
  //mSettings.Align := TAlignLayout.Client;
end;

procedure TFrmDocumentDropper.FormDestroy(Sender: TObject);
begin
  mSettings.Free();
  mRules.Free();
end;

procedure TFrmDocumentDropper.lstbxDestinationsKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = Ord(#8) then
    if lstbxDestinations.ItemIndex <> -1 then
      lstbxDestinations.Items.Delete(lstbxDestinations.ItemIndex);
end;

procedure TFrmDocumentDropper.lstbxRulesItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var
  s: string;
  lPos: Integer;
begin
  s := mRules[lstbxRules.ItemIndex div 5];
  lPos := Pos('->', s);
  if lPos = 0 then
  begin
    edtRuleSrc.Text := Trim(s);
    edtRuleDest.Text := '';
  end
  else
  begin
    edtRuleSrc.Text := Trim(Copy(s, 1, lPos - 1));
    edtRuleDest.Text := Trim(Copy(s, lPos + 2, Length(s) - lPos));
  end;

  chkbxMoveTo.IsChecked := edtRuleDest.Text <> '';

  // starts/ends with, contains, equals...
  if StartsStr('*', edtRuleSrc.Text) then
  begin
    if EndsStr('*', edtRuleSrc.Text) then
      cmbxRuleSrc.ItemIndex :=  Ord(tRuleType.rtContains)
    else
      cmbxRuleSrc.ItemIndex :=  Ord(tRuleType.rtEndsWith);
  end
  else if EndsText('*', edtRuleSrc.Text) then
    cmbxRuleSrc.ItemIndex :=  Ord(tRuleType.rtStartsWith)
  else
    cmbxRuleSrc.ItemIndex :=  Ord(tRuleType.rtEquals);

  edtRuleSrc.Text := StringReplace(edtRuleSrc.Text, '*', '', [rfReplaceAll]);
end;

procedure TFrmDocumentDropper.lstbxRulesKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = Ord(#8) then
  begin
    if lstbxRules.ItemIndex <> -1 then
    begin
      DeleteRule(lstbxRules.ItemIndex div 5);
      UpdateRuleView();
    end;
  end;
end;

procedure TFrmDocumentDropper.lstbxSourcesKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = Ord(#8) then
    if lstbxSources.ItemIndex <> -1 then
      lstbxSources.Items.Delete(lstbxSources.ItemIndex);
end;

procedure TFrmDocumentDropper.UpdateRuleView;
var
  s: string;
  lRule: TStringList;
  lPos: Integer;
  lSrc: string;
  lDest: string;
begin
  lstbxRules.Clear();
  for s in mRules do
  begin
    lRule := TStringList.Create();
    try
      lPos := Pos('->', s);
      if lPos = 0 then
      begin
        lSrc := Trim(s);
        lDest := '';
      end
      else
      begin
        lSrc := Trim(Copy(s, 1, lPos - 1));
        lDest := Trim(Copy(s, lPos + 2, Length(s) - lPos));
      end;

      if lDest <> '' then
      begin
        lRule.Add('Move file to');
        lRule.Add('"' + lDest + '"');
      end
      else
      begin
        lRule.Add('Move file');
        lRule.Add('');
      end;
      lRule.Add('if file name');

      // starts/ends with, contains, equals...
      if StartsStr('*', lSrc) then
      begin
        if EndsStr('*', lSrc) then
          lRule.Add(cRuleType[tRuleType.rtContains])
        else
          lRule.Add(cRuleType[tRuleType.rtEndsWith]);
      end
      else if EndsText('*', lSrc) then
        lRule.Add(cRuleType[tRuleType.rtStartsWith])
      else
        lRule.Add(cRuleType[tRuleType.rtEquals]);

      lRule.Add('"' + StringReplace(lSrc, '*', '', [rfReplaceAll]) + '"');

      lstbxRules.Items.AddStrings(lRule);
    finally
      lRule.Free();
    end;
  end;
end;

end.
