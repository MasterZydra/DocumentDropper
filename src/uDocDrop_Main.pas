unit uDocDrop_Main;

interface

uses
  uDocDrop_Utils,
  uRuleProcessor,
  System.IniFiles, System.IOUtils, System.StrUtils, System.DateUtils,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListView, FMX.Layouts,
  FMX.ListBox, FMX.TabControl, FMX.Edit, FMX.ScrollBox, FMX.Memo,
  System.ImageList, FMX.ImgList, System.Generics.Defaults;

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
    tbitmRule: TTabItem;
    ImageList: TImageList;
    pnlBtns: TPanel;
    btnLoadSelection: TButton;
    btnSaveSelections: TButton;
    btnProcess: TButton;
    lstbxRules: TListBox;
    LayoutRuleSource: TLayout;
    lblFileName: TLabel;
    cmbxRuleSrc: TComboBox;
    edtRuleSrc: TEdit;
    LayoutRuleDestination: TLayout;
    chkbxMoveTo: TCheckBox;
    edtRuleDest: TEdit;
    LayoutButtons: TLayout;
    btnAddRule: TButton;
    btnDeleteRule: TButton;
    btnUpdateRule: TButton;
    LayoutFilterSort: TLayout;
    lblFilter: TLabel;
    edtRuleFilter: TEdit;
    cmbxRuleFilter: TComboBox;
    lblSortBy: TLabel;
    cmbxSort: TComboBox;
    LayoutRule: TLayout;
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
    procedure lstbxRulesChange(Sender: TObject);
    procedure btnRuleFilterClick(Sender: TObject);
    procedure edtRuleFilterKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure cmbxSortChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
    mRules: tRuleList;
    procedure FillRuleComboBox(const pComboBox: TComboBox);
    procedure UpdateRuleView;
    function GuiToRuleString: string;
  public
    { Public-Deklarationen }
  end;

var
  FrmDocumentDropper: TFrmDocumentDropper;

implementation

const
  cDestinations = 'Destinations';
  cSources = 'Sources';
  cPathes = 'Pathes';
  cRules = 'Rules';

{$R *.fmx}

procedure TFrmDocumentDropper.btnAddRuleClick(Sender: TObject);
begin
  mRules.Add(tRule.Create(GuiToRuleString));
  UpdateRuleView();
end;

procedure TFrmDocumentDropper.btnDeleteRuleClick(Sender: TObject);
begin
  mRules.Delete(lstbxRules.ItemIndex);
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
      FillListBox(lstbxSources, lIniFile.ReadString(cSources, cPathes, ''));
      FillListBox(lstbxDestinations, lIniFile.ReadString(cDestinations, cPathes, ''));
      mRules.FillList(lIniFile.ReadString(cRules, cRules, ''));
    finally
      lIniFile.Free();
    end;
    UpdateRuleView();
  end;
end;

procedure TFrmDocumentDropper.btnProcessClick(Sender: TObject);
{TODO -oHeinD -cGeneral : ActionItem}
var
  lSource: string;
  lFiles: TStringDynArray;
  lFile: string;
  procedure CheckRules(const pFile: string);
  var
    lRule: tRule;
    lFile: string;
    lDest: string;
    lRuleSrcDir: string;
    lRuleDestDir: string;
    lDirectories: TStringDynArray;
    lDirectory: string;
    lSubFolder: string;
    lDestPath: string;
    lFolder: string;
    lFolders: TStringList;
    lTempPath: string;
  begin
    for lRule in mRules do
    begin
      for lDest in lstbxDestinations.Items do
      begin
        lDirectories := TDirectory.GetDirectories(lDest);

        for lDirectory in lDirectories do
        begin
          lFile := ExtractFileName(pFile);
          lRuleSrcDir := StringReplace(lRule.SourceFilter, '%dest%',
            ExtractFileName(lDirectory), [rfIgnoreCase, rfReplaceAll]);
          lRuleDestDir := StringReplace(lRule.Destination, '%dest%',
            ExtractFileName(lDirectory), [rfIgnoreCase, rfReplaceAll]);

          lRuleSrcDir := ReplaceVariables(lRuleSrcDir);
          lRuleDestDir := ReplaceVariables(lRuleDestDir);

          if Matchstrings(lFile, lRuleSrcDir) and
            ((lRuleDestDir = '') or
            (lRuleDestDir <> '') and
            Matchstrings(ExtractFileName(lDirectory), Copy(lRuleDestDir, 1,
              Pos(TPath.DirectorySeparatorChar, lRule.Destination) - 1))) then
          begin
            if lRule.Destination = '' then
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
        CheckRules(lFile);
    end
    else
    begin
      // File
      CheckRules(lSource);
    end;
  end;
end;

procedure TFrmDocumentDropper.btnRuleFilterClick(Sender: TObject);
begin
  UpdateRuleView();
end;

procedure TFrmDocumentDropper.btnSaveSelectionsClick(Sender: TObject);
var
  lIniFile: TIniFile;
  lFileName: string;
begin
  SaveDialog.Filter := 'Document Dropper Settings (*.dds)|*.dds';
  if SaveDialog.Execute() then
  begin
    lFileName := SaveDialog.FileName;
    // Add extension
    if not EndsStr('.dds', lFileName) then
      lFileName := lFileName + '.dds';
    lIniFile := TIniFile.Create(lFileName);
    try
      lIniFile.WriteString(cSources, cPathes, GetStringFromStrings(lstbxSources.Items));
      lIniFile.WriteString(cDestinations, cPathes, GetStringFromStrings(lstbxDestinations.Items));
      lIniFile.WriteString(cRules, cRules, mRules.RuleString);
    finally
      lIniFile.Free();
    end;
  end;
end;

procedure TFrmDocumentDropper.btnSrcAddDirClick(Sender: TObject);
var
  lDir: string;
begin
  if SelectDirectory('Select directory', '', lDir) then
  begin
    if lDir <> '' then
      lstbxSources.Items.Add(lDir);
  end;
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
  mRules[lstbxRules.ItemIndex].Rule := GuiToRuleString();
  UpdateRuleView();
end;

procedure TFrmDocumentDropper.cmbxSortChange(Sender: TObject);
begin
  UpdateRuleView();
end;

procedure TFrmDocumentDropper.edtRuleFilterKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 13 then
    UpdateRuleView();
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
  FillRuleComboBox(cmbxRuleSrc);
  mRules := tRuleList.Create(True);
  tbctrlMain.ActiveTab := tbitmSelectSources;
end;

procedure TFrmDocumentDropper.FormDestroy(Sender: TObject);
begin
  mRules.Free();
end;

procedure TFrmDocumentDropper.FormResize(Sender: TObject);
begin
  if Height < 300 then
    Height := 300;
  if Self.Width < 650 then
    Width := 650;
end;

function TFrmDocumentDropper.GuiToRuleString: string;
var
  lRuleType: tRuleType;
begin
  // Prepare pathes
  edtRuleSrc.Text := CorrectDirectorySeparator(edtRuleSrc.Text);
  edtRuleDest.Text := CorrectDirectorySeparator(edtRuleDest.Text);
  // Create RuleString
  lRuleType := tRuleType(cmbxRuleSrc.ItemIndex);
  case lRuleType of
    rtStartsWith: Result := edtRuleSrc.Text + '*';
    rtEndsWith:   Result := '*' + edtRuleSrc.Text;
    rtContains:   Result := '*' + edtRuleSrc.Text + '*';
    rtEquals:     Result := edtRuleSrc.Text;
  end;
  if chkbxMoveTo.IsChecked then
  begin
    Result := Result + ' -> ' + edtRuleDest.Text;
  end;
end;

procedure TFrmDocumentDropper.lstbxDestinationsKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = Ord(#8) then
    if lstbxDestinations.ItemIndex <> -1 then
      lstbxDestinations.Items.Delete(lstbxDestinations.ItemIndex);
end;

procedure TFrmDocumentDropper.lstbxRulesChange(Sender: TObject);
begin
  with mRules[lstbxRules.ItemIndex] do
  begin
    edtRuleSrc.Text := Source;
    edtRuleDest.Text := Destination;
    chkbxMoveTo.IsChecked := IsOtherDestination;
    cmbxRuleSrc.ItemIndex := Ord(RuleType);
  end;
end;

procedure TFrmDocumentDropper.lstbxRulesItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  lstbxRulesChange(Sender);
end;

procedure TFrmDocumentDropper.lstbxRulesKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = Ord(#8) then
  begin
    if lstbxRules.ItemIndex <> -1 then
    begin
      mRules.Delete(lstbxRules.ItemIndex);
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
  lRule: tRule;
  lMatchFilter: Boolean;
begin
  lstbxRules.Clear();
  // Sort rules
  mRules.Sort(TComparer<tRule>.Construct(
    function (const L, R: tRule): Integer
    var
      lL, lR: string;
    begin
      case cmbxSort.ItemIndex of
        0:
        begin
          lL := L.SourceFilter;
          lR := R.SourceFilter;
        end;
        1:
        begin
          lL := L.Destination;
          lR := R.Destination;
        end;
      end;
      if lL = lR then
        Result := 0
      else if lL < lR then
        Result := -1
      else
        Result := 1;
    end
  ));
  // Output rules
  for lRule in mRules do
  begin
    // No filter
    if (edtRuleFilter.Text = '') then
      lstbxRules.Items.Add(lRule.AsString)
    else
    begin
      lMatchFilter := False;
      case cmbxRuleFilter.ItemIndex of
        0: lMatchFilter := ContainsText(lRule.AsString, edtRuleFilter.Text);
        1: lMatchFilter := ContainsText(lRule.Source, edtRuleFilter.Text);
        2: lMatchFilter := ContainsText(lRule.Destination, edtRuleFilter.Text);
      end;
      if lMatchFilter then
        lstbxRules.Items.Add(lRule.AsString);
    end;
  end;
end;

end.
