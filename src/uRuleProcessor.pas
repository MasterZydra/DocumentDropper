unit uRuleProcessor;

interface

uses
  uDocDrop_Utils,
  Generics.Collections,
  System.StrUtils, System.DateUtils, System.SysUtils, System.Classes;

type
  tRuleType = (rtStartsWith, rtEndsWith, rtContains, rtEquals);
const
  cRuleType: array[tRuleType] of string = (
    'starts with...', 'ends with...', 'contains...', 'equals...');

type
  tRule = class(TObject)
  private
    FRule: string;
    FSourceFilter: string;
    FSource: string;
    FDestination: string;
    FAsString: string;
    FRuleType: tRuleType;
    FIsOtherDestination: Boolean;
    procedure ParseRule;
    // Setter
    procedure SetRule(const pRule: string);
  public
    constructor Create(const pRule: string); reintroduce;
    /// <summary> Rule string </summary>
    property Rule: string read FRule write SetRule;
    /// <summary> Condition for file name </summary>
    property SourceFilter: string read FSourceFilter;
    property Source: string read FSource;
    /// <summary> Storage location if IsOtherDestination </summary>
    property Destination: string read FDestination;
    /// <summary> Returns rule as text for GUI </summary>
    property AsString: string read FAsString;
    property RuleType: tRuleType read FRuleType;
    property IsOtherDestination: Boolean read FIsOtherDestination;
  end;

  tRuleList = class(TObjectList<tRule>)
  public
    procedure FillList(const pRuleString: string);
    function RuleString: string;
  end;

implementation

resourcestring
  StrMoveFile = 'Move file if name %s "%s"';
  StrMoveFileTo = 'Move file to "%s" if name %s "%s"';

{ tRule }

constructor tRule.Create(const pRule: string);
begin
  inherited Create();
  FRule := pRule;
  ParseRule();
end;

procedure tRule.ParseRule;
var
  lSepPos: Integer;
begin
  lSepPos := Pos('->', FRule);
  FIsOtherDestination := lSepPos <> 0;
  if FIsOtherDestination then
  begin
    FSourceFilter := Trim(Copy(FRule, 1, lSepPos - 1));
    FDestination := Trim(Copy(FRule, lSepPos + 2, Length(FRule) - lSepPos));
  end
  else
  begin
    FSourceFilter := Trim(FRule);
    FDestination := '';
  end;
  FSource := StringReplace(FSourceFilter, '*', '', [rfReplaceAll]);
  // FRuleType
  if StartsStr('*', FSourceFilter) then
  begin
    if EndsStr('*', FSourceFilter) then
      FRuleType := tRuleType.rtContains
    else
      FRuleType := tRuleType.rtEndsWith;
  end
  else if EndsText('*', FSourceFilter) then
    FRuleType := tRuleType.rtStartsWith
  else
    FRuleType := tRuleType.rtEquals;
  // FAsString
  if FIsOtherDestination then
  begin
    FAsString := Format(StrMoveFileTo,
      [FDestination, cRuleType[FRuleType], FSource]);
  end
  else
  begin
    FAsString := Format(StrMoveFile,
      [cRuleType[FRuleType], FSource]);
  end;
end;

procedure tRule.SetRule(const pRule: string);
begin
  FRule := pRule;
  ParseRule();
end;

{ tRuleList }

procedure tRuleList.FillList(const pRuleString: string);
var
  lRules: TStringList;
  s: string;
begin
  Clear();
  lRules := TStringList.Create();
  Split(';', pRuleString, lRules);
  try
    for s in lRules do
    begin
      Add(tRule.Create(s));
    end;
  finally
    lRules.Free;
  end;
end;

function tRuleList.RuleString: string;
var
  lRule: tRule;
  lFirst: Boolean;
begin
  lFirst := True;
  for lRule in Self do
  begin
    if lFirst then
    begin
      lFirst := False;
      Result := lRule.Rule;
    end
    else
      Result := Result  + ';' + lRule.Rule;
  end;
end;

end.
