unit uDocDrop_Utils;

interface

uses
  FMX.ListBox, System.Classes, System.SysUtils, System.IOUtils;

function GetStringFromStrings(const pStrings: TStrings): string;
procedure FillListBox(const pListBox: TListBox; const pContent: string);
procedure Split(const pDelimiter: Char; const pText: string;
  const pStrings: TStrings);
function MatchStrings(Source, pattern: string): Boolean;
function CorrectDirectorySeparator(const pPath: string): string;


implementation

function CorrectDirectorySeparator(const pPath: string): string;
begin
  Result := StringReplace(pPath, '/', TPath.DirectorySeparatorChar, [rfReplaceAll]);
  Result := StringReplace(Result, '\', TPath.DirectorySeparatorChar, [rfReplaceAll]);
end;

function MatchStrings(Source, pattern: string): Boolean;
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


function GetStringFromStrings(const pStrings: TStrings): string;
var
  i: Integer;
begin
  for i := 0 to pStrings.Count - 1 do
  begin
    if i > 0 then
      Result := Result + ';' + pStrings[i]
    else
      Result := pStrings[i];
  end;
end;

procedure FillListBox(const pListBox: TListBox; const pContent: string);
begin
  Split(';', pContent, pListBox.Items);
end;

procedure Split(const pDelimiter: Char; const pText: string;
  const pStrings: TStrings);
begin
  pStrings.Clear;
  pStrings.Delimiter       := pDelimiter;
  pStrings.StrictDelimiter := True;
  pStrings.DelimitedText   := pText;
end;

end.
