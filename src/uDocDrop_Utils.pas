unit uDocDrop_Utils;

interface

uses
  FMX.ListBox, System.Classes;

function GetStringFromStrings(const pStrings: TStrings): string;
procedure FillListBox(const pListBox: TListBox; const pContent: string);
procedure Split(const pDelimiter: Char; const pText: string;
  const pStrings: TStrings);

implementation

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
