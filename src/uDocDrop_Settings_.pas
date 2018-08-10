unit uDocDrop_Settings_;

interface

uses
  System.IniFiles;

type
  tDocDropSettings = class(TObject)
    private
      mIniFile: TIniFile;
    public
      constructor Create; reintroduce;
      destructor Destroy;
  end;

implementation

{ tDocDropSettings }

constructor tDocDropSettings.Create;
begin
  inherited Create();
  mIniFile := TIniFile.Create('Settings.ini');
end;

destructor tDocDropSettings.Destroy;
begin
  mIniFile.Free();
  inherited;
end;

end.
