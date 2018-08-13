unit uDocDrop_Settings;

interface

uses
  System.IniFiles,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl;

type
  TframeDocDropSettings = class(TFrame)
    tabCtrlSettings: TTabControl;
    tabItmCommon: TTabItem;
  private
    { Private-Deklarationen }
    mIniFile: TIniFile;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TframeDocDropSettings }

constructor TframeDocDropSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  mIniFile := TIniFile.Create('Settings.ini');
end;

destructor TframeDocDropSettings.Destroy;
begin
  mIniFile.Free();
  inherited;
end;

end.
