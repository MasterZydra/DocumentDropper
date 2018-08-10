program DocumentDropper;

uses
  System.StartUpCopy,
  FMX.Forms,
  uDocDrop_Main in 'uDocDrop_Main.pas' {FrmDocumentDropper},
  uDocDrop_Settings in 'uDocDrop_Settings.pas' {frameDocDropSettings: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmDocumentDropper, FrmDocumentDropper);
  Application.Run;
end.
