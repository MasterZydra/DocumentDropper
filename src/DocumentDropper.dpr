program DocumentDropper;

uses
  System.StartUpCopy,
  FMX.Forms,
  uDocDrop_Main in 'uDocDrop_Main.pas' {FrmDocumentDropper},
  uDocDrop_Utils in 'uDocDrop_Utils.pas',
  uRuleProcessor in 'uRuleProcessor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmDocumentDropper, FrmDocumentDropper);
  Application.Run;
end.
