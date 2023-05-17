Program DoomLoader;

Uses
  Vcl.Forms,
  DLMainForm In 'Source\DLMainForm.pas' {frmDLMainForm};

{$R *.res}


Begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Doom Loader';
  Application.CreateForm(TfrmDLMainForm, frmDLMainForm);
  Application.Run;

End.
