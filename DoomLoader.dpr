Program DoomLoader;

{$R 'DoomLoaderITHVerInfo.res' 'DoomLoaderITHVerInfo.RC'}

uses
  Vcl.Forms,
  DLMainForm in 'Source\DLMainForm.pas' {frmDLMainForm};

{$R *.res}


Begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Doom Loader';
  Application.CreateForm(TfrmDLMainForm, frmDLMainForm);
  Application.Run;

End.
