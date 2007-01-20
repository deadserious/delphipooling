program PoolTemplate;

uses
  Forms,
  IWHTTPServer,
  Unit1 in 'Unit1.pas' {Form1},
  DataModule1 in 'DataModule1.pas' {dmDataOne: TDataModule},
  DataModule2 in 'DataModule2.pas' {dmDataTwo: TDataModule},
  UserSessionUnit in 'UserSessionUnit.pas' {IWUserSession: TIWUserSessionBase},
  MainForm in 'MainForm.pas' {frmMain: TIWAppForm},
  EmailForm in 'EmailForm.pas' {frmSend: TIWAppForm},
  ServerController in 'ServerController.pas' {IWServerController: TIWServerControllerBase};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Custom StandAlone Server';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
