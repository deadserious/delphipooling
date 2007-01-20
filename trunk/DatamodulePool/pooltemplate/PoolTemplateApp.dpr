program PoolTemplateApp;

uses
  Forms,
  IWMain,
  DataModule1 in 'DataModule1.pas' {dmDataOne: TDataModule},
  DataModule2 in 'DataModule2.pas' {dmDataTwo: TDataModule},
  EmailForm in 'EmailForm.pas' {frmSend: TIWAppForm},
  MainForm in 'MainForm.pas' {frmMain: TIWAppForm},
  UserSessionUnit in 'UserSessionUnit.pas' {IWUserSession: TIWUserSessionBase},
  ServerController in 'ServerController.pas' {IWServerController: TIWServerControllerBase},
  SQLLogger in 'SQLLogger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformIWMain, formIWMain);
  Application.Run;
end.
