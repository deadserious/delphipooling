program Project1;

uses
  Forms,
  IWMain,
  ServerController in 'ServerController.pas' {IWServerController: TIWServerController},
  Unit1 in 'Unit1.pas' {IWForm1: TIWFormModuleBase},
  Pooling in '..\..\Pooling.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformIWMain, formIWMain);
  Application.Run;
end.
