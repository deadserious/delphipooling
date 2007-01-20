library Project1;

uses
  ActiveX,
  ComObj,
  WebBroker,
  ISAPIThreadPool,
  ISAPIApp,
  Unit2 in 'Unit2.pas' {WebModule2: TWebModule},
  Pooling in '..\..\Pooling.pas';

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.CreateForm(TWebModule2, WebModule2);
  Application.Run;
end.
