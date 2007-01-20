unit ServerController;

interface

uses
  SysUtils, Classes, IWServerControllerBase, IWBaseForm, HTTPApp,
  // For OnNewSession Event
  UserSessionUnit, IWApplication, IWAppForm, DataModuleUnit, IWDataModulePool,
  DataModuleDataAwarePool;

type
  TIWServerController = class(TIWServerControllerBase)
    Pool: TArcDMDataAwarePool;
    procedure IWServerControllerBaseNewSession(ASession: TIWApplication;
      var VMainForm: TIWBaseForm);
    procedure PoolGetModuleClass(Sender: TObject;
      var DataModule: TDataModuleClass);
  private
  public
  end;

  function UserSession: TIWUserSession;
  function IWServerController: TIWServerController;

implementation

{$R *.dfm}

uses
  IWInit, IWGlobal;

function UserSession: TIWUserSession;
begin
  Result := TIWUserSession(WebApplication.Data);
end;

function IWServerController: TIWServerController;
begin
  Result := TIWServerController(GServerController);
end;

procedure TIWServerController.IWServerControllerBaseNewSession(
  ASession: TIWApplication; var VMainForm: TIWBaseForm);
begin
  ASession.Data := TIWUserSession.Create(nil);
end;

procedure TIWServerController.PoolGetModuleClass(Sender: TObject;
  var DataModule: TDataModuleClass);
begin
  DataModule := TDataModule1;
end;

initialization
  TIWServerController.SetServerControllerClass;

end.

 
