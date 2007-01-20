unit ServerController;

interface

uses
  SysUtils, Classes, IWServerControllerBase, IWBaseForm, HTTPApp,
  // For OnNewSession Event
  UserSessionUnit, IWApplication, IWAppForm, DataModuleDataAwarePool, IWServer;

type
  TIWServerHacker = class(TIWServer)
  end;

  TIWServerController = class(TIWServerControllerBase)
    PoolSmall: TArcDMDataAwarePool;
    PoolMain: TArcDMDataAwarePool;
    
    procedure IWServerControllerBaseNewSession(ASession: TIWApplication;
      var VMainForm: TIWBaseForm);
    procedure IWServerControllerBaseCreate(Sender: TObject);
    procedure PoolMainGetModuleClass(ASender: TObject;
      var ADataModule: TDataModuleClass);
    procedure PoolSmallGetModuleClass(ASender: TObject;
      var ADataModule: TDataModuleClass);
    procedure PoolMainLogError(ASender: TObject; E: Exception;
      const Msg: String);
    procedure PoolSmallLogError(ASender: TObject; E: Exception;
      const Msg: String);
     
  private
  public
    procedure Log(msg : string);
  end;

  
  function UserSession: TIWUserSession;
  function IWServerController: TIWServerController;

implementation
{$R *.dfm}

uses
  IniFiles, IWInit, IWGlobal, WebCommon, DataModule1, DataModule2;

function IWServerController: TIWServerController;
begin
  Result := TIWServerController(GServerController);
end;

function UserSession: TIWUserSession;
begin
  Result := TIWUserSession(WebApplication.Data);
end;

procedure TIWServerController.IWServerControllerBaseNewSession(
  ASession: TIWApplication; var VMainForm: TIWBaseForm);
begin
  ASession.Data := TIWUserSession.Create(nil);
end;

procedure TIWServerController.IWServerControllerBaseCreate(
  Sender: TObject);
var
  ini : TIniFile;
begin
  ini := TIniFile.Create(DLLFilePath+'PoolTemplate.Ini');
  try
    PoolMain.PoolCount := ini.ReadInteger('DB','MainPoolSize',10);
    PoolMain.PoolMax := PoolMain.PoolCount;
    PoolSmall.PoolCount := ini.ReadInteger('DB','SmallPoolSize',3);
    PoolSmall.PoolMax := PoolSmall.PoolCount;
  finally
    ini.Free;
  end;
  PoolMain.Active := True;
  PoolSmall.Active := True;
end;

procedure TIWServerController.PoolMainGetModuleClass(ASender: TObject;
  var ADataModule: TDataModuleClass);
begin
  ADataModule := TdmDataOne;
end;

procedure TIWServerController.PoolSmallGetModuleClass(ASender: TObject;
  var ADataModule: TDataModuleClass);
begin
  ADataModule := TdmDataTwo;
end;

procedure TIWServerController.Log(msg: string);
begin
  TIWServerHacker(GIWServer).Log(msg);
end;

procedure TIWServerController.PoolMainLogError(ASender: TObject;
  E: Exception; const Msg: String);
begin
  Log('PoolMain Exception: '+Msg);
end;

procedure TIWServerController.PoolSmallLogError(ASender: TObject;
  E: Exception; const Msg: String);
begin
  Log('PoolSmall Exception: '+Msg);
end;

initialization
  TIWServerController.SetServerControllerClass;

end.

