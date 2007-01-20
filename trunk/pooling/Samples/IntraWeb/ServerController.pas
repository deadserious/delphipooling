unit ServerController;

interface

uses
  SysUtils, Classes, IWServerControllerBase, IWBaseForm, HTTPApp, Pooling,
  // For OnNewSession Event
  IWApplication, IWAppForm;

type
  TIWServerController = class(TIWServerControllerBase)
    procedure IWServerControllerBaseNewSession(ASession: TIWApplication;
      var VMainForm: TIWBaseForm);
    procedure IWServerControllerBaseCreate(Sender: TObject);
    procedure IWServerControllerBaseDestroy(Sender: TObject);
  private
    Pool : TObjectPool;
    procedure OnCreateObject(Sender : TObject; var AObject : TObject);
    procedure OnDestroyObject(Sender : TObject; var AObject : TObject);
  public
    // Note: TMyDataModule is a fictional DataModule that you would create to
    //       house your page data.

    // We create an AcquireDM method so that we don't have to typecast calls to Pool.Aquire.
    function AquireDM : TMyDataModule;
    procedure ReleaseDM(dm : TMyDataModule);
  end;

  TUserSession = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
  end;

// Procs
  function UserSession: TUserSession;

implementation
{$R *.dfm}

uses
  IWInit;

function UserSession: TUserSession;
begin
  Result := TUserSession(WebApplication.Data);
end;

procedure TIWServerController.IWServerControllerBaseNewSession(
  ASession: TIWApplication; var VMainForm: TIWBaseForm);
begin
  ASession.Data := TUserSession.Create(nil);
end;

constructor TUserSession.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TIWServerController.IWServerControllerBaseCreate(
  Sender: TObject);
begin
  Pool := TObjectPool.Create;
  Pool.PoolSize := 20;
  Pool.AutoGrow := False;
  Pool.OnCreateObject := OnCreateObject;
  Pool.OnDestroyObject := OnDestroyObject;
  Pool.Start;
end;

procedure TIWServerController.IWServerControllerBaseDestroy(
  Sender: TObject);
begin
  Pool.Stop;
  Pool.Free;
end;

procedure TIWServerController.OnCreateObject(Sender: TObject;
  var AObject: TObject);
begin
  AObject := TMyDataModule.Create(nil);
end;

procedure TIWServerController.OnDestroyObject(Sender: TObject;
  var AObject: TObject);
begin
  AObject.Free;
end;

function TIWServerController.AquireDM: TMyDataModule;
begin
  Result := TMyDataModule(Pool.Acquire);
end;

procedure TIWServerController.ReleaseDM(dm: TMyDataModule);
begin
  Pool.Release(dm);
end;

initialization
  TIWServerController.SetServerControllerClass;

end.

 
