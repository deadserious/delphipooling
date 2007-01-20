////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  unit DataModuleServerPool                                                 //
//    Copyright 2003 by Arcana Technologies Incorporated                      //
//    Written By Jason Southwell                                              //
//                                                                            //
//  Description:                                                              //
//    This component provides a thread safe pool of TDataModules.  The        //
//    main use of such a component would be for increasing the scalability    //
//    of web applications.  In particular, this component was developed       //
//    IntraWeb in mind, however it works perfectly fine in other Delphi       //
//    web development platforms such as WebBroker and WebSnap.                //
//                                                                            //
//    Information on IntraWeb can be found at www.atozedsoftware.com          //
//    Arcana Technologies Incorporated has no affilation with IntraWeb        //
//    or Atozed Software with the exception of being a satisfied customer.    //
//                                                                            //
//  Updates:                                                                  //
//    12/16/2003 JS - Created TArcDMServerPool component with the exact same  //
//                    public/published interface as the TDataModulePool       //
//                    component.  This rewrite is a much lighter version of   //
//                    the TDataModulePool based on the TObjectPool class,     //
//                    also from Arcana Technologies.  This version removes    //
//                    the duplicate thread layer when used in server apps     //
//                    which are already inherently threaded.                  //
//                                                                            //
//  License:                                                                  //
//    This code is covered by the Mozilla Public License 1.1 (MPL 1.1)        //
//    Full text of this license can be found at                               //
//    http://www.opensource.org/licenses/mozilla1.1.html                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit DataModuleServerPool;

interface

uses
  Windows, {$IFDEF VER130}Forms,{$ENDIF} Messages, SysUtils, Classes, SyncObjs,
  ArcIWDMPooling;

type
  TArcDMServerPool = class;

  TDataModuleEvent = procedure(var ADataModule : TDataModule) of Object;

  TArcDMServerPool = class(TComponent)
  private
    FSetToActive : boolean;
    FPool : TObjectPool;
    FOnCreateDataModule: TDataModuleEvent;
    FOnFreeDataModule: TDataModuleEvent;
    procedure SetActive(const Value: boolean);
    procedure SetPoolCount(const Value: integer);
    function getVersion: string;
    procedure SetVersion(const Value: string);
    function GetPoolCount: integer;
    function GetActive: boolean;
  protected
    procedure ErrorIfActive;
    procedure ErrorIfNotActive;
    procedure _OnCreateObject(Sender : TObject; var AObject : TObject);
    procedure _OnDestroyObject(Sender : TObject; var AObject : TObject);
    procedure Loaded; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Lock : TDataModule;
    procedure Unlock(var ADataModule : TDataModule);
    function CurrentThreadsInUse : integer;
  published
    property OnCreateDataModule : TDataModuleEvent read FOnCreateDataModule write FOnCreateDataModule;
    property OnFreeDataModule : TDataModuleEvent read FOnFreeDataModule write FOnFreeDataModule;
    property PoolCount : integer read GetPoolCount write SetPoolCount;

    property Active : boolean read GetActive write SetActive;
    property Version : string read getVersion write SetVersion;
  end;

const
  COMPONENT_VERSION = '2.0.0';

function MakeUniqueDBName(Prefix : string='') : string;
{$IFDEF FPC}
procedure Register;
{$ENDIF}

implementation


{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('Data Pool', [TArcDMServerPool]);
end;
{$ENDIF}

function MakeUniqueDBName(Prefix : string='') : string;
begin
  if Prefix = '' then
    Prefix := 'DB';
  result := Prefix+IntToStr(GetCurrentThreadId);
end;

{ TArcDMServerPool }

constructor TArcDMServerPool.Create(AOwner: TComponent);
begin
  inherited;
  FSetToActive := False;
  FPool := TObjectPool.Create;
  FPool.PoolSize := 20;


  FPool.OnCreateObject := @_OnCreateObject ;
  FPool.OnDestroyObject := @_OnDestroyObject;
end;

function TArcDMServerPool.CurrentThreadsInUse: integer;
begin
  Result := FPool.UsageCount;
end;

destructor TArcDMServerPool.Destroy;
var
  i : integer;
begin
  if Active then
    Active := False;
  FPool.Free;
  inherited;
end;

procedure TArcDMServerPool.ErrorIfActive;
begin
  if (not(csDesigning in ComponentState)) and FPool.Active then
    raise EComponentError.Create('You cannot perform this operation on an active TArcDMServerPool');
end;

procedure TArcDMServerPool.ErrorIfNotActive;
begin
  if not FPool.Active then
    raise EComponentError.Create('You cannot perform this operation on an inactive TArcDMServerPool');
end;

function TArcDMServerPool.GetActive: boolean;
begin
  Result := FPool.Active;
end;

function TArcDMServerPool.GetPoolCount: integer;
begin
  Result := FPool.PoolSize;
end;

function TArcDMServerPool.getVersion: string;
begin
  Result := COMPONENT_VERSION;
end;

procedure TArcDMServerPool.Loaded;
begin
  inherited;
  if FSetToActive then
  begin
    FPool.Start(true);
    FSetToActive := False;
  end;
end;

function TArcDMServerPool.Lock: TDataModule;
begin
  ErrorIfNotActive;
  Result := TDataModule(FPool.Acquire);
end;

procedure TArcDMServerPool.SetActive(const Value: boolean);
begin
  if Value = FPool.Active then
    exit;

  if csLoading in ComponentState then
  begin
    FSetToActive := True;   
  end else
  begin
    if Value then
      FPool.Start(True)
    else
      FPool.Stop;
  end;
end;

procedure TArcDMServerPool.SetPoolCount(const Value: integer);
begin
  ErrorIfActive;
  FPool.PoolSize := Value;
end;

procedure TArcDMServerPool.SetVersion(const Value: string);
begin
  // do nothing;
end;

procedure TArcDMServerPool.Unlock(var ADataModule : TDataModule);
begin
  ErrorIfNotActive;
  FPool.Release(ADataModule);
  ADataModule := nil;
end;

procedure TArcDMServerPool._OnCreateObject(Sender: TObject;
  var AObject: TObject);
var
  DM : TDataModule;
begin
  DM := nil;
  if Assigned(FOnCreateDataModule) then
    FOnCreateDataModule(DM);
  AObject := DM;
end;

procedure TArcDMServerPool._OnDestroyObject(Sender: TObject;
  var AObject: TObject);
var
  DM : TDataModule;
begin
  DM := TDataModule(AObject);
  if Assigned(FOnFreeDataModule) then
    FOnFreeDataModule(DM);
  AObject := nil;
end;

end.
