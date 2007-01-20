////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  unit DataModuleDataAwarePool                                              //
//    Copyright 2004 by Arcana Technologies Incorporated                      //
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
//    4/28/2004 JS -  Created TArcDMDataAwarePool component with the exact    //
//                    same public/published interface as the TDataModulePool  //
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

unit DataModuleDataAwarePool;

interface

uses
  Windows, {$IFDEF VER130}Forms,{$ENDIF} Messages, SysUtils, Classes, SyncObjs,
  ArcIWDMPooling, Midas, db, dbclient, Types, Variants, Provider;

type
  TDataModuleClass = class of TDataModule;
  TArcDMDataAwarePool = class;

  TArcDMPoolAdapter = class(TComponent)
  private
    FDataModule: TDataModule;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataModule : TDataModule read FDataModule;
  end;

  TArcErrorLogEvent = procedure(ASender : TObject; E : Exception; const Msg : string) of object;
  TGetDataModuleClassEvent = procedure(ASender : TObject; var ADataModule : TDataModuleClass) of object;
  TDataModuleNotifyEvent = procedure(ASender : TObject; ADataModule : TDataModule) of object;
  TArcDMDataAwarePool = class(TComponent, IAppServer)
  private
    FSetToActive : boolean;
    FPool : TObjectPool;
    FDataModule : TDataModule; // designtime use only.
    FDataModuleClass : TDataModuleClass;
    FDataModuleAdapter: TArcDMPoolAdapter;
    FOnGetModuleClass: TGetDataModuleClassEvent;
    FOnUnLockDataModule: TDataModuleNotifyEvent;
    FOnLockDataModule: TDataModuleNotifyEvent;
    FOnLogError: TArcErrorLogEvent;
    procedure SetActive(const Value: boolean);
    procedure SetPoolCount(const Value: integer);
    function getVersion: string;
    procedure SetVersion(const Value: string);
    function GetPoolCount: integer;
    function GetActive: boolean;
    function GetAutoGrow: boolean;
    function GetPoolMax: integer;
    procedure SetAutoGrow(const Value: boolean);
    procedure SetPoolMax(const Value: integer);
    procedure SetDataModuleAdapter(const Value: TArcDMPoolAdapter);
  protected
    procedure Start; virtual;
    procedure ErrorIfActive;
    procedure ErrorIfNotActive;
    procedure _OnCreateObject(Sender : TObject; var AObject : TObject);
    procedure _OnDestroyObject(Sender : TObject; var AObject : TObject);
    procedure Loaded; override;
    procedure SetName(const NewName: TComponentName); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoLogError(E: Exception; const Msg : String); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Lock : TDataModule;
    procedure Unlock(var ADataModule : TDataModule);
    function CurrentThreadsInUse : integer;

    // IAppServer Interface implementation
    function AS_ApplyUpdates(const ProviderName: WideString;
      Delta: OleVariant; MaxErrors: Integer; out ErrorCount: Integer;
      var OwnerData: OleVariant): OleVariant; safecall;
    function AS_DataRequest(const ProviderName: WideString;
      Data: OleVariant): OleVariant; safecall;
    procedure AS_Execute(const ProviderName: WideString;
      const CommandText: WideString; var Params: OleVariant;
      var OwnerData: OleVariant); safecall;
    function AS_GetParams(const ProviderName: WideString;
      var OwnerData: OleVariant): OleVariant; safecall;
    function AS_GetProviderNames: OleVariant; safecall;
    function AS_GetRecords(const ProviderName: WideString; Count: Integer;
      out RecsOut: Integer; Options: Integer;
      const CommandText: WideString; var Params: OleVariant;
      var OwnerData: OleVariant): OleVariant; safecall;
    function AS_RowRequest(const ProviderName: WideString; Row: OleVariant;
      RequestType: Integer; var OwnerData: OleVariant): OleVariant;
      safecall;
  published
    // Note, we cannot use TDataModule straight due to a limitation in Delphi.
    // a component cannot have a TDataModule or a TForm property that references
    // a component outside it's own ownership tree.
    property DataModuleAdapter : TArcDMPoolAdapter read FDataModuleAdapter write SetDataModuleAdapter;

    property PoolCount : integer read GetPoolCount write SetPoolCount;

    property Active : boolean read GetActive write SetActive;
    property AutoGrow : boolean read GetAutoGrow write SetAutoGrow;
    property PoolMax : integer read GetPoolMax write SetPoolMax;
    property Version : string read getVersion write SetVersion;
    property OnGetModuleClass : TGetDataModuleClassEvent read FOnGetModuleClass write FOnGetModuleClass;
    property OnLockDataModule : TDataModuleNotifyEvent read FOnLockDataModule write FOnLockDataModule;
    property OnUnLockDataModule : TDataModuleNotifyEvent read FOnUnLockDataModule write FOnUnLockDataModule;
    // OnLogError exists to so that we can find the real reason for a Catostrophic Error.  CDS's will
    // always mask the real exception with Catostrophic Error.
    property OnLogError : TArcErrorLogEvent read FOnLogError write FOnLogError;
  end;

  TOnGetPoolEvent = procedure(ASender : TObject; var APool : TArcDMDataAwarePool) of object;
  TArcPoolConnection = class(TCustomRemoteServer)
  private
    FPool: TArcDMDataAwarePool;
    FOnGetPool: TOnGetPoolEvent;
    FConnected : boolean;
    procedure SetPool(const Value: TArcDMDataAwarePool);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetServerList: OleVariant; override;
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    procedure GetProviderNames(Proc: TGetStrProc); override;
  public
    function GetServer: IAppServer; override;
  published
    property Pool : TArcDMDataAwarePool read FPool write SetPool;
    property OnGetPool : TOnGetPoolEvent read FOnGetPool write FOnGetPool;

    { Publish standard DataSnap Connection properties}
    property Connected;
    property AfterConnect;
    property BeforeConnect;
    property AfterDisconnect;
    property BeforeDisconnect;
  end;

const
  COMPONENT_VERSION = '3.0.0';

function MakeUniqueDBName(Prefix : string='') : string;

implementation

uses Math;

var
  RegisteredPoolList : TStringList;
  CSPoolList : TCriticalSection;

function MakeUniqueDBName(Prefix : string='') : string;
begin
  if Prefix = '' then
    Prefix := 'DB';
  result := Prefix+IntToStr(GetCurrentThreadId);
end;

{ TArcDMDataAwarePool }

function TArcDMDataAwarePool.AS_ApplyUpdates(const ProviderName: WideString;
  Delta: OleVariant; MaxErrors: Integer; out ErrorCount: Integer;
  var OwnerData: OleVariant): OleVariant;
var
  dm : TDataModule;
  c : TComponent;
  p : TBaseProvider;
  dsp : TDatasetProvider;
begin
  try
    p := nil;
    dsp := nil;
    dm := Lock;
    try
      c := dm.FindComponent(ProviderName);
      if c is TBaseProvider then
        p := TBaseProvider(c)
      else
        if c is TDataset then
        begin
          dsp := TDataSetProvider.Create(nil);
          dsp.DataSet := TDataSet(c);
          p := dsp;
        end;
      if p <> nil then
        Result := p.ApplyUpdates(Delta,MaxErrors,ErrorCount,OwnerData);
    finally
      Unlock(dm);
      dsp.Free;
    end;
  except
    on e: exception do
    begin
      DoLogError(e, e.Message);
      raise;
    end;
  end;
end;

function TArcDMDataAwarePool.AS_DataRequest(const ProviderName: WideString;
  Data: OleVariant): OleVariant;
var
  dm : TDataModule;
  c : TComponent;
  p : TBaseProvider;
  dsp : TDatasetProvider;
begin
  try
    p := nil;
    dsp := nil;
    dm := Lock;
    try
      c := dm.FindComponent(ProviderName);
      if c is TBaseProvider then
        p := TBaseProvider(c)
      else
        if c is TDataset then
        begin
          dsp := TDataSetProvider.Create(nil);
          dsp.DataSet := TDataSet(c);
          p := dsp;
        end;
      if p <> nil then
        Result := p.DataRequest(Data);
    finally
      Unlock(dm);
      dsp.Free;
    end;
  except
    on e: exception do
    begin
      DoLogError(e, e.Message);
      raise;
    end;
  end;
end;

procedure TArcDMDataAwarePool.AS_Execute(const ProviderName,
  CommandText: WideString; var Params, OwnerData: OleVariant);
var
  dm : TDataModule;
  c : TComponent;
  p : TBaseProvider;
  dsp : TDatasetProvider;
begin
  try
    p := nil;
    dsp := nil;
    dm := Lock;
    try
      c := dm.FindComponent(ProviderName);
      if c is TBaseProvider then
        p := TBaseProvider(c)
      else
        if c is TDataset then
        begin
          dsp := TDataSetProvider.Create(nil);
          dsp.DataSet := TDataSet(c);
          p := dsp;
        end;
      if p <> nil then
        p.Execute(CommandText, Params, OwnerData);
    finally
      Unlock(dm);
      dsp.Free;
    end;
  except
    on e: exception do
    begin
      DoLogError(e, e.Message);
      raise;
    end;
  end;
end;

function TArcDMDataAwarePool.AS_GetParams(const ProviderName: WideString;
  var OwnerData: OleVariant): OleVariant;
var
  dm : TDataModule;
  c : TComponent;
  p : TBaseProvider;
  dsp : TDatasetProvider;
begin
  try
    p := nil;
    dsp := nil;
    dm := Lock;
    try
      c := dm.FindComponent(ProviderName);
      if c is TBaseProvider then
        p := TBaseProvider(c)
      else
        if c is TDataset then
        begin
          dsp := TDataSetProvider.Create(nil);
          dsp.DataSet := TDataSet(c);
          p := dsp;
        end;
      if p <> nil then
        Result := p.GetParams(OwnerData);
    finally
      Unlock(dm);
      dsp.Free;
    end;
  except
    on e: exception do
    begin
      DoLogError(e, e.Message);
      raise;
    end;
  end;
end;

function TArcDMDataAwarePool.AS_GetProviderNames: OleVariant;
var
  i : Integer;
  sl : TStringList;
begin
  try
    sl := TStringList.Create;
    try
      if Assigned(FDataModule) then
      begin
        for i := 0 to FDataModule.ComponentCount-1 do
        begin
          if (FDataModule.Components[i] is TBaseProvider) or
             (FDataModule.Components[i] is TDataset) then
            sl.Add(FDataModule.Components[i].Name);
        end;
      end;
      VarClear(Result);
      if sl.Count > 0 then
      begin
        Result := VarArrayCreate([0, sl.Count-1], varVariant);
        for I := 0 to sl.Count-1 do
          Result[I] := Variant(sl[I]);
      end;
    finally
      sl.Free;
    end;
  except
    on e: exception do
    begin
      DoLogError(e, e.Message);
      raise;
    end;
  end;
end;

function TArcDMDataAwarePool.AS_GetRecords(const ProviderName: WideString;
  Count: Integer; out RecsOut: Integer; Options: Integer;
  const CommandText: WideString; var Params,
  OwnerData: OleVariant): OleVariant;
var
  dm : TDataModule;
  c : TComponent;
  p : TBaseProvider;
  dsp : TDatasetProvider;
begin
  try
    p := nil;
    dsp := nil;
    dm := Lock;
    try
      c := dm.FindComponent(ProviderName);
      if c is TBaseProvider then
        p := TBaseProvider(c)
      else
        if c is TDataset then
        begin
          dsp := TDataSetProvider.Create(nil);
          dsp.DataSet := TDataSet(c);
          p := dsp;
        end;
        Result := p.GetRecords(Count, RecsOut, Options, CommandText, Params, OwnerData)
    finally
      Unlock(dm);
      dsp.Free;
    end;
  except
    on e: exception do
    begin
      DoLogError(e, e.Message);
      raise;
    end;
  end;
end;

function TArcDMDataAwarePool.AS_RowRequest(const ProviderName: WideString;
  Row: OleVariant; RequestType: Integer;
  var OwnerData: OleVariant): OleVariant;
var
  dm : TDataModule;
  c : TComponent;
  p : TBaseProvider;
  dsp : TDatasetProvider;
begin
  try
    p := nil;
    dsp := nil;
    dm := Lock;
    try
      c := dm.FindComponent(ProviderName);
      if c is TBaseProvider then
        p := TBaseProvider(c)
      else
        if c is TDataset then
        begin
          dsp := TDataSetProvider.Create(nil);
          dsp.DataSet := TDataSet(c);
          p := dsp;
        end;
      if p <> nil then
        Result := p.RowRequest(Row, RequestType, OwnerData);
    finally
      Unlock(dm);
      dsp.Free;
    end;
  except
    on e: exception do
    begin
      DoLogError(e, e.Message);
      raise;
    end;
  end;
end;

constructor TArcDMDataAwarePool.Create(AOwner: TComponent);
begin
  inherited;
  FSetToActive := False;
  FPool := TObjectPool.Create;
  FPool.PoolSize := 20;
  FDataModuleClass := nil;

  FPool.OnCreateObject := _OnCreateObject;
  FPool.OnDestroyObject := _OnDestroyObject;
end;

function TArcDMDataAwarePool.CurrentThreadsInUse: integer;
begin
  Result := FPool.UsageCount;
end;

destructor TArcDMDataAwarePool.Destroy;
begin
  if Active then
    Active := False;
  FPool.Free;
  if Assigned(FDataModuleAdapter) then
    FDataModuleAdapter.RemoveFreeNotification(Self);
  inherited;
end;

procedure TArcDMDataAwarePool.DoLogError(E: Exception; const Msg: String);
begin
  if Assigned(FOnLogError) then
    FOnLogError(Self, E, Msg);
end;

procedure TArcDMDataAwarePool.ErrorIfActive;
begin
  if (not(csDesigning in ComponentState)) and FPool.Active then
    raise EComponentError.Create('You cannot perform this operation on an active TArcDMDataAwarePool');
end;

procedure TArcDMDataAwarePool.ErrorIfNotActive;
begin
  if not FPool.Active then
    raise EComponentError.Create('You cannot perform this operation on an inactive TArcDMDataAwarePool');
end;

function TArcDMDataAwarePool.GetActive: boolean;
begin
  if (csDesigning in ComponentState) then
    Result := FSetToActive
  else
    Result := FPool.Active;
end;

function TArcDMDataAwarePool.GetAutoGrow: boolean;
begin
  Result := FPool.AutoGrow;
end;

function TArcDMDataAwarePool.GetPoolCount: integer;
begin
  Result := FPool.PoolSize;
end;

function TArcDMDataAwarePool.GetPoolMax: integer;
begin
  Result := FPool.GrowToSize;
end;

function TArcDMDataAwarePool.getVersion: string;
begin
  Result := COMPONENT_VERSION;
end;

procedure TArcDMDataAwarePool.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    if FSetToActive then
    begin
      Start;
      FSetToActive := False;
    end;
end;

function TArcDMDataAwarePool.Lock: TDataModule;
begin
  if csDesigning in ComponentState then
  begin
    if FDataModule = nil then
      raise Exception.Create('No DataModuleAdapter is assigned to Pool.');
    Result := FDataModule;
  end else
  begin
    ErrorIfNotActive;
    Result := TDataModule(FPool.Acquire);
    if Assigned(FOnLockDataModule) then
      FOnLockDataModule(Self, Result);
  end;
end;

procedure TArcDMDataAwarePool.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and ((AComponent = FDataModuleAdapter) or (AComponent = FDataModule)) then
  begin
    FDataModule := nil;
    FDataModuleAdapter := nil;
  end;
  {if (Operation = opRemove) and (AComponent = FDataModule) then
    FDataModule := nil;}
end;

procedure TArcDMDataAwarePool.SetActive(const Value: boolean);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
  begin
    FSetToActive := Value;
  end else
  begin
    if Value = FPool.Active then
    exit;

    if Value then
      Start
    else
      FPool.Stop;
  end;
end;

procedure TArcDMDataAwarePool.SetAutoGrow(const Value: boolean);
begin
  FPool.AutoGrow := Value;
end;

procedure TArcDMDataAwarePool.SetDataModuleAdapter(const Value: TArcDMPoolAdapter);
begin
  if FDataModuleAdapter = Value then
    exit;
  if FDataModuleAdapter <> nil then
  begin
    if FDataModuleAdapter.DataModule <> nil then
      FDataModuleAdapter.DataModule.RemoveFreeNotification(Self);
    FDataModuleAdapter.RemoveFreeNotification(Self);
  end;
  FDataModuleAdapter := Value;
  if FDataModuleAdapter <> nil then
  begin
    FDataModuleAdapter.FreeNotification(Self);
    FDataModule := FDataModuleAdapter.DataModule;
    if FDataModule <> nil then
      FDataModule.FreeNotification(Self);
    if not (csDesigning in ComponentState) and (FSetToActive) and (not Active) then
      Active := True;
  end else
    FDataModule := nil;
end;

procedure TArcDMDataAwarePool.SetName(const NewName: TComponentName);
var
  idx : integer;
begin
  idx := RegisteredPoolList.IndexOf(Name);
  if idx >= 0 then
    RegisteredPoolList.Delete(idx);
  inherited;
  if NewName <> '' then
  begin
    if Active or (csDesigning in ComponentState) then
    RegisteredPoolList.AddObject(NewName,Self);
  end;
end;

procedure TArcDMDataAwarePool.SetPoolCount(const Value: integer);
begin
  ErrorIfActive;
  FPool.PoolSize := Value;
end;

procedure TArcDMDataAwarePool.SetPoolMax(const Value: integer);
begin
  FPool.GrowToSize := Value;
end;

procedure TArcDMDataAwarePool.SetVersion(const Value: string);
begin
  // do nothing;
end;

procedure TArcDMDataAwarePool.Start;
begin
  if (FDataModule <> nil) then
    FDataModuleClass := TDataModuleClass(FDataModule.ClassType); 
  if Assigned(FOnGetModuleClass) then
    FOnGetModuleClass(Self,FDataModuleClass);
  if FDataModuleClass = nil then
    raise Exception.Create('Could not retrieve DataModule from DataModuleAdapter');
  FPool.Start(true);
end;

procedure TArcDMDataAwarePool.Unlock(var ADataModule : TDataModule);
begin
  if not (csDesigning in ComponentState) then
  begin
    if ADataModule = nil then
      exit;
    if Assigned(FOnUnLockDataModule) then
      FOnUnLockDataModule(Self, ADataModule);
    ErrorIfNotActive;
    FPool.Release(ADataModule);
    ADataModule := nil;
  end;
end;

procedure TArcDMDataAwarePool._OnCreateObject(Sender: TObject;
  var AObject: TObject);
begin
  if FDataModuleClass = nil then
    raise Exception.Create('Cannot determine TDataModule class to create.');
  AObject := FDataModuleClass.Create(nil);
end;

procedure TArcDMDataAwarePool._OnDestroyObject(Sender: TObject;
  var AObject: TObject);
begin
  FreeAndNil(AObject);
end;

{ TArcPoolConnection }

procedure TArcPoolConnection.DoConnect;
begin
  if (not Assigned(FPool)) and (Assigned(FOnGetPool)) then
    FOnGetPool(Self, FPool);

  if (not (csDesigning in ComponentState)) and (not Assigned(FPool)) then
    raise Exception.Create('Pool property is nil.');

  FConnected := True;
  inherited;
end;

procedure TArcPoolConnection.DoDisconnect;
begin
  FConnected := False;
  inherited;
end;

function TArcPoolConnection.GetConnected: Boolean;
begin
  Result := FConnected;
end;

procedure TArcPoolConnection.GetProviderNames(Proc: TGetStrProc);
var
  List: Variant;
  I: Integer;
begin
  Connected := True;
  VarClear(List);
  try
    List := GetServer.AS_GetProviderNames;
  except
    { Assume any errors means the list is not available. }
  end;
  if VarIsArray(List) and (VarArrayDimCount(List) = 1) then
    for I := VarArrayLowBound(List, 1) to VarArrayHighBound(List, 1) do
      Proc(List[I]);
end;

function TArcPoolConnection.GetServer: IAppServer;
begin
  Connected := True;
  Result := FPool as IAppServer;
end;

function TArcPoolConnection.GetServerList: OleVariant;
var
  i : Integer;
begin
  CSPoolList.Enter;
  try
    VarClear(Result);
    if RegisteredPoolList.Count > 0 then
    begin
      Result := VarArrayCreate([0, RegisteredPoolList.Count-1], varVariant);
      for I := 0 to RegisteredPoolList.Count-1 do
        Result[I] := Variant(RegisteredPoolList[I]);
    end;
  finally
    CSPoolList.Leave;
  end;
end;

procedure TArcPoolConnection.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FPool) then
    FPool := nil;
end;

procedure TArcPoolConnection.SetPool(const Value: TArcDMDataAwarePool);
begin
  if FPool = Value then exit;
  if FPool <> nil then
    FPool.RemoveFreeNotification(Self);
  FPool := Value;
  if FPool <> nil then
    FPool.FreeNotification(Self);
end;

{ TArcDMPoolAdapter }

constructor TArcDMPoolAdapter.Create(AOwner: TComponent);
begin
  if not (AOwner is TDataModule) then
    raise Exception.Create('TArcDMPoolAdapter must be owned by a TDataModule.');
  inherited;
  FDataModule := TDataModule(AOwner);
  FDataModule.FreeNotification(Self);
end;

destructor TArcDMPoolAdapter.Destroy;
begin
  if FDataModule <> nil then
    FDataModule.RemoveFreeNotification(Self);
  inherited;
end;

procedure TArcDMPoolAdapter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDataModule) then
    FDataModule := nil;
end;

initialization
  RegisteredPoolList := TStringList.Create;
  CSPoolList := TCriticalSection.Create;

finalization
  RegisteredPoolList.Free;
  CSPoolList.Free;


end.
