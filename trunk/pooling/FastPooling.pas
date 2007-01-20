////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  unit FastPooling.pas                                                      //
//    Copyright 2004 by Arcana Technologies Incorporated                      //
//    Written By Jason Southwell                                              //
//                                                                            //
//  Description:                                                              //
//    This unit houses a class that implementats a generic pooling manager.   //
//    This TFastObjectPool class does not use a CS and should perform better  //
//    under heavy loads.                                                      //
//                                                                            //
//  Updates:                                                                  //
//    07/31/2004 - TFastObjectPool Released to Open Source.                   //
//                                                                            //
//  License:                                                                  //
//    This code is covered by the Mozilla Public License 1.1 (MPL 1.1)        //
//    Full text of this license can be found at                               //
//    http://www.opensource.org/licenses/mozilla1.1.html                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit FastPooling;

interface

uses Windows, Classes, SysUtils, SyncObjs;

type
  TObjectEvent = procedure(Sender : TObject; var AObject : TObject) of object;

  PObjectHolder = ^TObjectHolder;
  TObjectHolder = record
    TheObject : TObject;
    AssignedTo : Pointer;
  end;

  TFastObjectPool = class(TObject)
  private
    ObjList : TList;

    FActive : boolean;
    FPoolSize: integer;
    FOnCreateObject: TObjectEvent;
    FOnDestroyObject: TObjectEvent;
    FUsageCount: integer;
    FRaiseExceptions: boolean;
    FAcquireTimeout: integer;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Start(RaiseExceptions : boolean = False); virtual;
    procedure Stop; virtual;

    function Acquire : TObject; virtual;
    procedure Release(item : TObject); virtual;

    property Active : boolean read FActive;
    property AcquireTimeout : integer read FAcquireTimeout write FAcquireTimeout;
    property RaiseExceptions : boolean read FRaiseExceptions write FRaiseExceptions;
    property UsageCount : integer read FUsageCount;
    property PoolSize : integer read FPoolSize write FPoolSize;
    property OnCreateObject : TObjectEvent read FOnCreateObject write FOnCreateObject;
    property OnDestroyObject : TObjectEvent read FOnDestroyObject write FOnDestroyObject;
  end;

implementation

uses DateUtils;

{ TFastObjectPool }

function TFastObjectPool.Acquire: TObject;
var
  idx : integer;
  i : integer;
  pThisThread : Pointer;
  dtStart : TDateTime;
begin
  Result := nil;
  if not FActive then
  begin
    if FRaiseExceptions then
      raise EAbort.Create('Cannot acquire an object before calling Start')
    else
      exit;
  end;

  InterlockedIncrement(FUsageCount);

  Result := nil;

  pThisThread := Pointer(GetCurrentThread);
  i := 0;
  dtStart := Now;
  while Result = nil do
  begin
    if i = ObjList.Count then
    begin
      if FAcquireTimeout = 0 then
        break
      else
        i := 0;
    end;
    InterlockedCompareExchange(PObjectHolder(ObjList[i]).AssignedTo,pThisThread,nil);
    if PObjectHolder(ObjList[i]).AssignedTo = pThisThread then
    begin
      Result := PObjectHolder(ObjList[i]).TheObject;
      break;
    end;
    inc(i);

    if (FAcquireTimeout > 0) and (MilliSecondsBetween(Now,dtStart) > FAcquireTimeout) then
      break;
  end;

  if Result = nil then
  begin
    if FRaiseExceptions then
      raise Exception.Create('There are no available objects in the pool')
    else
      Exit;
  end;
end;

constructor TFastObjectPool.Create;
begin
  ObjList := TList.Create;

  FActive := False;
  FPoolSize := 20;
  FRaiseExceptions := True;
  FOnCreateObject := nil;
  FOnDestroyObject := nil;
end;

destructor TFastObjectPool.Destroy;
begin
  if FActive then
    Stop;
  ObjList.Free;
  inherited;
end;

procedure TFastObjectPool.Release(item: TObject);
  function FindItem(item : TObject) : PObjectHolder;
  var
    i : integer;
  begin
    result := nil;
    for i := ObjList.Count-1 downto 0 do
    begin
      if PObjectHolder(ObjList[i]).TheObject = item then
      begin
        result := PObjectHolder(ObjList[i]);
        break;
      end;
    end;
  end;
var
  idx : integer;
  poh : PObjectHolder;
  pThread : Pointer;
begin
  if not FActive then
  begin
    if FRaiseExceptions then
      raise Exception.Create('Cannot release an object before calling Start')
    else
      exit;
  end;
  if item = nil then
  begin
    if FRaiseExceptions then
      raise Exception.Create('Cannot release an object before calling Start')
    else
      exit;
  end;

  poh := FindItem(item);
  if poh = nil then
  begin
    if FRaiseExceptions then
      raise Exception.Create('Cannot release an object that is not in the pool')
    else
      exit;
  end;
  pThread := Pointer(GetCurrentThread);
  InterlockedCompareExchange(poh^.AssignedTo,nil,pThread);
  Dec(FUsageCount);
end;

procedure TFastObjectPool.Start(RaiseExceptions : boolean = False);
var
  i : integer;
  poh : PObjectHolder;
begin
  // Make sure events are assigned before starting the pool.
  if not Assigned(FOnCreateObject) then
    raise Exception.Create('There must be an OnCreateObject event before calling Start');
  if not Assigned(FOnDestroyObject) then
    raise Exception.Create('There must be an OnDestroyObject event before calling Start');

  // Call the OnCreateObject event once for each item in the pool.
  for i := 0 to FPoolSize-1 do
  begin
    New(poh);
    poh^.TheObject := nil;
    poh^.AssignedTo := nil;
    FOnCreateObject(Self,poh^.TheObject);
    ObjList.Add(poh);
  end;

  // Set the active flag to true so that the Acquire method will return values.
  FActive := True;

  // Automatically set RaiseExceptions to false by default.  This keeps
  // exceptions from being raised in threads.
  FRaiseExceptions := RaiseExceptions;
end;

procedure TFastObjectPool.Stop;
var
  i : integer;
  poh : PObjectHolder;
begin

  // Wait until all objects have been released from the pool.  After waiting
  // 10 seconds, stop anyway.  This may cause unforseen problems, but usually
  // you only Stop a pool as the application is stopping.  40 x 250 = 10,000
  FActive := False;
  for i := 1 to 40 do
  begin
    // Setting Active to false here keeps the Acquire method from continuing to
    // retrieve objects.
    if FUsageCount = 0 then
      break;
    // Sleep here to allow give threads time to release their objects.
    Sleep(250);
  end;


  // Loop through all items in the pool calling the OnDestroyObject event.
  for i := 0 to FPoolSize-1 do
  begin
    poh := ObjList[i];
    if Assigned(FOnDestroyObject) then
      FOnDestroyObject(Self, poh^.TheObject)
    else
      poh^.TheObject.Free;
    Dispose(poh);
  end;

  // clear the memory used by the list object and TBits class.
  ObjList.Clear;

  FRaiseExceptions := True;
end;

end.
