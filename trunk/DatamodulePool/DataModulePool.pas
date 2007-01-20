////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  unit DataModulePool                                                       //
//    Copyright 2002 by Arcana Technologies Incorporated                      //
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
//    08/31/2001 JS - Released to TDataMoudlePool to Open Source.             //
//    05/03/2002 JS - Added MakeUniqueDBName to solve problems with some dbs  //
//                    (ie DBISAM) where duplicate DatabaseNames cause         //
//                    problems.  Use it in the OnCreateDataModule to Assign   //
//                    to the DatabaseName property of the Database component  //
//                    and each table component.                               //
//    06/12/2002 JS - Fixed bug where threads weren't getting released if you //
//                    didn't programatically set active to false before       //
//                    freeing the component.  I added code to the destructor  //
//                    to insure that this happens.                            //
//                    Also fixed a bug which caused an access violation if    //
//                    you tried to set the component active=False then back   //
//                    to active=True                                          //
//    06/13/2002 JS - Fixed bug in destruction of component.  Threads were    //
//                    Getting hung and the OnFreeDataModule event was not     //
//                    getting called.  Also made the destruction process      //
//                    more stable.                                            //
//    07/25/2002 JS - Fixed apache locking bug.  For some reason, apache      //
//                    locks when calling TThread.Waitfor.  Instead I added    //
//                    a work around by using the OnTerminate event of the     //
//                    thread to know that it is ok to free.  It seems to work //
//                    fine for both ISAPI and Apache DSOs.  This should be    //
//                    tested well though.  There is still an outstanding      //
//                    issue when the TDataModulePool component is used in     //
//                    Apache.  For some reason, Apache loads the DSO, frees   //
//                    the DSO and then loads the DSO again.  In most          //
//                    implementations, this will cause your datamodules to be //
//                    created, then destroyed, then created again before      //
//                    being truly ready for connections.  If you connect to   //
//                    the database in the creation of the datamodule, you     //
//                    will notice some extremely slow start times.  But the   //
//                    runtime should not be effected.                         //
//    08/01/2002 JS - Rolled back the apache bug fix.  It caused problems     //
//                    Apache was ran as a service.                            //
//    08/25/2002 JS - Moved Component Registration to a new unit.             //
//                                                                            //
//  License:                                                                  //
//    This code is covered by the Mozilla Public License 1.1 (MPL 1.1)        //
//    Full text of this license can be found at                               //
//    http://www.opensource.org/licenses/mozilla1.1.html                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit DataModulePool;

interface

uses
  Windows, {$IFDEF VER130}Forms,{$ENDIF} Messages, SysUtils, Classes, SyncObjs;

type
  TDataModulePool = class;

  TDataModuleEvent = procedure(var ADataModule : TDataModule) of Object;
  TDataModuleThread = class(TThread)
  private
    Pool : TDataModulePool;
    FreeDM : TDataModuleEvent;
    CreateDM : TDataModuleEvent;
  protected
    procedure DoFreeDM; virtual;
    procedure DoCreateDM; virtual;
    procedure Execute; override;
  public
    DM : TDataModule;
    ReadyToFree : boolean;
    constructor Create( const aPool : TDataModulePool; const aCreateDM, aFreeDM : TDataModuleEvent); virtual;
  end;

  TDataModulePool = class(TComponent)
  private
    FThreads : TList;
    FLocked : TBits;
    CS : TCriticalSection;
    FActive: boolean;
    FPoolCount : integer;
    FOnCreateDataModule: TDataModuleEvent;
    FOnFreeDataModule: TDataModuleEvent;
    procedure SetActive(const Value: boolean);
    procedure SetPoolCount(const Value: integer);
    function getVersion: string;
    procedure SetVersion(const Value: string);
  protected
    procedure ErrorIfActive;
    procedure ErrorIfNotActive;
    procedure ThreadTerminateEvent(Sender : TObject);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Lock : TDataModule;
    procedure Unlock(var ADataModule : TDataModule);
    function CurrentThreadsInUse : integer;
  published
    property OnCreateDataModule : TDataModuleEvent read FOnCreateDataModule write FOnCreateDataModule;
    property OnFreeDataModule : TDataModuleEvent read FOnFreeDataModule write FOnFreeDataModule;
    property PoolCount : integer read FPoolCount write SetPoolCount;

    property Active : boolean read FActive write SetActive;
    property Version : string read getVersion write SetVersion;
  end;

const
  COMPONENT_VERSION = '0.1.1b';

var
  TerminatedCount : integer;

function MakeUniqueDBName(Prefix : string='') : string;

implementation

function MakeUniqueDBName(Prefix : string='') : string;
begin
  if Prefix = '' then
    Prefix := 'DB';
  result := Prefix+IntToStr(GetCurrentThreadId);
end;

{ TDataModulePool }

constructor TDataModulePool.Create(AOwner: TComponent);
begin
  inherited;
  CS := TCriticalSection.Create;
  FThreads := TList.Create;
  FLocked := TBits.Create;
  FActive := False;
  FPoolCount := 20;
  FLocked.Size := 20;
end;

function TDataModulePool.CurrentThreadsInUse: integer;
var
  i : integer;
begin
  CS.Enter;
  try
    Result := 0;
    for i := 0 to FLocked.Size-1 do
      if FLocked[i] then
        inc(Result);
  finally
    CS.Leave;
  end;
end;

destructor TDataModulePool.Destroy;
var
  i : integer;
begin
  for i := 0 to FThreads.Count-1 do
    if Assigned(TDataModuleThread(FThreads[i]).DM) then
      TDataModuleThread(FThreads[i]).DM.Destroying;
  if Active then
    Active := False;
  FThreads.Free;
  FLocked.Free;
  CS.Free;
  inherited;
end;

procedure TDataModulePool.ErrorIfActive;
begin
  if (not(csDesigning in ComponentState)) and FActive then
    raise EComponentError.Create('You cannot perform this operation on an active TDataModulePool');
end;

procedure TDataModulePool.ErrorIfNotActive;
begin
  if not FActive then
    raise EComponentError.Create('You cannot perform this operation on an inactive TDataModulePool');
end;

function TDataModulePool.getVersion: string;
begin
  Result := COMPONENT_VERSION;
end;

function TDataModulePool.Lock: TDataModule;
var
  i : integer;
begin
  ErrorIfNotActive;
  CS.Enter;
  try
    Result := nil;
    i := FLocked.OpenBit;
    if i >= FPoolCount then
      raise EComponentError.Create('There are no available connections in the pool.');
    FLocked[i] := True;
    Result := TDataModuleThread(FThreads[i]).DM;
  finally
    CS.Leave;
  end;
end;

procedure TDataModulePool.SetActive(const Value: boolean);
var
  i : integer;
begin
  if Value=FActive then exit;

  if not (csDesigning in ComponentState) then
    if Value then
    begin
      CS.Enter;
      try
        for i := 0 to FPoolCount-1 do
        begin
          FThreads.Add(TDataModuleThread.Create(Self, FOnCreateDataModule, FOnFreeDataModule));
          //TDataModuleThread(FThreads[FThreads.Count-1]).OnTerminate := ThreadTerminateEvent;
        end;
        FLocked.Size := FPoolCount;
      finally
        CS.Leave;
      end;
      FActive := True;
    end else
    begin
      FActive := False;
      CS.Enter;
      try
        TerminatedCount := 0;
        while FThreads.Count > 0 do
        begin
          TDatamoduleThread(FThreads[FThreads.Count-1]).Terminate;
          {while not TDatamoduleThread(FThreads[FThreads.Count-1]).ReadyToFree do
          begin
            CheckSynchronize;
            Sleep(1);
          end;}
          Sleep(1);
          TDatamoduleThread(FThreads[FThreads.Count-1]).WaitFor;  // waitfor causes problems in apache.  Instead using the ReadyToFree workaround.
          TDatamoduleThread(FThreads[FThreads.Count-1]).Free;
          FThreads.Delete(FThreads.Count-1);
          FLocked.Size := FLocked.Size-1;
        end;
      finally
        CS.Leave;
      end;
    end;
  FActive := Value;
end;

procedure TDataModulePool.SetPoolCount(const Value: integer);
begin
  ErrorIfActive;
  FPoolCount := Value;
end;

procedure TDataModulePool.SetVersion(const Value: string);
begin
  // do nothing;
end;

procedure TDataModulePool.ThreadTerminateEvent(Sender: TObject);
begin
  TDataModuleThread(Sender).ReadyToFree := True;
end;

procedure TDataModulePool.Unlock(var ADataModule : TDataModule);
var
  i, j : integer;
begin
  ErrorIfNotActive;
  CS.Enter;
  try
    j := -1;
    for i := 0 to FThreads.Count-1 do
      if TDataModuleThread(FThreads[i]).DM = ADataModule then
      begin
        j := i;
        break;
      end;
    if j >= 0 then
      FLocked[j] := False;
  finally
    CS.Leave;
  end;
  ADataModule := nil;
end;

{ TDataModuleThread }

constructor TDataModuleThread.Create( const aPool : TDataModulePool; const aCreateDM, aFreeDM : TDataModuleEvent);
begin
  inherited Create(True);
  Pool := aPool;
  FreeDM := aFreeDM;
  CreateDM := aCreateDM;
  Synchronize(DoCreateDM);
  FreeOnTerminate := False;
  Resume;
end;

procedure TDataModuleThread.DoCreateDM;
begin
  if Assigned(CreateDM) then
    CreateDM(DM);
end;

procedure TDataModuleThread.DoFreeDM;
begin
  if Assigned(FreeDM) then
    FreeDM(DM);
end;

procedure TDataModuleThread.Execute;
begin
  while not terminated do
    Sleep(1);
  Synchronize(DoFreeDM);
  InterlockedIncrement(TerminatedCount);
end;

end.
