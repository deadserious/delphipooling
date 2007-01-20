unit DataModule2;

interface

uses
  SysUtils, Classes, DBXpress, FMTBcd, DB, SqlExpr, DataModuleDataAwarePool;

type
  TdmDataTwo = class(TDataModule)
    connDB: TSQLConnection;
    tblContacts: TSQLTable;
    tblContactsPK: TIntegerField;
    tblContactsEMAIL: TStringField;
    PoolAdapter: TArcDMPoolAdapter;
    procedure DataModuleCreate(Sender: TObject);
  private
    Monitor : TSQLMonitor;
    procedure MonitorLogTrace(Sender: TObject; CBInfo: pSQLTRACEDesc);
  public
    { Public declarations }
  end;

var
  dmDataTwo: TdmDataTwo;

implementation

uses IniFiles, WebCommon, SQLLogger;

{$R *.dfm}

procedure TdmDataTwo.DataModuleCreate(Sender: TObject);
var
  ini : TIniFile;
begin
  if connDB.Connected then
    raise Exception.Create('You forgot to disconnect the database at designtime.');

  ini := TIniFile.Create(DLLFilePath+'PoolTemplate.Ini');
  try
    connDB.Params.Values['User_Name'] := ini.ReadString('DB','Username','');
    connDB.Params.Values['Password'] := ini.ReadString('DB','Password','');
    connDB.Params.Values['Database'] := ini.ReadString('DB','Filename','');
  finally
    ini.Free;
  end;

  Monitor := TSQLMonitor.Create(Self);
  Monitor.OnLogTrace := MonitorLogTrace;
  Monitor.SQLConnection := connDB;
  Monitor.Active := True;

  connDB.Connected := True;
  tblContacts.Open;
end;

procedure TdmDataTwo.MonitorLogTrace(Sender: TObject;
  CBInfo: pSQLTRACEDesc);
begin
  LogSQL(Self,CBInfo);
  Monitor.TraceList.Clear;
end;

end.
