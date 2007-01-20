unit DataModule1;

interface

uses
  SysUtils, Classes, DBXpress, DB, SqlExpr, FMTBcd, DataModuleDataAwarePool;

type
  TdmDataOne = class(TDataModule)
    connDB: TSQLConnection;
    tblMessages: TSQLTable;
    tblMessagesTOADDRESS: TStringField;
    tblMessagesSUBJECT: TStringField;
    tblMessagesMESSAGETEXT: TMemoField;
    PoolAdapter: TArcDMPoolAdapter;
    spMessagesPK: TSQLStoredProc;
    tblMessagesPK: TIntegerField;
    procedure DataModuleCreate(Sender: TObject);
  private
    Monitor : TSQLMonitor;
    procedure MonitorLogTrace(Sender: TObject; CBInfo: pSQLTRACEDesc);
  public
    function NewMessagesPK : integer;
  end;

var
  dmDataOne: TdmDataOne;

implementation

uses IniFiles, WebCommon, SQLLogger;

{$R *.dfm}

procedure TdmDataOne.DataModuleCreate(Sender: TObject);
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
  tblMessages.Open;
end;

procedure TdmDataOne.MonitorLogTrace(Sender: TObject; CBInfo: pSQLTRACEDesc);
begin
  LogSQL(Self,CBInfo);  
  Monitor.TraceList.Clear;
end;

function TdmDataOne.NewMessagesPK: integer;
begin
  spMessagesPK.ExecProc;
  result := spMessagesPK.Params.ParamValues['ID'];
end;

end.
