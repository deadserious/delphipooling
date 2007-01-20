unit SQLLogger;

interface

uses Classes, SysUtils, SyncObjs, SQLExpr;

procedure LogSQL(Module : TDataModule; CBInfo: pSQLTRACEDesc);

implementation

uses WebCommon, DateUtils;

var
  LogCS : TCriticalSection;

procedure LogSQL(Module : TDataModule; CBInfo: pSQLTRACEDesc);
var
  txt : TextFile;
  sFilename : string;
  sDate : string;
begin
  DateTimeToString(sDate,'yyyy-dd-mm hh:nn:ss.zzz',Now);
  sFilename := DllFilePath+Module.ClassName+'.log';

  LogCS.Enter;
  try
    AssignFile(txt,sFilename);
    if FileExists(sFilename) then
      Append(txt)
    else
      Rewrite(txt);
    try
      WriteLn(txt,IntToStr(Integer(Pointer(Module)))+#9+sDate+#9#9+string(CBInfo.pszTrace));
    finally
      CloseFile(txt);
    end;
  finally
    LogCS.Leave;
  end;
end;

initialization
  LogCS := TCriticalSection.Create;

finalization
  LogCS.Free;
  
end.
 
