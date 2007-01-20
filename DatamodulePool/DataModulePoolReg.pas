unit DataModulePoolReg;

interface

uses Classes;

procedure Register;

implementation

uses DataModulePool, DataModuleServerPool, DataModuleDataAwarePool;

procedure Register;
begin
  RegisterComponents('Data Pool', [TDataModulePool, TArcDMServerPool, TArcDMDataAwarePool, TArcDMPoolAdapter, TArcPoolConnection]);
end;


end.
