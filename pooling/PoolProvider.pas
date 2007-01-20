unit PoolProvider;

interface

uses SysUtils, Classes, Provider, db, DBClient, DBConsts, Variants, DSIntf, MidConst
  DataModuleServerPool;

type
  TOnGetPoolEvent = procedure(aSender : TObject; var aPool : TArcDMServerPool);

  TPooledDatsetProvider = class(TDatasetProvider)
  private
    FPool: TArcDMServerPool;
    FOnGetPool: TGetPoolEvent;
    function GetPool: TArcDMServerPool;
  published
    property Pool : TArcDMServerPool read GetPool write FPool;
    property OnGetPool : TGetPoolEvent read FOnGetPool write FOnGetPool;
  end;

implementation

{ TPooledDatsetProvider }

function TPooledDatsetProvider.GetPool: TArcDMServerPool;
begin
  if (FPool = nil) and (csDesigning in ComponentState) then
    if Assigned(FOnGetPool) then
      FOnGetPool(Self, FPool);
  Result := FPool;
end;

end.
