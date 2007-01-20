unit UserSessionUnit;

{
  This is a DataModule where you can add components or declare fields that are specific to 
  ONE user. Instead of creating global variables, it is better to use this datamodule. You can then
  access the it using UserSession.
}
interface

uses
  IWUserSessionBase, SysUtils, Classes, DB, DBClient,
  DataModuleDataAwarePool;

type
  TIWUserSession = class(TIWUserSessionBase)
    procedure PoolConnectionGetPool(aSelf: TObject;
      var aPool: TArcDMDataAwarePool);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  ServerController;

{$R *.dfm}

procedure TIWUserSession.PoolConnectionGetPool(aSelf: TObject;
  var aPool: TArcDMDataAwarePool);
begin
  aPool := IWServerController.Pool;
end;

end.
