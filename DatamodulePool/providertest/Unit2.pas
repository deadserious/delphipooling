unit Unit2;

interface

uses
  SysUtils, Classes, DB, IBCustomDataSet, IBTable, IBDatabase, Provider,
  DataModuleDataAwarePool;

type
  TDataModule2 = class(TDataModule)
    IBDatabase1: TIBDatabase;
    IBTransaction1: TIBTransaction;
    IBTable1: TIBTable;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule2: TDataModule2;

implementation

{$R *.dfm}

end.
