unit DataModuleUnit;

interface

uses
  {$IFDEF Linux}QForms, {$ELSE}Forms, {$ENDIF}
  SysUtils, Classes, DataModuleDataAwarePool, IBDatabase, DB,
  IBCustomDataSet, IBTable;

type
  TDataModule1 = class(TDataModule)
    PoolAdapter: TArcDMPoolAdapter;
    dbDemos: TIBDatabase;
    tblBiolife: TIBTable;
    transDefault: TIBTransaction;
  private
  public
  end;

var
  DataModule1 : TDataModule1;

implementation

{$R *.dfm}

end.
 
