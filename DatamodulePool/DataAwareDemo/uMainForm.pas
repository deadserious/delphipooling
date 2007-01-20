unit uMainForm;

interface

uses
  Classes, SysUtils, IWAppForm, IWApplication, IWColor, IWTypes, DB,
  DBClient, Controls, IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl,
  IWControl, IWCompEdit, IWDBStdCtrls, IWExtCtrls, IWDBExtCtrls,
  DataModuleDataAwarePool;

type
  TIWForm1 = class(TIWAppForm)
    IWDBEdit1: TIWDBEdit;
    IWDBEdit2: TIWDBEdit;
    cdsBiolife: TClientDataSet;
    cdsBiolifeSPECIES_NO: TFloatField;
    cdsBiolifeCATEGORY: TStringField;
    cdsBiolifeCOMMON_NAME: TStringField;
    cdsBiolifeSPECIES_NAME: TStringField;
    cdsBiolifeLENGTH__CM_: TFloatField;
    cdsBiolifeLENGTH_IN: TFloatField;
    cdsBiolifeNOTES: TMemoField;
    cdsBiolifeGRAPHIC: TBlobField;
    dsBiolife: TDataSource;
    PoolConnection: TArcPoolConnection;
    IWDBImage1: TIWDBImage;
    IWDBNavigator1: TIWDBNavigator;
    procedure PoolConnectionGetPool(aSelf: TObject;
      var aPool: TArcDMDataAwarePool);
  public
  end;

implementation

uses UserSessionUnit, ServerController;

{$R *.dfm}


procedure TIWForm1.PoolConnectionGetPool(aSelf: TObject;
  var aPool: TArcDMDataAwarePool);
begin
  APool := IWServerController.Pool;
end;

initialization
  TIWForm1.SetAsMainForm;

end.
