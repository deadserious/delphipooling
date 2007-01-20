unit MainForm;

interface

uses
  Classes, SysUtils, IWAppForm, IWApplication, IWColor, IWTypes, DB,
  DBClient, Controls, IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl,
  IWControl, IWWebGrid, IWDBAdvWebGrid, IWCompButton,
  DataModuleDataAwarePool, IWCompEdit;

type
  TfrmMain = class(TIWAppForm)
    TIWDBAdvWebGrid1: TTIWDBAdvWebGrid;
    cdsMessages: TClientDataSet;
    MainPoolConnection: TArcPoolConnection;
    dsMessages: TDataSource;
    IWButton1: TIWButton;
    edtFrom: TIWEdit;
    btnChangeFrom: TIWButton;
    procedure MainPoolConnectionGetPool(ASender: TObject;
      var APool: TArcDMDataAwarePool);
    procedure IWButton1Click(Sender: TObject);
    procedure IWAppFormCreate(Sender: TObject);
    procedure IWAppFormRender(Sender: TObject);
    procedure btnChangeFromClick(Sender: TObject);
    procedure cdsMessagesBeforePost(DataSet: TDataSet);
  public
  end;

implementation

uses ServerController, EmailForm;

{$R *.dfm}


procedure TfrmMain.MainPoolConnectionGetPool(ASender: TObject;
  var APool: TArcDMDataAwarePool);
begin
  APool := IWServerController.PoolMain;
end;

procedure TfrmMain.IWButton1Click(Sender: TObject);
begin
  TfrmSend.Create(WebApplication).Show;
end;

procedure TfrmMain.IWAppFormCreate(Sender: TObject);
begin
  MainPoolConnection.Open;
  cdsMessages.Active := True;
end;

procedure TfrmMain.IWAppFormRender(Sender: TObject);
begin
  cdsMessages.Refresh;
  edtFrom.Text := UserSession.FromAddress;
end;

procedure TfrmMain.btnChangeFromClick(Sender: TObject);
begin
  UserSession.FromAddress := edtFrom.Text; // Keep in mind that this value is per user.
end;

procedure TfrmMain.cdsMessagesBeforePost(DataSet: TDataSet);
begin
  // although we don't need it for this demo, this is an example of your beforepost event issue.

  //if tbTariffTar_COmpany.AsInteger = 0 then
  //  tbTariffTar_Company.AsInteger := UserSession.CompanyID;
end;

initialization
  TfrmMain.SetAsMainForm;

end.
