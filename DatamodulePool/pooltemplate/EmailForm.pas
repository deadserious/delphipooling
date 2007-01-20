unit EmailForm;

interface

uses
  Classes, SysUtils, IWAppForm, IWApplication, IWColor, IWTypes,
  IWCompButton, IWCompMemo, IWDBStdCtrls, IWCompEdit, Controls,
  IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl, IWControl,
  IWCompListbox, DB, DBClient, DataModuleDataAwarePool;

type
  TfrmSend = class(TIWAppForm)
    lcbAddresses: TIWDBLookupComboBox;
    edtSender: TIWDBEdit;
    memMessage: TIWDBMemo;
    btnSend: TIWButton;
    btnCancel: TIWButton;
    cdsMessages: TClientDataSet;
    MainPoolConnection: TArcPoolConnection;
    dsMessages: TDataSource;
    cdsContacts: TClientDataSet;
    SmallPoolConnection: TArcPoolConnection;
    dsContacts: TDataSource;
    cdsPK: TClientDataSet;
    cdsMessagesPK: TIntegerField;
    cdsMessagesTOADDRESS: TStringField;
    cdsMessagesSUBJECT: TStringField;
    cdsMessagesMESSAGETEXT: TMemoField;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure IWAppFormCreate(Sender: TObject);
    procedure SmallPoolConnectionGetPool(ASender: TObject;
      var APool: TArcDMDataAwarePool);
    procedure MainPoolConnectionGetPool(ASender: TObject;
      var APool: TArcDMDataAwarePool);
    procedure cdsMessagesBeforePost(DataSet: TDataSet);
  public
  end;

implementation

uses ServerController, UserSessionUnit, DataModule1;

{$R *.dfm}

procedure TfrmSend.btnCancelClick(Sender: TObject);
begin
  cdsMessages.Cancel;
  cdsMessages.CancelUpdates;
  Release;
end;

procedure TfrmSend.btnSendClick(Sender: TObject);
var
  dm : TDataModule;
begin
  //UserSession.SendEmail(lcbAddresses.Text,edtSender.Text,memMessage.Text);

  dm := MainPoolConnection.Pool.Lock;
  try
    cdsMessagesPK.AsInteger := TdmDataOne(dm).NewMessagesPK;
  finally
    MainPoolConnection.Pool.Unlock(dm);
  end;
  cdsMessages.Post;
  cdsMessages.ApplyUpdates(0);
  Release;
end;

procedure TfrmSend.IWAppFormCreate(Sender: TObject);
begin
  MainPoolConnection.Open;
  SmallPoolConnection.Open;
  cdsMessages.Active := True;
  cdsContacts.Active := True;
  cdsMessages.Append;
end;

procedure TfrmSend.SmallPoolConnectionGetPool(ASender: TObject;
  var APool: TArcDMDataAwarePool);
begin
  APool := IWServerController.PoolSmall;
end;

procedure TfrmSend.MainPoolConnectionGetPool(ASender: TObject;
  var APool: TArcDMDataAwarePool);
begin
  APool := IWServerController.PoolMain;
end;

procedure TfrmSend.cdsMessagesBeforePost(DataSet: TDataSet);
begin
//  Abort;
end;

end.
