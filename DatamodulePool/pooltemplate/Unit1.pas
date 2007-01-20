unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IWStandAloneServer, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    IWStandAloneServer1: TIWStandAloneServer;
    procedure IWStandAloneServer1DebugLog(ASender: TObject; ALog: String);
    procedure IWStandAloneServer1NewSession(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses IWGlobal, ServerController;

{$R *.dfm}

procedure TForm1.IWStandAloneServer1DebugLog(ASender: TObject;
  ALog: String);
begin
  Memo1.Lines.Add(ALog);
end;

procedure TForm1.IWStandAloneServer1NewSession(Sender: TObject);
begin
  IWServerController.Log('New User Session');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Add('Server started');
  Memo1.Lines.Add('Listening on port: ' + IntToStr(GServerController.Port));
end;

end.
