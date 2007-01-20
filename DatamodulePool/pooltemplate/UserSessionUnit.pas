unit UserSessionUnit;

{
  This is a DataModule where you can add components or declare fields that are specific to 
  ONE user. Instead of creating global variables, it is better to use this datamodule. You can then
  access the it using UserSession.
}
interface

uses
  IWUserSessionBase, SysUtils, Classes;

type
  TIWUserSession = class(TIWUserSessionBase)
    procedure IWUserSessionBaseCreate(Sender: TObject);
    procedure IWUserSessionBaseDestroy(Sender: TObject);
  private
    SMTPServer : string;
    CCList : TStringList;
  public
    FromAddress : string;
    CompanyID : integer;
    procedure SendEmail(ToAddress, Subject, Text: string);
  end;

implementation

uses InSMTP, IniFiles, WebCommon;

{$R *.dfm}

procedure TIWUserSession.IWUserSessionBaseCreate(Sender: TObject);
var
  ini : TIniFile;
begin
  CCList := TStringList.Create;
  FromAddress := 'default@arcanatech.com';
  ini := TIniFile.Create(DLLFilePath+'PoolTemplate.Ini');
  try
    SMTPServer := ini.ReadString('Mail','Server','');
    CCList.CommaText := ini.ReadString('Mail','cc','');
  finally
    ini.Free;
  end;
end;

procedure TIWUserSession.SendEmail(ToAddress, Subject, Text: string);
var
  smtp : TInSMTP;
begin
  // Note, I'm creating the mail component here manually, but you could (and probably should)
  // use the threaded version that you are using in your current project.  Just overwrite the
  // content of this procedure to launch the new thread.

  // Also, QuickSend does not support cc's, otherwise you might use the CCList variable here.

  smtp := TInSMTP.Create(nil);
  try
    smtp.QuickSend(SMTPServer, Subject, ToAddress, FromAddress,Text);
  finally
    smtp.Free;
  end;
end;

procedure TIWUserSession.IWUserSessionBaseDestroy(Sender: TObject);
begin
  CCList.Free;
end;

end.
