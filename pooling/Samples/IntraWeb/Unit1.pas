unit Unit1;

interface

uses
  Classes, SysUtils, IWAppForm, IWApplication, IWTypes;

type
  TIWForm1 = class(TIWAppForm)
    procedure IWAppFormRender(Sender: TObject);
  public
  end;

implementation

{$R *.dfm}

uses
  ServerController;


procedure TIWForm1.IWAppFormRender(Sender: TObject);
var
  dm : TMyDataModule;
begin
  // This procedure shows how you might make use of a pooled DataModule.  Essentially
  // you just call the acquireDM function to get an instance, and releaseDM to
  // return it to the pool.  Make sure to reset any data state before using as other
  // threads may have repositioned cursors, etc.

  dm := AcquireDM;
  try
    dm.MyQuery.SQL.Text := 'Select * from MyTable');
    dm.MyQuery.Open;
    while not dm.MyQuery.EOF do
    begin
      txtDescription.Lines.Add( MyQuery.FieldByName('Description').AsString+'<br>' );
      dm.MyQuery.Next;
    end;
    dm.MyQuery.Close;
  finally
    ReleaseDM(dm);
  end;
end;

initialization
  TIWForm1.SetAsMainForm;



end.
 
