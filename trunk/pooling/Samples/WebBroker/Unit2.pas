unit Unit2;

interface

uses
  SysUtils, Classes, HTTPApp, Pooling;

type
  // We need to create a helper object when working in WebBroker.  We need some object
  // to house our OnCreateObject/OnDestroyObject event.  Unfortunately, the WebModule
  // is not adequate for this purpose as multiple copies of it will be created at runtime.
  // Instead we will create a TPoolHelper object and instantiate this in the initialization
  // section.  This allows us a convienient place to store the events and also to auto-start
  // the pool object.
  TPoolHelper = class(TObject)
  private
    procedure OnCreateObject(Sender : TObject; var AObject : TObject);
    procedure OnDestroyObject(Sender : TObject; var AObject : TObject);
  public
    Pool : TObjectPool;
    constructor Create;
    destructor Destroy; override;
  end;

  TWebModule2 = class(TWebModule)
    procedure WebModule2actDefaultAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
  public
  end;

// Note: TMyDataModule is a fictional DataModule that you would create to
//       house your page data.

// We create an AcquireDM method so that we don't have to typecast calls to Pool.Aquire.
function AquireDM : TMyDataModule;
procedure ReleaseDM(dm : TMyDataModule);

var
  WebModule2: TWebModule2;
  PoolHelper : TPoolHelper;

implementation

{$R *.dfm}

function AquireDM : TMyDataModule;
begin
  Result := TMyDataModule(PoolHelper.Pool.Acquire);
end;

procedure ReleaseDM(dm : TMyDataModule);
begin
  PoolHelper.Pool.Release(dm);
end;

{ TPoolHelper }

constructor TPoolHelper.Create;
begin
  Pool := TObjectPool.Create;
  Pool.PoolSize := 20;
  Pool.AutoGrow := False;
  Pool.OnCreateObject := OnCreateObject;
  Pool.OnDestroyObject := OnDestroyObject;
  Pool.Start;
end;

destructor TPoolHelper.Destroy;
begin
  Pool.Stop;
  Pool.Free;
  inherited;
end;

procedure TPoolHelper.OnCreateObject(Sender: TObject;
  var AObject: TObject);
begin
  AObject := TMyDataModule.Create(nil);
end;

procedure TPoolHelper.OnDestroyObject(Sender: TObject;
  var AObject: TObject);
begin
  AObject.Free;
end;

{ TWebModule2 }

procedure TWebModule2.WebModule2actDefaultAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
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
      Response.Content := Response.Content+
         MyQuery.FieldByName('Description').AsString+
         '<br>';
      dm.MyQuery.Next;
    end;
    dm.MyQuery.Close;
  finally
    ReleaseDM(dm);
  end;
end;

initialization
  PoolHelper := TPoolHelper.Create;

finalization
  PoolHelper.Free;

end.
