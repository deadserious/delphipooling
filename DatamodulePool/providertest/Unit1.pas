unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataModuleServerPool, Provider, PoolDBClient, Grids, DBGrids,
  DB, DBClient, DataModuleDataAwarePool;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    ArcPoolConnection1: TArcPoolConnection;
    ArcDMDataAwarePool1 : TArcDMDataAwarePool;
    DBGrid1: TDBGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
                                    
implementation

uses Unit2;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ArcDMDataAwarePool1.Active := True;
  ArcPoolConnection1.Connected := True;
  ClientDataSet1.Active := True;
end;

end.
