unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Pooling;

type
  TWordThread = class(TThread)
  protected
    Word : string;
    Pool : TObjectPool;
    Listbox : TListBox;
    procedure Execute; override;
  public
    constructor Create(aPool : TObjectPool; lb : TListBox); reintroduce;
    procedure DoWordUpdate;
  end;

  TForm1 = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    lbThread1: TListBox;
    Label1: TLabel;
    lbThread2: TListBox;
    Label2: TLabel;
    lbThread3: TListBox;
    Label3: TLabel;
    lbThread4: TListBox;
    Label4: TLabel;
    lbThread5: TListBox;
    Label5: TLabel;
    lbThread6: TListBox;
    Label6: TLabel;
    lbThread7: TListBox;
    Label7: TLabel;
    lbThread8: TListBox;
    Label8: TLabel;
    lbThread9: TListBox;
    Label9: TLabel;
    chkAutoGrow: TCheckBox;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure chkAutoGrowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    WordThreads : TList;
    Pool : TObjectPool;
    procedure CreatePoolObject(Sender : TObject; var AObject : TObject);
    procedure DestroyPoolObject(Sender : TObject; var AObject : TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  SearchWords : array [0..26] of string =
      ('other','methods','of','improving','scalability','you','should','first',
       'learn','if','it','makes','sense','to','do','so','Profile','your','applications',
       'looking','for','the','source','each','bottleneck','fred','flintstone');

implementation

{$R *.dfm}

{ TWordThread }

constructor TWordThread.Create(aPool : TObjectPool; lb : TListBox);
begin
  inherited Create(True);
  ListBox := lb;
  Pool := aPool;
  Resume;
end;

procedure TWordThread.DoWordUpdate;
begin
  ListBox.Items.Insert(0,Word);
end;

procedure TWordThread.Execute;
  function SearchCriteria : string;
  begin
    // pick a random work to lookup.
    result := SearchWords[Random(24-1)];
  end;
var
  sl : TStringList;
  i, idx : integer;
  s : string;
begin
  for i := 0 to 1000 do
  begin
    if terminated then exit;
    sl := TStringList(Pool.Acquire);
    if sl = nil then
    begin
      Word := intToStr(i+1)+': (Pool Max Met)';
      beep;
      Synchronize(DoWordUpdate);
      sleep(1000);
      continue;
    end;

    try
      // Add a delay to keep an object locked longer  This is to show what happens
      // if the pool maxed out.  If you want to see it work without a delay, then
      // comment it out.
      Sleep(150);
      if terminated then exit;

      s := SearchCriteria;
      idx := sl.IndexOf(s);
      if idx >= 0 then
        Word := intToStr(i+1)+': '+sl[idx]+' ('+IntToStr(idx)+')'
      else
        word := intToStr(i+1)+': (no match for "'+s+'")';
    finally
      Pool.Release(sl);
    end;
    Synchronize(DoWordUpdate);
    Sleep(1);
  end;
end;

{ TForm1 }

procedure TForm1.btnStartClick(Sender: TObject);
begin
  Pool.PoolSize := 2;
  Pool.GrowToSize := 10;
  Pool.AutoGrow := chkAutoGrow.Checked;
  Pool.OnCreateObject := CreatePoolObject;
  Pool.OnDestroyObject := DestroyPoolObject;
  Pool.Start;

  WordThreads.Add(TWordThread.Create(Pool,lbThread1));
  WordThreads.Add(TWordThread.Create(Pool,lbThread2));
  WordThreads.Add(TWordThread.Create(Pool,lbThread3));
  WordThreads.Add(TWordThread.Create(Pool,lbThread4));
  WordThreads.Add(TWordThread.Create(Pool,lbThread5));
  WordThreads.Add(TWordThread.Create(Pool,lbThread6));
  WordThreads.Add(TWordThread.Create(Pool,lbThread7));
  WordThreads.Add(TWordThread.Create(Pool,lbThread8));
  WordThreads.Add(TWordThread.Create(Pool,lbThread9));

end;

procedure TForm1.btnStopClick(Sender: TObject);
var
  i : integer;
begin
  // Do this in two seperate loops so that all threads will stop working.  If you
  // call free first, the thread.free has a waitfor that will cause a delay before
  // moving on to the next thread.
  for i := 0 to WordThreads.Count-1 do
    TWordThread(WordThreads[i]).Terminate;
  for i := 0 to WordThreads.Count-1 do
    TWordThread(WordThreads[i]).Free;
  Pool.Stop;
  Caption := 'Object Pool Test Application - [Poolsize='+IntToStr(Pool.PoolSize)+']';
end;

procedure TForm1.CreatePoolObject(Sender: TObject; var AObject: TObject);
begin
  AObject := TStringList.Create;
  with TStringList(AObject) do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
    LoadFromFile('textfile.txt');
  end;
end;

procedure TForm1.DestroyPoolObject(Sender: TObject; var AObject: TObject);
begin
  AObject.Free;
end;

procedure TForm1.chkAutoGrowClick(Sender: TObject);
begin
  Pool.AutoGrow := chkAutoGrow.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  WordThreads := TList.Create;
  Pool := TObjectPool.Create;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  WordThreads.Free;
  Pool.Free;
end;

initialization
  Randomize;

end.
