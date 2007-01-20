//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "uMainForm.h"
#include "ServerController.h"
#include "uPooledModule.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompButton"
#pragma link "IWCompEdit"
#pragma link "IWCompLabel"
#pragma link "IWControl"
#pragma link "IWExtCtrls"
#pragma link "IWVCLBaseControl"
#pragma link "jpeg"
#pragma link "IWDBExtCtrls"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TIWAppForm(Owner)
{

}
//---------------------------------------------------------------------------

void setAsMainForm() {
  TfrmMain::SetAsMainForm(__classid(TfrmMain));
 }
#pragma startup setAsMainForm
void __fastcall TfrmMain::IWAppFormCreate(TObject *Sender)
{
  CurrentRec = 0;
  FillFields(CurrentRec);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnPriorClick(TObject *Sender)
{
  FillFields(CurrentRec--);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnNextClick(TObject *Sender)
{
  FillFields(CurrentRec++);
}
//---------------------------------------------------------------------------
void TfrmMain::FillFields(long NewRec)
{
  TIWServerController *sc;
  TdmPooled *dm;
  char c;

  sc = (TIWServerController*)GServerController;
  dm = (TdmPooled*)(sc->Pool->Lock());

  try
  {
    if(NewRec < 0)
    {
      NewRec = 0;
    }
    if(NewRec > dm->Table1->RecordCount-1) {
      NewRec = dm->Table1->RecordCount-1;
    }

    dm->Table1->First();
    dm->Table1->MoveBy(NewRec);
    edtCommon->Text = dm->Table1->FieldByName("Common_Name")->AsString;
    edtSpecies->Text = dm->Table1->FieldByName("Species Name")->AsString;
    imgFish->Picture->Assign(dm->Table1->FieldByName("Graphic"));
  }
  __finally
  {
    sc->Pool->Unlock(dm);
  }
}
