//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ServerController.h"
#include "uPooledModule.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "DataModuleServerPool"
#pragma resource "*.dfm"

//---------------------------------------------------------------------------
__fastcall TIWServerController::TIWServerController(TComponent* Owner)
        : TIWServerControllerBase(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TIWServerController::IWServerControllerBaseNewSession(
      TIWApplication *ASession, TIWBaseForm *&VMainForm)
{
  ASession->Data = new TIWUserSession(NULL);
}
//---------------------------------------------------------------------------


void setServerController() {
  TIWServerController::SetServerControllerClass(__classid(TIWServerController));
}

#pragma startup setServerController


void __fastcall TIWServerController::PoolCreateDataModule(
      TDataModule *&ADataModule)
{
  ADataModule = new TdmPooled(NULL); 
}
//---------------------------------------------------------------------------
void __fastcall TIWServerController::PoolFreeDataModule(
      TDataModule *&ADataModule)
{
  ADataModule->Free();
}
//---------------------------------------------------------------------------
