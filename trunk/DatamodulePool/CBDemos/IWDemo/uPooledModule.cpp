//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "uPooledModule.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TdmPooled *dmPooled;
//---------------------------------------------------------------------------
__fastcall TdmPooled::TdmPooled(TComponent* Owner)
        : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
