//---------------------------------------------------------------------------

#ifndef ServerControllerH
#define ServerControllerH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <IWServerControllerBase.hpp>
#include <IWApplication.hpp>
#include <IWInit.hpp>
#include "UserSessionUnit.h"
#include "DataModuleServerPool.hpp"

//---------------------------------------------------------------------------
class TIWServerController  : public TIWServerControllerBase
{
__published:	// IDE-managed Components
        TArcDMServerPool *Pool;

        void __fastcall IWServerControllerBaseNewSession(
           TIWApplication *ASession, TIWBaseForm *&VMainForm);
        void __fastcall PoolCreateDataModule(TDataModule *&ADataModule);
        void __fastcall PoolFreeDataModule(TDataModule *&ADataModule);

private:	// User declarations
public:		// User declarations
        __fastcall TIWServerController(TComponent* Owner);
};

//---------------------------------------------------------------------------
extern PACKAGE TIWServerController *IWServerController;
//---------------------------------------------------------------------------
#endif
