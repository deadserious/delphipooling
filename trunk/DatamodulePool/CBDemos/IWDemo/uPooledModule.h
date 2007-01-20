//---------------------------------------------------------------------------

#ifndef uPooledModuleH
#define uPooledModuleH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <DB.hpp>
#include <DBTables.hpp>
//---------------------------------------------------------------------------
class TdmPooled : public TDataModule
{
__published:	// IDE-managed Components
        TSession *Session1;
        TDatabase *Database1;
        TTable *Table1;
private:	// User declarations
public:		// User declarations
        __fastcall TdmPooled(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmPooled *dmPooled;
//---------------------------------------------------------------------------
#endif
