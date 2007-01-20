//---------------------------------------------------------------------------

#ifndef uMainFormH
#define uMainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompButton.hpp"
#include "IWCompEdit.hpp"
#include "IWCompLabel.hpp"
#include "IWControl.hpp"
#include "IWExtCtrls.hpp"
#include "IWVCLBaseControl.hpp"
#include "IWDBExtCtrls.hpp"

//---------------------------------------------------------------------------
class TfrmMain: public TIWAppForm
{
__published:	// IDE-managed Components
        TIWImage *imgFish;
        TIWButton *btnPrior;
        TIWButton *btnNext;
        TIWLabel *IWLabel1;
        TIWLabel *IWLabel2;
        TIWEdit *edtCommon;
        TIWEdit *edtSpecies;
        void __fastcall IWAppFormCreate(TObject *Sender);
        void __fastcall btnPriorClick(TObject *Sender);
        void __fastcall btnNextClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        long CurrentRec;
        __fastcall TfrmMain(TComponent* Owner);
        void FillFields(long NewRec);
};
//---------------------------------------------------------------------------
#endif
