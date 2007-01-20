//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORM("uMainForm.cpp", frmMain); /* TIWAppForm: File Type */
USEFORM("ServerController.cpp", IWServerController); /* TIWServerControllerBase: File Type */
USEFORM("UserSessionUnit.cpp", IWUserSession); /* TIWUserSessionBase: File Type */
USEFORM("uPooledModule.cpp", dmPooled); /* TDataModule: File Type */
//---------------------------------------------------------------------------
#include <IWMain.hpp>
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
          Forms::Application->Initialize();
          Forms::Application->CreateForm(__classid(TFormIWMain), &FormIWMain);
                 Forms::Application->Run();
        }
        catch (Exception &exception)
        {

        }
        return 0;
}
//---------------------------------------------------------------------------

#pragma link "IWIndy_72_60.lib"