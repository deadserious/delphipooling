// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DataModuleServerPool.pas' rev: 6.00

#ifndef DataModuleServerPoolHPP
#define DataModuleServerPoolHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ArcIWDMPooling.hpp>	// Pascal unit
#include <SyncObjs.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Datamoduleserverpool
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TDataModuleEvent)(Classes::TDataModule* &ADataModule);

class DELPHICLASS TArcDMServerPool;
class PASCALIMPLEMENTATION TArcDMServerPool : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	bool FSetToActive;
	Arciwdmpooling::TObjectPool* FPool;
	TDataModuleEvent FOnCreateDataModule;
	TDataModuleEvent FOnFreeDataModule;
	void __fastcall SetActive(const bool Value);
	void __fastcall SetPoolCount(const int Value);
	AnsiString __fastcall getVersion();
	void __fastcall SetVersion(const AnsiString Value);
	int __fastcall GetPoolCount(void);
	bool __fastcall GetActive(void);
	
protected:
	void __fastcall ErrorIfActive(void);
	void __fastcall ErrorIfNotActive(void);
	void __fastcall _OnCreateObject(System::TObject* Sender, System::TObject* &AObject);
	void __fastcall _OnDestroyObject(System::TObject* Sender, System::TObject* &AObject);
	virtual void __fastcall Loaded(void);
	
public:
	__fastcall virtual TArcDMServerPool(Classes::TComponent* AOwner);
	__fastcall virtual ~TArcDMServerPool(void);
	Classes::TDataModule* __fastcall Lock(void);
	void __fastcall Unlock(Classes::TDataModule* &ADataModule);
	int __fastcall CurrentThreadsInUse(void);
	
__published:
	__property TDataModuleEvent OnCreateDataModule = {read=FOnCreateDataModule, write=FOnCreateDataModule};
	__property TDataModuleEvent OnFreeDataModule = {read=FOnFreeDataModule, write=FOnFreeDataModule};
	__property int PoolCount = {read=GetPoolCount, write=SetPoolCount, nodefault};
	__property bool Active = {read=GetActive, write=SetActive, nodefault};
	__property AnsiString Version = {read=getVersion, write=SetVersion};
};


//-- var, const, procedure ---------------------------------------------------
#define COMPONENT_VERSION "2.0.0"
extern PACKAGE AnsiString __fastcall MakeUniqueDBName(AnsiString Prefix = "");

}	/* namespace Datamoduleserverpool */
using namespace Datamoduleserverpool;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DataModuleServerPool
