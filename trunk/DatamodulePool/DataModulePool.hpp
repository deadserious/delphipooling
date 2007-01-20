// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DataModulePool.pas' rev: 6.00

#ifndef DataModulePoolHPP
#define DataModulePoolHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SyncObjs.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Datamodulepool
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TDataModuleEvent)(Classes::TDataModule* &ADataModule);

class DELPHICLASS TDataModuleThread;
class DELPHICLASS TDataModulePool;
class PASCALIMPLEMENTATION TDataModulePool : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	Classes::TList* FThreads;
	Classes::TBits* FLocked;
	Syncobjs::TCriticalSection* CS;
	bool FActive;
	int FPoolCount;
	TDataModuleEvent FOnCreateDataModule;
	TDataModuleEvent FOnFreeDataModule;
	void __fastcall SetActive(const bool Value);
	void __fastcall SetPoolCount(const int Value);
	AnsiString __fastcall getVersion();
	void __fastcall SetVersion(const AnsiString Value);
	
protected:
	void __fastcall ErrorIfActive(void);
	void __fastcall ErrorIfNotActive(void);
	void __fastcall ThreadTerminateEvent(System::TObject* Sender);
	
public:
	__fastcall virtual TDataModulePool(Classes::TComponent* AOwner);
	__fastcall virtual ~TDataModulePool(void);
	Classes::TDataModule* __fastcall Lock(void);
	void __fastcall Unlock(Classes::TDataModule* &ADataModule);
	int __fastcall CurrentThreadsInUse(void);
	
__published:
	__property TDataModuleEvent OnCreateDataModule = {read=FOnCreateDataModule, write=FOnCreateDataModule};
	__property TDataModuleEvent OnFreeDataModule = {read=FOnFreeDataModule, write=FOnFreeDataModule};
	__property int PoolCount = {read=FPoolCount, write=SetPoolCount, nodefault};
	__property bool Active = {read=FActive, write=SetActive, nodefault};
	__property AnsiString Version = {read=getVersion, write=SetVersion};
};


class PASCALIMPLEMENTATION TDataModuleThread : public Classes::TThread 
{
	typedef Classes::TThread inherited;
	
private:
	TDataModulePool* Pool;
	TDataModuleEvent FreeDM;
	TDataModuleEvent CreateDM;
	
protected:
	virtual void __fastcall DoFreeDM(void);
	virtual void __fastcall DoCreateDM(void);
	virtual void __fastcall Execute(void);
	
public:
	Classes::TDataModule* DM;
	bool ReadyToFree;
	__fastcall virtual TDataModuleThread(const TDataModulePool* aPool, const TDataModuleEvent aCreateDM, const TDataModuleEvent aFreeDM);
public:
	#pragma option push -w-inl
	/* TThread.Destroy */ inline __fastcall virtual ~TDataModuleThread(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
#define COMPONENT_VERSION "0.1.1b"
extern PACKAGE int TerminatedCount;
extern PACKAGE AnsiString __fastcall MakeUniqueDBName(AnsiString Prefix = "");

}	/* namespace Datamodulepool */
using namespace Datamodulepool;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DataModulePool
