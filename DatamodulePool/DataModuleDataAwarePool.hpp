// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DataModuleDataAwarePool.pas' rev: 6.00

#ifndef DataModuleDataAwarePoolHPP
#define DataModuleDataAwarePoolHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Provider.hpp>	// Pascal unit
#include <Variants.hpp>	// Pascal unit
#include <Types.hpp>	// Pascal unit
#include <DBClient.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <Midas.hpp>	// Pascal unit
#include <ArcIWDMPooling.hpp>	// Pascal unit
#include <SyncObjs.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Datamoduledataawarepool
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TArcDMPoolAdapter;
class PASCALIMPLEMENTATION TArcDMPoolAdapter : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	Classes::TDataModule* FDataModule;
	
public:
	__fastcall virtual TArcDMPoolAdapter(Classes::TComponent* AOwner);
	
__published:
	__property Classes::TDataModule* DataModule = {read=FDataModule};
public:
	#pragma option push -w-inl
	/* TComponent.Destroy */ inline __fastcall virtual ~TArcDMPoolAdapter(void) { }
	#pragma option pop
	
};


class DELPHICLASS TArcDMDataAwarePool;
class PASCALIMPLEMENTATION TArcDMDataAwarePool : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	bool FSetToActive;
	Arciwdmpooling::TObjectPool* FPool;
	Classes::TDataModule* FDataModule;
	TArcDMPoolAdapter* FDataModuleAdapter;
	void __fastcall SetActive(const bool Value);
	void __fastcall SetPoolCount(const int Value);
	AnsiString __fastcall getVersion();
	void __fastcall SetVersion(const AnsiString Value);
	int __fastcall GetPoolCount(void);
	bool __fastcall GetActive(void);
	bool __fastcall GetAutoGrow(void);
	int __fastcall GetPoolMax(void);
	void __fastcall SetAutoGrow(const bool Value);
	void __fastcall SetPoolMax(const int Value);
	void __fastcall SetDataModuleAdapter(const TArcDMPoolAdapter* Value);
	
protected:
	void __fastcall ErrorIfActive(void);
	void __fastcall ErrorIfNotActive(void);
	void __fastcall _OnCreateObject(System::TObject* Sender, System::TObject* &AObject);
	void __fastcall _OnDestroyObject(System::TObject* Sender, System::TObject* &AObject);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall SetName(const AnsiString NewName);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	
public:
	__fastcall virtual TArcDMDataAwarePool(Classes::TComponent* AOwner);
	__fastcall virtual ~TArcDMDataAwarePool(void);
	Classes::TDataModule* __fastcall Lock(void);
	void __fastcall Unlock(Classes::TDataModule* &ADataModule);
	int __fastcall CurrentThreadsInUse(void);
	HRESULT __safecall AS_ApplyUpdates(const WideString ProviderName, const OleVariant Delta, int MaxErrors, /* out */ int &ErrorCount, OleVariant &OwnerData, OleVariant &AS_ApplyUpdates_result);
	HRESULT __safecall AS_DataRequest(const WideString ProviderName, const OleVariant Data, OleVariant &AS_DataRequest_result);
	HRESULT __safecall AS_Execute(const WideString ProviderName, const WideString CommandText, OleVariant &Params, OleVariant &OwnerData);
	HRESULT __safecall AS_GetParams(const WideString ProviderName, OleVariant &OwnerData, OleVariant &AS_GetParams_result);
	HRESULT __safecall AS_GetProviderNames(OleVariant &AS_GetProviderNames_result);
	HRESULT __safecall AS_GetRecords(const WideString ProviderName, int Count, /* out */ int &RecsOut, int Options, const WideString CommandText, OleVariant &Params, OleVariant &OwnerData, OleVariant &AS_GetRecords_result);
	HRESULT __safecall AS_RowRequest(const WideString ProviderName, const OleVariant Row, int RequestType, OleVariant &OwnerData, OleVariant &AS_RowRequest_result);
	
__published:
	__property TArcDMPoolAdapter* DataModuleAdapter = {read=FDataModuleAdapter, write=SetDataModuleAdapter};
	__property int PoolCount = {read=GetPoolCount, write=SetPoolCount, nodefault};
	__property bool Active = {read=GetActive, write=SetActive, nodefault};
	__property bool AutoGrow = {read=GetAutoGrow, write=SetAutoGrow, nodefault};
	__property int PoolMax = {read=GetPoolMax, write=SetPoolMax, nodefault};
	__property AnsiString Version = {read=getVersion, write=SetVersion};
private:
	void *__IAppServer;	/* Midas::IAppServer */
	
public:
	operator IAppServer*(void) { return (IAppServer*)&__IAppServer; }
	
};


typedef void __fastcall (__closure *TOnGetPoolEvent)(System::TObject* aSelf, TArcDMDataAwarePool* aPool);

class DELPHICLASS TArcPoolConnection;
class PASCALIMPLEMENTATION TArcPoolConnection : public Dbclient::TCustomRemoteServer 
{
	typedef Dbclient::TCustomRemoteServer inherited;
	
private:
	TArcDMDataAwarePool* FPool;
	TOnGetPoolEvent FOnGetPool;
	bool FConnected;
	
protected:
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	virtual OleVariant __fastcall GetServerList();
	virtual void __fastcall DoConnect(void);
	virtual void __fastcall DoDisconnect(void);
	virtual bool __fastcall GetConnected(void);
	virtual void __fastcall GetProviderNames(Classes::TGetStrProc Proc);
	
public:
	virtual Midas::_di_IAppServer __fastcall GetServer();
	
__published:
	__property TArcDMDataAwarePool* Pool = {read=FPool, write=FPool};
	__property TOnGetPoolEvent OnGetPool = {read=FOnGetPool, write=FOnGetPool};
	__property Connected  = {default=0};
	__property AfterConnect ;
	__property BeforeConnect ;
	__property AfterDisconnect ;
	__property BeforeDisconnect ;
public:
	#pragma option push -w-inl
	/* TCustomRemoteServer.Create */ inline __fastcall virtual TArcPoolConnection(Classes::TComponent* AOwner) : Dbclient::TCustomRemoteServer(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomConnection.Destroy */ inline __fastcall virtual ~TArcPoolConnection(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
#define COMPONENT_VERSION "2.0.0"
extern PACKAGE AnsiString __fastcall MakeUniqueDBName(AnsiString Prefix = "");

}	/* namespace Datamoduledataawarepool */
using namespace Datamoduledataawarepool;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DataModuleDataAwarePool
