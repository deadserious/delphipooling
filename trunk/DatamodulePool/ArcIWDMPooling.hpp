// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ArcIWDMPooling.pas' rev: 6.00

#ifndef ArcIWDMPoolingHPP
#define ArcIWDMPoolingHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SyncObjs.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Arciwdmpooling
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TObjectEvent)(System::TObject* Sender, System::TObject* &AObject);

class DELPHICLASS TObjectPool;
class PASCALIMPLEMENTATION TObjectPool : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Syncobjs::TCriticalSection* CS;
	Classes::TList* ObjList;
	Classes::TBits* ObjInUse;
	bool FActive;
	bool FAutoGrow;
	int FGrowToSize;
	int FPoolSize;
	TObjectEvent FOnCreateObject;
	TObjectEvent FOnDestroyObject;
	int FUsageCount;
	bool FRaiseExceptions;
	
public:
	__fastcall virtual TObjectPool(void);
	__fastcall virtual ~TObjectPool(void);
	virtual void __fastcall Start(bool RaiseExceptions = false);
	virtual void __fastcall Stop(void);
	virtual System::TObject* __fastcall Acquire(void);
	virtual void __fastcall Release(System::TObject* item);
	__property bool Active = {read=FActive, nodefault};
	__property bool RaiseExceptions = {read=FRaiseExceptions, write=FRaiseExceptions, nodefault};
	__property int UsageCount = {read=FUsageCount, nodefault};
	__property int PoolSize = {read=FPoolSize, write=FPoolSize, nodefault};
	__property bool AutoGrow = {read=FAutoGrow, write=FAutoGrow, nodefault};
	__property int GrowToSize = {read=FGrowToSize, write=FGrowToSize, nodefault};
	__property TObjectEvent OnCreateObject = {read=FOnCreateObject, write=FOnCreateObject};
	__property TObjectEvent OnDestroyObject = {read=FOnDestroyObject, write=FOnDestroyObject};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Arciwdmpooling */
using namespace Arciwdmpooling;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ArcIWDMPooling
