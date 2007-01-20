////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  unit uProfiler.pas                                                        //
//    Copyright 2003 by Arcana Technologies Incorporated                      //
//    Written By Jason Southwell                                              //
//                                                                            //
//  Description:                                                              //
//    This unit houses some common functions to assist with profiling delphi  //
//    applications.                                                           //
//                                                                            //
//  Updates:                                                                  //
//    04/03/2003 - Released to Open Source.  Functions include MarkStart,     //
//                 MarkFinish, AppMemoryUsage, AppThreadCount, and            //
//                 TotalMemoryLoad.
//                                                                            //
//  License:                                                                  //
//    This code is covered by the Mozilla Public License 1.1 (MPL 1.1)        //
//    Full text of this license can be found at                               //
//    http://www.opensource.org/licenses/mozilla1.1.html                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit uProfiler;

interface

uses SysUtils, Classes, Windows, PsAPI, TlHelp32;

// Call MarkStart when entering a procedure that you wish to time.  MarkStart
// will add to a mark stack allowing for recursive calls.  Make sure that
// each MarkStart is matched with a MarkFinished so that you don't run out of
// stack space.
procedure MarkStart;
// Call MarkFinish when leaving a procedure that you wish to time.
function MarkFinish : int64;
// Call AppMemoryUsage to learn the current memory useage from your application
function AppMemoryUsage : int64;
// Call AppThreadCount to learn the number of threads in use by your application.
function AppThreadCount : integer;
// Call TotalMemoryLoad to learn the percentage of memory being used on the computer.
function TotalMemoryLoad : integer;

implementation

uses DateUtils;

const MaxMarkListSize = 100;

threadvar MarkList : array[0..MaxMarkListSize-1] of TDateTime;
threadvar MarkIdx : integer;
threadvar initialized : boolean;

procedure MarkStart;
begin
  if not initialized then
  begin
    MarkIdx := -1;
    initialized := True;
  end;
  inc(MarkIdx);
  if MarkIdx >= MaxMarkListSize then
  begin
    dec(MarkIdx);
    raise Exception.Create('Too many recursive marks.');
  end;
  MarkList[MarkIdx] := Now;
end;

function MarkFinish : int64;
var
  d : TDateTime;
begin
  d := Now;
  if MarkIdx < 0 then
    raise Exception.Create('MarkStart must be called before MarkFinish');

  Result := MillisecondsBetween(d,MarkList[MarkIdx]);
  dec(MarkIdx);
end;

function AppMemoryUsage : int64;
var
  CurrentProcess : Cardinal;
  pmc : PROCESS_MEMORY_COUNTERS;
begin
  CurrentProcess := GetCurrentProcess;
  pmc.cb := sizeOf(pmc);
  if not GetProcessMemoryInfo(CurrentProcess,@pmc,sizeof(pmc)) then
    RaiseLastWin32Error;
  result := pmc.WorkingSetSize ;
end;

function AppThreadCount : integer;
var
  c : Cardinal;
  te : THREADENTRY32;
  iTotal, iAppTotal : LongWord;
  CurrentProcess : DWord;
  ThreadID : DWord;
begin
  iTotal := 0;
  iAppTotal := 0;
  try
    ThreadID := GetCurrentThreadId;

    c := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
    try

      CurrentProcess := GetCurrentProcessID;
      te.dwSize := sizeof(THREADENTRY32);

      if Thread32First(c,te) then
      repeat
        if te.th32OwnerProcessID=CurrentProcess then
          inc(iAppTotal);
        inc(iTotal);
      until not Thread32Next(c,te);


    finally
      CloseHandle(c);
    end;
  finally
    Result := iAppTotal;
    // iTotal holds the total threads in use on the system.  We could
    // return this value as well, but currently we just ignore the result. 
  end;
end;

function TotalMemoryLoad : integer;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwMemoryLoad;
end;

end.
