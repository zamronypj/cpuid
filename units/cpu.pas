unit Cpu;

{$mode objfpc}{$H+}
{$asmmode intel}

interface

uses
  Classes, SysUtils, CpuInterface;

type

   TCPUIDResult = record
       eax : cardinal;
       ebx : cardinal;
       ecx : cardinal;
       edx : cardinal;
   end;

   TCPUVendorId = array [0..2] of cardinal;
   TCPUVendorIdStr = array [0..11] of ansichar;

   TCPUBasicInfo = record
     operation : cardinal;
     case boolean of
        true : (VendorIdStr : TCPUVendorIdStr);
        false: (VendorId : TCPUVendorId);
   end;

   TCPUBrand = array[0..15] of ansichar;
   TCPUBrandString = record
     case boolean of
       true : (res : TCPUIDResult);
       false : (brand : TCPUBrand);
   end;

   TCPUExtInfo = record
     extOperation : cardinal;
     extOperationAvail : boolean;
     brandStringAvail : boolean;
     brandString : string;
   end;

   { TCpu }

   TCpu = class(TInterfacedObject, ICPU)
   private
      function cpuidExec(const operation : cardinal) : TCPUIDResult;
      function getCPUBasicInfo() : TCPUBasicInfo;
      function getCPUExtInfo() : TCPUExtInfo;
   public
      function cpuidSupported() : boolean;
      function processorName() : string;
      function vendorName() : string;
      function hasFeature(const feature : string) : boolean;
      function family() : byte;
      function model() : byte;
      function stepping() : byte;
   end;

implementation

{ TCpu }

const
   CPUID_BIT = $200000;

   CPUID_OPR_BASIC_INFO = 0;
   CPUID_OPR_VERSION_FEATURE_INFO = 1;
   CPUID_OPR_EXTENDED_INFO = $80000000;
   CPUID_OPR_BRAND_INFO_AVAIL = $80000004;
   CPUID_OPR_BRAND_INFO_0 = $80000002;
   CPUID_OPR_BRAND_INFO_1 = $80000003;
   CPUID_OPR_BRAND_INFO_2 = $80000004;

{$IFDEF CPU32}
{------------start 32 bit architecture code ----------}

{**
 * Test availability of CPUID instruction
 *
 * @return boolean true if CPUID supported
 *}
function TCpu.cpuidSupported() : boolean;
var supported:boolean;
begin
   asm
      push eax
      push ecx

      //copy EFLAGS -> EAX
      pushfd
      pop eax

      //store original RFLAGS
      //so we can restore it later
      mov ecx, eax

      //change bit 21
      xor rax, CPUID_BIT

      //copy EAX -> EFLAGS
      push eax
      popfd

      //copy EFLAGS back to EAX
      pushfd
      pop eax

      //CPUID_BIT is reserved bit and cannot be changed
      //for 386 processor or lower
      //if we can change, it means we run on newer processor

      and eax, CPUID_BIT
      shr eax, 21
      mov supported, al

      //restore original EFLAGS
      push ecx
      popfd

      pop ecx
      pop eax
   end;
   result := supported;
end;


{*
* Run CPUID instruction
* @param cardinal operation store code for operation to be performed
*}
function TCpu.cpuidExec(const operation : cardinal) : TCPUIDResult;
var tmpEax, tmpEbx, tmpEcx, tmpEdx : cardinal;
begin
  asm
     push eax
     push ebx
     push ecx
     push edx

     mov eax, operation
     cpuid

     mov tmpEax, eax
     mov tmpEbx, ebx
     mov tmpEcx, ecx
     mov tmpEdx, edx

     pop edx
     pop ecx
     pop ebx
     pop eax
  end;
  result.eax := tmpEax;
  result.ebx := tmpEbx;
  result.ecx := tmpEcx;
  result.edx := tmpEdx;
end;
{------------end 32 bit architecture code ----------}
{$ENDIF}

{$IFDEF CPU64}
{------------start 64 bit architecture code ----------}

 {**
  * Test availability of CPUID instruction
  *
  * @return boolean true if CPUID supported
  *}
 function TCpu.cpuidSupported() : boolean;
 var supported:boolean;
 begin
    asm
       push rax
       push rcx

       //copy RFLAGS -> RAX
       pushfq
       pop rax

       //store original RFLAGS
       //so we can restore it later
       mov rcx, rax

       //change bit 21
       xor rax, CPUID_BIT

       //copy RAX -> RFLAGS
       push rax
       popfq

       //copy RFLAGS back to EAX
       pushfq
       pop rax

       //CPUID_BIT is reserved bit and cannot be changed
       //for 386 processor or lower
       //if we can change, it means we run on newer processor

       and rax, CPUID_BIT
       shr rax, 21
       mov supported, al

       //restore original RFLAGS
       push rcx
       popfq

       pop rcx
       pop rax
    end;
    result := supported;
 end;


{*
 * Run CPUID instruction
 * @param cardinal operation store code for operation to be performed
 *}
function TCpu.cpuidExec(const operation : cardinal) : TCPUIDResult;
var tmpEax, tmpEbx, tmpEcx, tmpEdx : cardinal;
begin
   asm
      push rax
      push rbx
      push rcx
      push rdx

      mov eax, operation
      cpuid

      mov tmpEax, eax
      mov tmpEbx, ebx
      mov tmpEcx, ecx
      mov tmpEdx, edx

      pop rdx
      pop rcx
      pop rbx
      pop rax
   end;
   result.eax := tmpEax;
   result.ebx := tmpEbx;
   result.ecx := tmpEcx;
   result.edx := tmpEdx;
end;
{------------end 64 bit architecture code ----------}
{$ENDIF}

function TCpu.getCPUBasicInfo() : TCPUBasicInfo;
var res:TCPUIDResult;
begin
   FillByte(result, sizeof(TCPUBasicInfo), 0);
   if not cpuidSupported() then
   begin
      exit;
   end;

   res := cpuidExec(CPUID_OPR_BASIC_INFO);
   result.operation   := res.eax;
   result.VendorId[0] := res.ebx;
   result.VendorId[1] := res.edx;
   result.VendorId[2] := res.ecx;
end;

function TCpu.getCPUExtInfo() : TCPUExtInfo;
var res:TCPUIDResult;
    br:TCPUBrandString;
begin
   FillByte(result, sizeof(TCPUExtInfo), 0);
   if not cpuidSupported() then
   begin
      exit;
   end;

  res := cpuidExec(CPUID_OPR_EXTENDED_INFO);
  result.extOperation := res.eax;
  result.extOperationAvail := (res.eax > CPUID_OPR_EXTENDED_INFO);
  result.brandStringAvail:=(res.eax >= CPUID_OPR_BRAND_INFO_AVAIL);
  result.brandString:='';
  if result.brandStringAvail then
  begin
    br.res := cpuidExec(CPUID_OPR_BRAND_INFO_0);
    result.brandString := result.brandString + br.brand;
    br.res := cpuidExec(CPUID_OPR_BRAND_INFO_1);
    result.brandString := result.brandString + br.brand;
    br.res := cpuidExec(CPUID_OPR_BRAND_INFO_2);
    result.brandString := result.brandString + br.Brand;
  end;
end;

function TCpu.processorName() : string;
var cpuExtInfo : TCPUExtInfo;
begin
  cpuExtInfo := getCPUExtInfo();
  result := cpuExtInfo.brandString;
end;

function TCpu.vendorName() : string;
var basicInfo : TCPUBasicInfo;
begin
  basicInfo := getCPUBasicInfo();
  result := basicInfo.VendorIdStr;
end;

function TCpu.hasFeature(const feature: string): boolean;
begin

  result := false;

end;

const
   CPUID_VERSION_EXTFAMILY_BIT   = $f00000;
   CPUID_VERSION_EXTMODEL_BIT    = $0f0000;
   CPUID_VERSION_PROCTYPE_BIT    = $003000;
   CPUID_VERSION_FAMILY_BIT      = $000f00;
   CPUID_VERSION_MODEL_BIT       = $0000f0;
   CPUID_VERSION_STEPPING_BIT    = $00000f;

function TCpu.family() : byte;
var res:TCPUIDResult;
    familyValue, extFamilyValue : byte;
begin
  res := cpuidExec(CPUID_OPR_VERSION_FEATURE_INFO);
  familyValue := (res.eax and CPUID_VERSION_FAMILY_BIT) shr 8;
  result := familyValue;
  if (familyValue = $0f) then
  begin
    extFamilyValue := (res.eax and CPUID_VERSION_EXTFAMILY_BIT) shr 20;
    result := extFamilyValue + familyValue;
  end;
end;

function TCpu.model(): byte;
var res:TCPUIDResult;
    modelValue, familyValue, extModelValue : byte;
begin
  res := cpuidExec(CPUID_OPR_VERSION_FEATURE_INFO);
  modelValue := (res.eax and CPUID_VERSION_MODEL_BIT) shr 4;
  familyValue := (res.eax and CPUID_VERSION_FAMILY_BIT) shr 8;
  result := modelValue;
  if ((familyValue = $06) or (familyValue = $0f)) then
  begin
    extModelValue := (res.eax and CPUID_VERSION_EXTMODEL_BIT) shr 16;
    result := (extModelValue shl 4) + modelValue;
  end;
end;

function TCpu.stepping(): byte;
var res:TCPUIDResult;
begin
  res := cpuidExec(CPUID_OPR_VERSION_FEATURE_INFO);
  result := res.eax and CPUID_VERSION_STEPPING_BIT;
end;

end.

