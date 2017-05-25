unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, CpuInterface;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    labelFeatures: TLabel;
    labelProcessorFeatures: TLabel;
    labelStepping: TLabel;
    labelProcessorStepping: TLabel;
    labelVendor: TLabel;
    labelCpuid: TLabel;
    labelName: TLabel;
    labelFamily: TLabel;
    labelModel: TLabel;
    labelProcessorModel: TLabel;
    labelProcessorFamily: TLabel;
    labelProcessorName: TLabel;
    labelCPUIDSupport: TLabel;
    labelVendorName: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    function supportedFeature(const cpuIntf : ICPU; const feature: string) : string;
    function reportAllSupportedFeatures(const cpuIntf : ICPU) : string;
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses cpu;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var processor : ICPU;
begin
  processor := TCpu.Create();
  labelCPUIDSupport.Caption:= 'not supported';
  if (processor.cpuidSupported()) then
  begin
    labelCPUIDSupport.Caption:= 'supported';
  end;
  labelVendorName.Caption := processor.vendorName();
  labelProcessorName.Caption := processor.processorName();
  labelProcessorFamily.Caption := inttoStr(processor.family());
  labelProcessorModel.Caption := inttoStr(processor.model());
  labelProcessorStepping.Caption := inttoStr(processor.stepping());
  labelProcessorFeatures.Caption := reportAllSupportedFeatures(processor);
end;

{**
 * Return feature string if it is supported
 *}
function TfrmMain.supportedFeature(const cpuIntf: ICPU; const feature: string): string;
begin
  result := '';
  if (cpuIntf.hasFeature(feature)) then
  begin
    result := feature;
  end;
end;

function TfrmMain.reportAllSupportedFeatures(const cpuIntf: ICPU): string;
var features : array [0..58] of string = (
      'SSE3', 'PCLMULQDQ', 'DTES64', 'MONITOR', 'DS-CPL', 'VMX',
      'SMX', 'EIST', 'TM2', 'SSSE3', 'CNXT-ID', 'SDBG', 'FMA',
      'CMPXCHG16B', 'xTPR', 'PDCM', 'PCID', 'DCA', 'SSE4_1',
      'SSE4_2', 'x2APIC', 'MOVBE', 'POPCNT', 'TSC-DEADLINE',
      'AES', 'XSAVE', 'OSXSAVE', 'AVX', 'F16C',
      'RDRAND', 'FPU', 'VME', 'DE', 'PSE', 'TSC', 'MSR',
      'PAE', 'MCE', 'CX8', 'APIC', 'SEP', 'MTRR', 'PGE',
      'MCA', 'CMOV', 'PAT', 'PSE-36', 'PSN', 'CLFSH', 'DS',
      'ACPI', 'MMX', 'FXSR', 'SSE', 'SSE2', 'SS', 'HTT', 'TM', 'PBE'
    );

    i:integer;
    tmp:string;
begin
  result:='';
  for i := 0 to 57 do
  begin
    tmp:= supportedFeature(cpuIntf, features[i]);
    if (length(tmp) > 0) then
    begin
      result:= result + tmp + ' ';
    end;
  end;
end;

end.

