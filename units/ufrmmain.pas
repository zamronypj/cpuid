unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
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
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses CpuInterface, cpu;

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
end;

end.

