unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
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
end;

end.

