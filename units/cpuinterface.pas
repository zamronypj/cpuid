unit CpuInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

   ICPU = interface
      function cpuidSupported() : boolean;
      function vendorName() : string;
      function processorName() : string;
      function family() : byte;
      function model() : byte;
      function stepping() : byte;
      function hasFeature(const feature : string) : boolean;
   end;

implementation

end.

