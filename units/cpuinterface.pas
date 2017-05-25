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
   end;

implementation

end.

