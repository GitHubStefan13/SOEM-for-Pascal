{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit EtherCAT_Master;

{$warn 5023 off : no warning about unused units}
interface

uses
  EC_Resources, SOEM_Interface, EtherCAT_types, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('EtherCAT_Master', @Register);
end.
