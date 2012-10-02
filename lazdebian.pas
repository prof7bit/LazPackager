{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazdebian;

interface

uses
  frmDebianOptions, lazdebianmain, LazPackagerBase, frmLazPackagerPreview, 
  frmdebianmakepackage, LazPackagerDebian, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lazdebianmain', @lazdebianmain.Register);
end;

initialization
  RegisterPackage('lazdebian', @Register);
end.
