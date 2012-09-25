unit lazdebianMakefile;

{$mode objfpc}{$H+}

interface

function LoadMakefileConfiguredTemplate: String;
procedure SaveMakefileConfiguredTemplate(Makefile: String);
function GetMakefileFinal: String;
function GetMakefileDefaultTemplate: String;

implementation
uses
  Classes, SysUtils;

function GetMakefileConfigured: String;
begin

end;

function LoadMakefileConfiguredTemplate: String;
begin

end;

procedure SaveMakefileConfiguredTemplate(Makefile: String);
begin

end;

function GetMakefileFinal: String;
begin

end;

function GetMakefileDefaultTemplate: String;
begin
  Result := '.PHONY : all'#10;
  Result += 'all:'#10;
  Result += #9'lazbuild ?PROJECT'#10;
  Result += #10;
  Result += '.PHONY : clean'#10;
  Result += 'clean:'#10;
  Result += #9'$(RM) -r lib'#10;
  Result += #9'$(RM) *.res'#10;
  Result += #9'$(RM) ?EXECUTABLE'#10;
  Result += #10;
  Result += '.PHONY : install'#10;
  Result += 'install:'#10;
  Result += #9'mkdir -p $(DESTDIR)($PREFIX)/bin'#10;
  Result += #9'install ?EXECUTABLE $(DESTDIR)($PREFIX)/bin/'#10;
  Result += #10;
end;


end.

