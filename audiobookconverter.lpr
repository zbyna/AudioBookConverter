program audiobookconverter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, tachartlazaruspkg, anchordockpkg, base,
  playerform, main
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Audio Book Converter';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmBase, frmBase);
  Application.CreateForm(TfrmPlayer, frmPlayer);
  Application.Run;
end.  
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
