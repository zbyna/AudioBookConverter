program audiobookconverter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, tachartlazaruspkg, base, mplayercontrollaz
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Audio Book Converter';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmBase, frmBase);
  Application.Run;
end.  
 
 
