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
  frmMain.Hide;
  Application.CreateForm(TfrmPlayer, frmPlayer);
  frmPlayer.Hide;
  Application.CreateForm(TfrmBase, frmBase);
  frmBase.Hide;
  Application.Run;
end.  
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
