unit main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  AnchorDocking
  , AnchorDockOptionsDlg;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

uses base, playerform;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // to prevent annoying white flash before application quitting :-)
  DockMaster.ManualFloat(frmBase);
  DockMaster.ManualFloat(frmPlayer);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Self.Width:= 750;
  DockMaster.MakeDockSite(Self,[akBottom],admrpNone,False); // admrpChild throws cycle dected error with
                                                            //  DockMaster.ManualDock see frmBase.Create;
  DockMaster.OnShowOptions:=@ShowAnchorDockOptions;
  DockMaster.ShowHeader:=True;
  DockMaster.HeaderStyle:='ThemedCaption';
end;

end.
