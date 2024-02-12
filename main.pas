unit main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  AnchorDocking
  , AnchorDockOptionsDlg
  , XMLPropStorage;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ilMain: TImageList;
    mmPlayerImportChapters: TMenuItem;
    mmViewFlipForms: TMenuItem;
    mmActionsClearLog: TMenuItem;
    mmMain: TMainMenu;
    mmActions: TMenuItem;
    mmPlayerClearList: TMenuItem;
    mmActionsVideoOr: TMenuItem;
    mmActionsAudioOr: TMenuItem;
    mmActionsAudioMp3: TMenuItem;
    mmActionsQuit: TMenuItem;
    mmPlayer: TMenuItem;
    mmView: TMenuItem;
    mmPlayerPlay: TMenuItem;
    mmPlayerStop: TMenuItem;
    mmPlayerPause: TMenuItem;
    mmPlayerAdd: TMenuItem;
    mmPlayerDelete: TMenuItem;
    mmPLayerUpdate: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SaveLayout(Filename: string);
    procedure LoadLayout(Filename: string);
  private

  public
    layoutLoaded : Boolean;

  end;

var
  frmMain: TfrmMain;
  playerPosition:TAlign ;

implementation

uses base, playerform;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
   SaveLayout('layout.xml');
  // to prevent annoying white flash before application quitting :-)
  DockMaster.ManualFloat(frmBase);
  DockMaster.ManualFloat(frmPlayer);
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
     if not layoutLoaded then
        begin

          DockMaster.MakeDockable(frmBase);
          DockMaster.MakeDockable(frmPlayer);
          LoadLayout('layout.xml');
          layoutLoaded:=True;
          // it is needed to figure out if player is on the left or right
          // and store it in playerPosition see. line 50
          // frmBase.memLog.Append(Format('-frmPlayer.Left: %d frmBase.left: %d ',
          //                                 [frmPlayer.ClientToScreen(frmPlayer.BoundsRect.TopLeft).X
          //                                 ,frmBase.ClientToScreen(frmBase.BoundsRect.TopLeft).X]
          //                             )
          //                       );
          if (frmPlayer.ClientToScreen(frmPlayer.BoundsRect.TopLeft).X
               -  frmBase.ClientToScreen(frmBase.BoundsRect.TopLeft).X) > 0 then
              playerPosition := alRight
          else
              playerPosition := alLeft;
        end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin

  DockMaster.MakeDockSite(Self,[akBottom],admrpChild,False); // admrpChild throws cycle dected error with
                                                            //  DockMaster.ManualDock see frmBase.Create;
  DockMaster.OnShowOptions:=@ShowAnchorDockOptions;
  DockMaster.ShowHeader:=True;
  DockMaster.HeaderAlignTop:=1000;
  DockMaster.HeaderStyle:='ThemedCaption';
  DockMaster.ScaleOnResize:=false;
  DockMaster.AllowDragging:=false;
  Self.Width:= 723;
  layoutLoaded:=False;
end;

procedure TfrmMain.SaveLayout(Filename: string);
var
  XMLConfig: TXMLConfigStorage;
begin
  try
    // create a new xml config file
    XMLConfig:=TXMLConfigStorage.Create(Filename,false);
    try
      // save the current layout of all forms
      DockMaster.SaveLayoutToConfig(XMLConfig);
      XMLConfig.WriteToDisk;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg('Error',
        'Error saving layout to file '+Filename+':'#13+E.Message,mtError,
        [mbCancel],0);
    end;
  end;
end;

procedure tfrmmain.loadlayout(filename: string);
var
  XMLConfig: TXMLConfigStorage;
begin
  try
    // load the xml config file
    XMLConfig:=TXMLConfigStorage.Create(Filename,True);
    try
      // restore the layout
      // this will close unneeded forms and call OnCreateControl for all needed
      DockMaster.LoadLayoutFromConfig(XMLConfig,True);
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg('Error',
        'Error loading layout from file '+Filename+':'#13+E.Message,mtError,
        [mbCancel],0);
    end;
  end;
end;

end.

