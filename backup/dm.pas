unit dm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Menus, Dialogs;

type

  { TTPSDataModule }

  TTPSDataModule = class(TDataModule)
    ExitMenu: TMenuItem;
    FileMenu: TMenuItem;
    FileOpenDialog: TOpenDialog;
    FileOpenMenu: TMenuItem;
    MainMenu: TMainMenu;
  private

  public

  end;

var
  DataModule: TTPSDataModule;

implementation

{$R *.lfm}

end.

