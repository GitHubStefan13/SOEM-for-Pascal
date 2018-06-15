unit Form_SOEM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm_Main }

  TForm_Main = class(TForm)
    bt_test: TButton;
    E_NPFDevice: TEdit;
    L_NPFDevice: TLabel;
    procedure bt_testClick(Sender: TObject);
  private

  public

  end;

var
  Form_Main: TForm_Main;

implementation

uses
  Soem_Interface;

{$R *.lfm}

{ TForm_Main }

procedure TForm_Main.bt_testClick(Sender: TObject);
var
  if_eth: string;
  p: PChar;

  slave_1 : ^ec_slave;
  result: integer;
begin
    if_eth := 'ENTER YOUR DEVICE NAME';
    p := PChar(if_eth);
    try
        if ec_init(p) = 1 then begin
            //ShowMessage(IntToStr(ec_config_init(false)));
            slave_1 := slaves[0];
        end;
    finally
        ShowMessage(IntToStr(ec_close));
    end;
end;

end.

