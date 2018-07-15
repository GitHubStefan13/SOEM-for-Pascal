unit Form_SOEM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SOEM_Interface;

type

  { TForm_Main }

  TForm_Main = class(TForm)
    bt_test: TButton;
    bt_stop :TButton;
    E_NPFDevice: TEdit;
    L_Ausgange :TLabel;
    L_SPSTime :TLabel;
    L_WC :TLabel;
    L_NPFDevice: TLabel;
    L_Log :TLabel;
    L_Eingange :TLabel;
    L_PSC :TLabel;
    M_Log :TMemo;
    procedure bt_stopClick(Sender :TObject);
    procedure bt_testClick(Sender: TObject);
    procedure LED_Click(Sender :TObject);
  private
    procedure doIOMapping;

    function Setup(slave: Word): SmallInt;
  public

  end;

var
  Form_Main: TForm_Main;
  EC_Master: EtherCAT;
  MySPS    : SPS;
  Stop     : Boolean;

implementation

{$R *.lfm}

{ TForm_Main }

function TForm_Main.Setup(slave: Word): SmallInt;
begin
  M_Log.Append('Configuring the Device...');
  result := 1;
end;

procedure TForm_Main.bt_testClick(Sender: TObject);

  function Setup_ECDI8(slave: Word): SmallInt;
  begin
    result := Setup(slave);
  end;

  function handle_msg(): integer;
  begin
    while EC_Master.inOP do begin
      EC_Master.process_msg;
    end;

    result := 0;
  end;

begin
  EC_Master := EtherCAT.Create;
  EC_Master.ifname := E_NPFDevice.Text;
  POINTER(EC_Master.Arr_PO2SOConfig[1]) := @Setup_ECDI8;
  EC_Master.init;

  MySPS := SPS.Create(true);
  POINTER(MySPS.process_msg) := @handle_msg;
  MySPS.Start;

  Stop := false;
  bt_stop.Visible := true;
  // Should be in a Thread of the Form
  while NOT Stop do begin
    L_WC.Caption  := 'Workcounter: ' + IntToStr(EC_Master.wkc);
    L_PSC.Caption := 'Processcounter: ' + FloatToStr(EC_Master.psc);
    L_SPSTime.Caption := 'Last SPSTime(ms): ' + FloatToStr(EC_Master.spstime * 1000);
    M_Log.Lines := EC_Master.SPSLog;

    doIOMapping;

    Application.ProcessMessages;
    Sleep(10);
  end;

  MySPS.Suspend;
  M_Log.Lines := EC_Master.SPSLog;
  MySPS.Terminate;
end;

procedure TForm_Main.LED_Click(Sender :TObject);
//var
//  ec_c, temp: Byte;

begin
  // Example for Toogling Outputs...
  //if Tag < Length(EC_Master.IOMapping.Outputs) then begin
  //  ec_c := $1 shl Tag;
  //  temp := Byte(EC_Master.PIOmapping^[0]);
  //
  //  if Lit then
  //     temp -= ec_c
  //  else
  //     temp += ec_c;
  //
  //  EC_Master.PIOmapping^[0] := Char(temp);
  //end;
end;

procedure TForm_Main.doIOMapping;

  procedure MapInput;
  begin
    // Example for displaying Inputs
    //if Length(EC_Master.IOMapping.Inputs) > Tag then
    //   Lit := EC_Master.IOMapping.Inputs[Tag];
  end;

  procedure MapOutput;
  begin
    // Example for displaying Outputs
    //if Length(EC_Master.IOMapping.Outputs) > Tag then
    //   Lit := EC_Master.IOMapping.Outputs[Tag];
  end;

begin
  MapInput;
  MapOutput;
end;

procedure TForm_Main.bt_stopClick(Sender :TObject);
begin
  Stop := true;
  L_WC.Caption := 'Workcounter:';
  if Sender is TButton then
     TButton(Sender).Visible := false;
end;

end.

