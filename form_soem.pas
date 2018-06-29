unit Form_SOEM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BigLED, strutils, EC_Resources, SOEM_Interface;

type

  { TForm_Main }

  RIOMapping = record
    Inputs: Array of Boolean;
    Outputs: Array of Boolean;
  end;

  TForm_Main = class(TForm)
    bt_test: TButton;
    bt_stop :TButton;
    E_NPFDevice: TEdit;
    L_Ausgange :TLabel;
    L_WC :TLabel;
    L_NPFDevice: TLabel;
    L_Log :TLabel;
    L_Eingange :TLabel;
    M_Log :TMemo;
    procedure bt_stopClick(Sender :TObject);
    procedure bt_testClick(Sender: TObject);
  private
    stop: boolean;
    inOP: boolean;
    IOMapping: RIOMapping;
    PIOmapping: PIOmap;

    procedure doIOMapping;

    function Setup(slave: Word): SmallInt;
  public

  end;

var
  Form_Main: TForm_Main;

implementation

{$R *.lfm}

{ TForm_Main }

function TForm_Main.Setup(slave: Word): SmallInt;
begin
  M_Log.Append('Configuring the Device...');
  result := 1;
end;

procedure TForm_Main.bt_testClick(Sender: TObject);
var
  if_eth: string;
  p: PChar;
  i: integer;
  wkc: Integer;
  slave: ec_slave;
  oloop, iloop: Cardinal;

  output, input: Byte;

  function Setup_ECDI8(slave: Word): SmallInt;
  begin
    result := Setup(slave);
  end;

begin
    if_eth := E_NPFDevice.Text;
    p := PChar(if_eth);
    GetMem(PIOmapping, sizeof(IOmap));
    try
        if ec_init(p) = 1 then begin
            M_Log.Append('Started interface');
            if ec_config_init(Byte(false)) = 1 then begin
                M_Log.Append('Found: ' + IntToStr(slavecounter^) + ' slave/s.');

                for i := 0 to EC_MAXSLAVE - 1 do begin
                   slave := slavelist^[i];

                   // Device: AXI E EC DI0 DO8 M12 6P
                   if (slave.eep_man = $84) AND (slave.eep_id = $2938D0) then begin
                      M_Log.Append('Found device: ' + slave.name + '  at Pos: ' + IntToStr(i));
                      POINTER(slave.PO2SOconfig) := @Setup_ECDI8;
                   end;
                end;
                stop := false;

                // Mapping of IO
                ec_config_map(PIOmapping);

                // Configure the Distributed Clock
                ec_configdc;

                // wait for all slaves to reach SAFE_OP state
                ec_statecheck(0, Ord(EC_STATE_SAFE_OP), EC_TIMEOUTSTATE);

                // Output Loop
                oloop := slavelist^[1].Obytes;
                if (oloop = 0) AND (slavelist^[1].Obits > 0) then oloop := 1;
                if oloop > 8 then oloop := 8;

                // Input Loop
                iloop := slavelist^[1].Ibytes;
                if (iloop = 0) AND (slavelist^[1].Ibits > 0) then iloop := 1;
                if iloop > 8 then iloop := 8;

                M_Log.Append('Request operational state for all slaves');
                slavelist^[0].State := Ord(EC_STATE_OPERATIONAL);

                // Send one valid process data to make outputs in slaves happy
                ec_send_processdata;
                ec_receive_processdata(EC_TIMEOUTRET);

                // Request Operational State for all Slaves
                ec_writestate(0);

                ec_statecheck(0, Ord(EC_STATE_OPERATIONAL), EC_TIMEOUTSTATE);
                wkc := ec_receive_processdata(EC_TIMEOUTSTATE);

                if slavelist^[0].state = Ord(EC_STATE_OPERATIONAL) then begin
                   M_Log.Append('Operational State reached for all slaves');

                   wkc  := 0;
                   inOP := TRUE;

                   bt_stop.Visible := true;
                   while NOT stop do begin
                     L_WC.Caption := 'Workcounter: ' + IntToStr(wkc);

                     SetLength(IOMapping.Outputs, oloop);
                     for i:= 0 to oloop - 1 do begin
                        output := (slavelist^[0].outputs^ shr (i)) mod 2;
                        IOMapping.Outputs[i] := Boolean(output);
                     end;

                     SetLength(IOMapping.Inputs, iloop);
                     for i:= 0 to iloop - 1 do begin
                        input := (slavelist^[0].inputs^ shr (i)) mod 2;
                        IOMapping.Inputs[i] := Boolean(input);
                     end;
                     doIOMapping;

                     ec_send_processdata();
                     wkc := ec_receive_processdata(EC_TIMEOUTSTATE);
                     Sleep(10);
                     Application.ProcessMessages;
                   end;

                   inOP := FALSE;
                end else begin
                   M_Log.Append('Not all slaves reached operational state');
                   ec_readstate();

                   for i:= 0 to slavecounter^  do begin
                       slave := slavelist^[i];
                       if slave.state <> Ord(EC_STATE_OPERATIONAL) then begin
                           M_Log.Append('Slave ' + IntToStr(i) + ' State = ' + IntToBin(slave.state, 16) + ' StatusCode = ' + IntToBin(slave.ALstatuscode, 16));
                       end;
                   end;
                end;

                M_Log.Append('Request Init State for all Slaves');
                slavelist^[0].state := ORD(EC_STATE_INIT);
                ec_writestate(0);
            end else begin
              M_Log.Append('No Slaves found');
              M_Log.Append('Closing interface...');
            end;
        end;
    finally
        ec_close;
        M_Log.Append('Closed interface');
        FreeMemAndNil(PIOmapping);
    end;
end;

procedure TForm_Main.doIOMapping;

  procedure MapInput;
  var
    i: integer;
  begin
    for i:=0 to Self.ComponentCount - 1 do begin
        
    end;
  end;

  procedure MapOutput;
  var
    i: integer;
  begin
    for i:=0 to Self.ComponentCount - 1 do begin
        
    end;
  end;

begin
  MapInput;
  MapOutput;
end;

procedure TForm_Main.bt_stopClick(Sender :TObject);
begin
  stop := true;
  L_WC.Caption := 'Workcounter:';
  if Sender is TButton then
     TButton(Sender).Visible := false;
end;

end.

