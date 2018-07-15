{|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
|        DO NOT CHANGE ANY VALUES WITHOUT KNOWING EXACTLY WHAT YOU DO!         |
|                                                                              |
|   The Records are carefully crafted around the DLL, changing just a single   |
|   value might(will) break the whole Structure and render it useless.         |
|                                                                              |
|                          YOU HAVE BEEN WARNED!                               |
|                                                                              |
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||}

unit SOEM_Interface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, dynlibs, EC_Resources, EtherCAT_types;

const
  {$ifdef Win32}
    DLL = 'DLL\x86\soem.dll';
  {$else}
      {$ifdef Win64}
        DLL = 'DLL\x64\soem.dll';
      {$else}
        DLL = 'DLL\linux\soem.lib';
      {$endif}
  {$endif}

type

  { EtherCAT }

  EtherCAT = class
    private
      // Handling our DLL
      dllHandle: TLibHandle;
      // Help variables used for looping over the IO's
      oloop, iloop: integer;
      // Cycle Time for one Processdata in ns
      CycleTime: LongWord;

      // ************************ Common Functions ****************************

      {** Initialise lib in single NIC mode
       * @param[in] ifname   = Dev name, f.e. "eth0"
       * @return >0 if OK
       *}
      ec_init:  function(const ifname: PChar):Integer; cdecl;
      {** Close the lib
       *}
      ec_close: procedure ((*void: explicit NOTHING*)); cdecl;

      // ************************ Configuration Functions **********************

      {** Locate DC slaves, measure propagation delays.
       *
       * @return boolean if slaves are found with DC
       *}
      ec_configdc: function: boolean; cdecl;
      {** Set DC of slave to fire sync0 at CyclTime interval with CyclShift offset.
      *
      * @param [in] slave            Slave number.
      * @param [in] act              TRUE = active, FALSE = deactivated
      * @param [in] CyclTime         Cycltime in ns.
      * @param [in] CyclShift        CyclShift in ns.
      *}
      ec_dcsync0:  procedure(slave: Word; act: boolean; CyclTime: LongWord; CyclShift: Integer); cdecl;
      {** Set DC of slave to fire sync0 and sync1 at CyclTime interval with CyclShift offset.
       *
       * @param [in] slave            Slave number.
       * @param [in] act              TRUE = active, FALSE = deactivated
       * @param [in] CyclTime0        Cycltime SYNC0 in ns.
       * @param [in] CyclTime1        Cycltime SYNC1 in ns. This time is a delta time in relation to
                                      the SYNC0 fire. If CylcTime1 = 0 then SYNC1 fires a the same time
                                      as SYNC0.
       * @param [in] CyclShift        CyclShift in ns.
       *}
      ec_dcsync01: procedure(slave: Word; act: boolean; CyclTime0, CyclTime1: LongWord; CyclShift: Integer); cdecl;
      {** Enumerate and init all slaves.
       *
       * @param[in] usetable     = TRUE when using configtable to init slaves, FALSE otherwise
       * @return Workcounter of slave discover datagram = number of slaves found
       *}
      ec_config_init: function(usetable :Byte):Integer; cdecl;
      {** Map all PDOs from slaves to IOmap with Outputs/Inputs
       * in sequential order (legacy SOEM way).
       *
       * @param[out] pIOmap     = pointer to IOmap
       * @return IOmap size
       *}
      ec_config_map:  function(pIOmap: PIOmap):Integer; cdecl;
      {** Enumerate / map and init all slaves.
      *
      * @param[in] usetable    = TRUE when using configtable to init slaves, FALSE otherwise
      * @param[out] pIOmap     = pointer to IOmap
      * @return Workcounter of slave discover datagram = number of slaves found
      *}
      ec_config_overlap:  function(usetable :Byte; pIOmap: PIOmap):Integer; cdecl;
      {** Map all PDOs in one group of slaves to IOmap with Outputs/Inputs
       * in sequential order (legacy SOEM way).
       *
       * @param[out] pIOmap     = pointer to IOmap
       * @param[in]  group      = group to map, 0 = all groups
       * @return IOmap size
       *}
      ec_config_map_group:  function(pIOmap: PIOmap; group: Byte):Integer; cdecl;
      {** Map all PDOs in one group of slaves to IOmap with Outputs/Inputs
      * overlapping. NOTE: Must use this for TI ESC when using LRW.
      *
      * @param[out] pIOmap     = pointer to IOmap
      * @param[in]  group      = group to map, 0 = all groups
      * @return IOmap size
      *}
      ec_config_overlap_map_group:  function(pIOmap: PIOmap; group: Byte):Integer; cdecl;

      // ***************************** Error Handling **************************

      {** Check if error list has entries.
       *
       * @param[in] context        = context struct
       * @return TRUE if error list contains entries.
       *}
      ec_iserror: function((*void: explicit NOTHING*)):Integer; cdecl;
      {** Report packet error
       *
       * @param[in]  context        = context struct
       * @param[in]  Slave      = Slave number
       * @param[in]  Index      = Index that generated error
       * @param[in]  SubIdx     = Subindex that generated error
       * @param[in]  ErrorCode  = Error code
       *}
      ec_packeterror: procedure(Slave, Index :Cardinal; SubIdx: Byte; ErrorCode: Cardinal); cdecl;
      {** Recover slave.
       *
       * @param[in] slave   = slave to recover
       * @param[in] timeout = local timeout f.e. EC_TIMEOUTRET3
       * @return >0 if successful
       * @see ecx_recover_slave
       *}
      ec_recover_slave:  function(slave: Word; timeout: Integer):Integer; cdecl;
      {** Reconfigure slave.
       *
       * @param[in] slave   = slave to reconfigure
       * @param[in] timeout = local timeout f.e. EC_TIMEOUTRET3
       * @return Slave state
       * @see ecx_reconfig_slave
       *}
      ec_reconfig_slave:  function(slave: Word; timeout: Integer):Integer; cdecl;

      // ******************************* States ********************************

      {** Read all slave states in ec_slave.
       * @return lowest state found
       *}
      ec_readstate: function((*void: explicit NOTHING*)):Integer; cdecl;
      {** Check actual slave state.
       * This is a blocking function.
       * @param[in] slave       = Slave number, 0 = all slaves
       * @param[in] reqstate    = Requested state
       * @param[in] timeout     = Timout value in us
       * @return Requested state, or found state after timeout.
       *}
      ec_statecheck:  function(slave, reqstate :Cardinal; timeout: integer):Cardinal; cdecl;
      {** Write slave state, if slave = 0 then write to all slaves.
       * The function does not check if the actual state is changed.
       * @param[in] slave = Slave number, 0 = master
       * @return 0
       *}
      ec_writestate: function(slave :Word):Integer; cdecl;

      // ************************** Receiving Data *****************************

      {* Receive processdata from slaves.
       * @See ec_receive_processdata_group.
       *}
      ec_receive_processdata: function(timeout: integer):Integer; cdecl;
      {* Receive processdata from slaves.
       * Second part from ec_send_processdata().
       * Received datagrams are recombined with the processdata with help from the stack.
       * If a datagram contains input processdata it copies it to the processdata structure.
       * @param[in]  group          = group number
       * @param[in]  timeout        = Timeout in us.
       * @return Work counter.
       *}
      ec_receive_processdata_group: function(group: Byte; timeout: integer):Integer; cdecl;

      // *************************** Sending Data ******************************

      {** Transmit processdata to slaves.
       * @see ec_send_processdata_group
       *}
      ec_send_processdata: function:Integer; cdecl;
      {** Transmit processdata to slaves.
       * Uses LRW, or LRD/LWR if LRW is not allowed (blockLRW).
       * Both the input and output processdata are transmitted.
       * The outputs with the actual data, the inputs have a placeholder.
       * The inputs are gathered with the receive processdata function.
       * In contrast to the base LRW function this function is non-blocking.
       * If the processdata does not fit in one datagram, multiple are used.
       * In order to recombine the slave response, a stack is used.
       * @param[in]  group          = group number
       * @return >0 if processdata is transmitted.
       *}
      ec_send_processdata_group: function(group: Byte):Integer; cdecl;
      {** Transmit processdata to slaves.
       * @see ec_send_overlap_processdata_group
       *}
      ec_send_overlap_processdata: function():Integer; cdecl;
      {** Transmit processdata to slaves.
      * Uses LRW, or LRD/LWR if LRW is not allowed (blockLRW).
      * Both the input and output processdata are transmitted in the overlapped IOmap.
      * The outputs with the actual data, the inputs replace the output data in the
      * returning frame. The inputs are gathered with the receive processdata function.
      * In contrast to the base LRW function this function is non-blocking.
      * If the processdata does not fit in one datagram, multiple are used.
      * In order to recombine the slave response, a stack is used.
      * @param[in]  context        = context struct
      * @param[in]  group          = group number
      * @return >0 if processdata is transmitted.
      *}
      ec_send_overlap_processdata_group: function(group: Byte):Integer; cdecl;

      // ************************ CAN over EtherCAT ****************************

      {** Report SDO error.
       *
       * @param[in]  Slave      = Slave number
       * @param[in]  Index      = Index that generated error
       * @param[in]  SubIdx     = Subindex that generated error
       * @param[in]  AbortCode  = Abortcode, see EtherCAT documentation for list
       *}
      ec_SDOerror: procedure(Slave, Index: Word; SubIdx: Byte; AbortCode: Integer);
      {** CoE SDO read, blocking. Single subindex or Complete Access.
       *
       * Only a "normal" upload request is issued. If the requested parameter is <= 4bytes
       * then a "expedited" response is returned, otherwise a "normal" response. If a "normal"
       * response is larger than the mailbox size then the response is segmented. The function
       * will combine all segments and copy them to the parameter buffer.
       *
       * @param[in]  slave      = Slave number
       * @param[in]  index      = Index to read
       * @param[in]  subindex   = Subindex to read, must be 0 or 1 if CA is used.
       * @param[in]  CA         = FALSE = single subindex. TRUE = Complete Access, all subindexes read.
       * @param[in,out] psize   = Size in bytes of parameter buffer, returns bytes read from SDO.
       * @param[out] p          = Pointer to parameter buffer
       * @param[in]  timeout    = Timeout in us, standard is EC_TIMEOUTRXM
       * @return Workcounter from last slave response
       *}
      ec_SDOread: function(slave, index: Word;subindex :Byte; CA: boolean; psize :PInteger; p : Pointer; timeout: integer):Integer; cdecl;
      {** CoE SDO write, blocking. Single subindex or Complete Access.
       *
       * A "normal" download request is issued, unless we have
       * small data, then a "expedited" transfer is used. If the parameter is larger than
       * the mailbox size then the download is segmented. The function will split the
       * parameter data in segments and send them to the slave one by one.
       *
       * @param[in]  Slave      = Slave number
       * @param[in]  Index      = Index to write
       * @param[in]  SubIndex   = Subindex to write, must be 0 or 1 if CA is used.
       * @param[in]  CA         = FALSE = single subindex. TRUE = Complete Access, all subindexes written.
       * @param[in]  psize      = Size in bytes of parameter buffer.
       * @param[out] p          = Pointer to parameter buffer
       * @param[in]  Timeout    = Timeout in us, standard is EC_TIMEOUTRXM
       * @return Workcounter from last slave response
       *}
      ec_SDOwrite: function(slave, Index: Word;subindex :Byte; CA: boolean; psize :Integer; p : Pointer; timeout: integer):Integer; cdecl;
      {** CoE RxPDO write, blocking.
       *
       * A RxPDO download request is issued.
       *
       * @param[in]  Slave         = Slave number
       * @param[in]  RxPDOnumber   = Related RxPDO number
       * @param[in]  psize         = Size in bytes of PDO buffer.
       * @param[out] p             = Pointer to PDO buffer
       * @return Workcounter from last slave response
       *}
      ec_RxPDO: function(Slave, RxPDOnumber :Word; psize : Integer; p :Pointer):Integer; cdecl;
      {** CoE TxPDO read remote request, blocking.
       *
       * A RxPDO download request is issued.
       *
       * @param[in]  slave         = Slave number
       * @param[in]  TxPDOnumber   = Related TxPDO number
       * @param[in,out] psize      = Size in bytes of PDO buffer, returns bytes read from PDO.
       * @param[out] p             = Pointer to PDO buffer
       * @param[in]  timeout       = Timeout in us, standard is EC_TIMEOUTRXM
       * @return Workcounter from last slave response
       *}
      ec_TxPDO: function(slave, TxPDOnumber :Word; psize :PInteger; p :Pointer; timeout: Integer):Integer; cdecl;
      {** CoE read PDO mapping.
       *
       * CANopen has standard indexes defined for PDO mapping. This function
       * tries to read them and collect a full input and output mapping size
       * of designated slave.
       *
       *
       * @param[in] Slave    = Slave number
       * @param[out] Osize   = Size in bits of output mapping (rxPDO) found
       * @param[out] Isize   = Size in bits of input mapping (txPDO) found
       * @return >0 if mapping succesful.
       *}
      ec_readPDOmap: function(Slave : Word; Osize :PInteger; Isize :PInteger):Integer; cdecl;
      {** CoE read PDO mapping in Complete Access mode (CA).
       *
       * CANopen has standard indexes defined for PDO mapping. This function
       * tries to read them and collect a full input and output mapping size
       * of designated slave. Slave has to support CA, otherwise use ec_readPDOmap().
       *
       * @param[in] Slave    = Slave number
       * @param[in] Thread_n = Calling thread index
       * @param[out] Osize   = Size in bits of output mapping (rxPDO) found
       * @param[out] Isize   = Size in bits of input mapping (txPDO) found
       * @return >0 if mapping succesful.A
       *}
      ec_readPDOmapCA: function(Slave :Word; Thread_n : Integer; Osize :PInteger; Isize: PInteger):Integer; cdecl;
      {** CoE read Object Description List.
       *
       * @param[in] Slave      = Slave number.
       * @param[out] pODlist  = resulting Object Description list.
       * @return Workcounter of slave response.
       *}
      ec_readODlist: function(Slave :Word; pODlist: Pec_ODlistt):Integer; cdecl;
      {** CoE read Object Description. Adds textual description to object indexes.
       *
       * @param[in] Item           = Item number in ODlist.
       * @param[in,out] pODlist    = referencing Object Description list.
       * @return Workcounter of slave response.
       *}
      ec_readODdescription: function(Item :Word; pODlist: Pec_ODlistt):Integer; cdecl;
      {** CoE read SDO service object entry, single subindex.
       * Used in ec_readOE().
       *
       * @param[in] Item           = Item in ODlist.
       * @param[in] SubI           = Subindex of item in ODlist.
       * @param[in] pODlist        = Object description list for reference.
       * @param[out] pOElist       = resulting object entry structure.
       * @return Workcounter of slave response.
       *}
      ec_readOEsingle: function(Item :Word; SubI :Byte; pODlist :Pec_ODlistt; pOElist :Pec_OElistt):Integer; cdecl;
      {** CoE read SDO service object entry.
       *
       * @param[in] Item           = Item in ODlist.
       * @param[in] pODlist        = Object description list for reference.
       * @param[out] pOElist       = resulting object entry structure.
       * @return Workcounter of slave response.
       *}
      ec_readOE: function(Item :Word; pODlist : Pec_ODlistt; pOElist : Pec_OElistt):Integer; cdecl;

      // Dynamic Loading of the Shared Library
      procedure LoadLib;
      // Dynamic Unloading of the Shared Library
      procedure UnloadLib;
    protected
    public
      {* Main slave data array.
      * Each slave found on the network gets its own record.
      *  ec_slave[0] is reserved for the master. Structure gets filled
      *  in by the configuration function ec_config().
      *}
      slavelist: Pec_slavet;
      // List of all the available Groups
      grouplist: Pec_groupt;
      // Number of Slaves found in the Network
      slavecounter : PInteger;
      // Working Counter
      wkc: Integer;
      // Process Cycle Counter
      psc: Extended;
      // Time the SPS needed to finish a Process Cycle in ms
      spstime: Extended;
      // In Operation State?
      inOP: boolean;
      // Interface name to use to start EtherCAT on
      ifname: string;
      // Internal Mapping from IO's to Arrays of Boolean
      IOMapping: RIOMapping;
      // Pointer for External Mapping of IO's
      PIOmapping: PIOmap;
      // SPS Log, shows errors, information...
      SPSLog: TStringList;
      Arr_PO2SOConfig: ArrPO2SOconfig;

      // ***************************** WPCAP ***********************************
      ec_find_adapters: function(): Pec_adapter;

      {* Creates the EtherCAT Master
       * @param CycleTime: The time between messages in ns, default is 1ms
      *}
      Constructor Create(CycleTimer: integer = 1000000);
      Destructor Destroy; override;

      // Configure our Master and Slaves
      procedure init; virtual;

      // Stops the Master and all Communication with the Slaves
      procedure Suspend;

      // Process the Messages from EtherCAT Slaves
      // Should be used in a loop...
      function process_msg: integer; virtual;
  end;

  SPS = class(TThread)
  private
    SPSCycle : Integer;
  protected
    procedure Execute; override;
  public
    process_msg: function() :integer;

    Constructor Create(CreateSuspended : boolean);

  end;

implementation

{ SPS }

constructor SPS.Create(CreateSuspended :boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure SPS.Execute;
begin
  SPSCycle := 0;
  while (not Terminated) do begin
    process_msg;
    Inc(SPSCycle);
  end;
end;

{ EtherCAT }

procedure EtherCAT.LoadLib;
begin
  dllHandle := SafeLoadLibrary(DLL);
  if dllHandle <> 0 then begin
     try
       Pointer(ec_find_adapters) := GetProcAddress(dllHandle, 'ec_find_adapters');

       Pointer(ec_init)       := GetProcAddress(dllHandle, 'ec_init');
       Pointer(ec_close)      := GetProcAddress(dllHandle, 'ec_close');
       Pointer(ec_configdc)   := GetProcAddress(dllHandle, 'ec_configdc');
       Pointer(ec_dcsync0)    := GetProcAddress(dllHandle, 'ec_dcsync0');
       Pointer(ec_dcsync01)   := GetProcAddress(dllHandle, 'ec_dcsync01');

       Pointer(ec_readstate)  := GetProcAddress(dllHandle, 'ec_readstate');
       Pointer(ec_writestate) := GetProcAddress(dllHandle, 'ec_writestate');
       Pointer(ec_statecheck) := GetProcAddress(dllHandle, 'ec_statecheck');

       Pointer(ec_iserror)    := GetProcAddress(dllHandle, 'ec_iserror');
       Pointer(ec_packeterror)   := GetProcAddress(dllHandle, 'ec_packeterror');

       Pointer(ec_config_map)       := GetProcAddress(dllHandle, 'ec_config_map');
       Pointer(ec_config_init)      := GetProcAddress(dllHandle, 'ec_config_init');
       Pointer(ec_config_overlap)   := GetProcAddress(dllHandle, 'ec_config_overlap');
       Pointer(ec_config_map_group) := GetProcAddress(dllHandle, 'ec_config_map_group');
       Pointer(ec_config_overlap_map_group) := GetProcAddress(dllHandle, 'ec_config_overlap_map_group');
       Pointer(ec_recover_slave)            := GetProcAddress(dllHandle, 'ec_recover_slave');
       Pointer(ec_reconfig_slave)           := GetProcAddress(dllHandle, 'ec_reconfig_slave');

       Pointer(ec_send_processdata)         := GetProcAddress(dllHandle, 'ec_send_processdata');
       Pointer(ec_send_processdata_group)   := GetProcAddress(dllHandle, 'ec_send_processdata_group');
       Pointer(ec_send_overlap_processdata) := GetProcAddress(dllHandle, 'ec_send_overlap_processdata');
       Pointer(ec_send_overlap_processdata_group) := GetProcAddress(dllHandle, 'ec_send_overlap_processdata_group');

       Pointer(ec_receive_processdata)       := GetProcAddress(dllHandle, 'ec_receive_processdata');
       Pointer(ec_receive_processdata_group) := GetProcAddress(dllHandle, 'ec_receive_processdata_group');

       Pointer(ec_SDOerror)          := GetProcAddress(dllHandle, 'ec_SDOerror');
       Pointer(ec_SDOread)           := GetProcAddress(dllHandle, 'ec_SDOread');
       Pointer(ec_SDOwrite)          := GetProcAddress(dllHandle, 'ec_SDOwrite');
       Pointer(ec_RxPDO)             := GetProcAddress(dllHandle, 'ec_RxPDO');
       Pointer(ec_TxPDO)             := GetProcAddress(dllHandle, 'ec_TxPDO');
       Pointer(ec_readPDOmap)        := GetProcAddress(dllHandle, 'ec_readPDOmap');
       Pointer(ec_readPDOmapCA)      := GetProcAddress(dllHandle, 'ec_readPDOmapCA');
       Pointer(ec_readODlist)        := GetProcAddress(dllHandle, 'ec_readODlist');
       Pointer(ec_readODdescription) := GetProcAddress(dllHandle, 'ec_readODdescription');
       Pointer(ec_readOEsingle)      := GetProcAddress(dllHandle, 'ec_readOEsingle');
       Pointer(ec_readOE)            := GetProcAddress(dllHandle, 'ec_readOE');

       slavelist    := GetProcAddress(dllHandle, 'ec_slave');
       grouplist    := GetProcAddress(dllHandle, 'ec_group');
       slavecounter := GetProcAddress(dllHandle, 'ec_slavecount');
     finally

     end;
  end;
end;

procedure EtherCAT.UnloadLib;
begin
  ec_init := nil;
  ec_close := nil;
  ec_configdc := nil;
  ec_config_init := nil;
  ec_config_map := nil;

  ec_readstate := nil;
  ec_writestate := nil;
  ec_statecheck := nil;

  ec_config_init := nil;
  ec_config_overlap := nil;
  ec_config_map_group := nil;
  ec_config_overlap_map_group := nil;
  ec_recover_slave := nil;
  ec_reconfig_slave := nil;

  ec_send_processdata := nil;
  ec_send_processdata_group := nil;
  ec_send_overlap_processdata := nil;
  ec_send_overlap_processdata_group := nil;
  ec_receive_processdata := nil;
  ec_receive_processdata_group := nil;

  ec_SDOerror := nil;
  ec_SDOread := nil;
  ec_SDOwrite := nil;
  ec_RxPDO := nil;
  ec_TxPDO := nil;
  ec_readPDOmap := nil;
  ec_readPDOmapCA := nil;
  ec_readODlist := nil;
  ec_readODdescription := nil;
  ec_readOEsingle := nil;
  ec_readOE := nil;

  FreeLibrary(dllHandle);
end;

procedure EtherCAT.init;
var
  p: PChar;
  i: integer;
  slave: Pec_slave;
  expectedWC: integer;
begin
  wkc  := 0;
  psc  := 0;
  spstime:= 0;

  p := PChar(ifname);
  try
      if ec_init(p) = 1 then begin
          SPSLog.Append('Started interface');
          if ec_config_init(Byte(false)) = 1 then begin
              SPSLog.Append('Found: ' + IntToStr(slavecounter^) + ' slave/s.');

              for i := 0 to EC_MAXSLAVE - 1 do begin
                 slave := @slavelist^[i];

                 // Device: AXL E EC DI0 DO8 M12 6P
                 if (slave^.eep_man = $84) AND (slave^.eep_id = $2938D0) then begin
                    SPSLog.Append('Found device: ' + slave^.name + '  at Pos: ' + IntToStr(i));
                    slave^.PO2SOconfig := Arr_PO2SOConfig[i];
                 end;
              end;

              // Mapping of IO and Configuration of Slaves
              ec_config_overlap(Byte(true), PIOmapping);

              // Configure the Distributed Clock
              ec_configdc;
              ec_dcsync0(0, true, CycleTime, 0);

              // wait for all slaves to reach SAFE_OP state
              ec_statecheck(0, Ord(EC_STATE_SAFE_OP), EC_TIMEOUTSTATE * 4);

              // Output Loop
              oloop := slavelist^[1].Obytes;
              if (oloop = 0) AND (slavelist^[1].Obits > 0) then oloop := 1;
              if oloop > 8 then oloop := 8;

              // Input Loop
              iloop := slavelist^[1].Ibytes;
              if (iloop = 0) AND (slavelist^[1].Ibits > 0) then iloop := 1;
              if iloop > 8 then iloop := 8;

              SPSLog.Append('Request operational state for all slaves');
              expectedWC := (grouplist^[0].outputsWKC * 2) + grouplist^[0].inputsWKC;
              SPSLog.Append('Calculated workcounter: ' + IntToStr(expectedWC));

              slavelist^[0].State := Ord(EC_STATE_OPERATIONAL);

              // Send one valid process data to make outputs in slaves happy
              ec_send_processdata;
              wkc := ec_receive_processdata(EC_TIMEOUTRET);

              // Request Operational State for all Slaves
              ec_writestate(0);

              ec_statecheck(0, Ord(EC_STATE_OPERATIONAL), EC_TIMEOUTSTATE);

              if slavelist^[0].state = Ord(EC_STATE_OPERATIONAL) then begin
                 SPSLog.Append('Operational State reached for all slaves');
                 inOp := true;
              end else begin
                 SPSLog.Append('Not all slaves reached operational state');
                 ec_readstate();

                 for i:= 0 to slavecounter^  do begin
                     slave := @slavelist^[i];
                     if slave^.state <> Ord(EC_STATE_OPERATIONAL) then begin
                         SPSLog.Append('Slave ' + IntToStr(i) + ' State = ' + IntToBin(slave^.state, 16) + ' StatusCode = ' + IntToBin(slave^.ALstatuscode, 16));
                     end;
                 end;
              end;
          end else begin
            SPSLog.Append('No Slaves found');
            SPSLog.Append('Closing interface...');
          end;
      end else begin
        SPSLog.Append('Interface could not be started...Devicename might be wrong');
      end;
  except
    on E: Exception do begin
        SPSLog.Append('Error' + E.Message);
        Suspend;
    end;
  end;
end;

procedure EtherCAT.Suspend;
begin
  if inOp then begin
     SPSLog.Append('Request Init State for all Slaves');
     slavelist^[0].state := ORD(EC_STATE_INIT);
     ec_writestate(0);
     inOP := false;

     ec_close;
     SPSLog.Append('Closed interface');
  end;
end;

function EtherCAT.process_msg :integer;
var
  i: integer;
  output, input: Byte;
begin
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

   ec_send_overlap_processdata();
   wkc :=ec_receive_processdata(EC_TIMEOUTRET3);
   psc += 1;

   result := 0;
end;

constructor EtherCAT.Create(CycleTimer: integer = 1000000);
begin
  GetMem(PIOmapping, sizeof(IOmap));
  SPSLog := TStringList.Create;
  CycleTime := CycleTimer;

  LoadLib;
end;

destructor EtherCAT.Destroy;
begin
  inherited Destroy;

  FreeMemAndNil(PIOmapping);
  UnloadLib;
end;

end.

