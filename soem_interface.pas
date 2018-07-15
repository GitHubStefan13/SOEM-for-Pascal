{|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
|        DO NOT CHANGE ANY VALUES WITHOUT KNOWING EXACTLY WHAT YOU DO!         |
|                                                                              |
|   The Records are carefully crafted around the DLL, changing just a single   |
|   value might(will) break the whole concept and display wrong values.        |
|                                                                              |
|                          YOU HAVE BEEN WARNED!                               |
|                                                                              |
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||}

unit SOEM_Interface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs, EC_Resources;

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
  IOmap = Array[0..127] of Char;

  PIOmap = ^IOmap;

  // Enum of possible States
  ec_state = (EC_STATE_INIT = 0, EC_STATE_PRE_OP, EC_STATE_BOOT, EC_STATE_SAFE_OP,
              EC_STATE_OPERATIONAL, EC_STATE_ACK, EC_STATE_ERROR);

  // Record for the Sync Manager
  ec_smt = packed record
    StartAddr: Word;
    SMLength: Word;
    SMflags: Cardinal;
  end;

  // Record for the FMMU
  ec_fmmut = packed record
    LogStart: Cardinal;
    LogLength: Word;
    LogStartbit: Byte;
    LogEndbit: Byte;
    PhysStart: Word;
    PhysStartBit: Byte;
    FMMUtype: Byte;
    FMMUactive: Byte;
    unused1: Byte;
    unused2: word;
  end;

  // Record for the Slave
  ec_slave = packed record
   // state of slave */
   state :Word;
   // AL status code */
   ALstatuscode :Word;
   // Configured address */
   configadr :Word;
   // Alias address */
   aliasadr :Word;
   // Manufacturer from EEprom */
   eep_man :Cardinal;
   // ID from EEprom */
   eep_id :Cardinal;
   // revision from EEprom */
   eep_rev :Cardinal;
   // Interface type */
   Itype :Word;
   // Device type */
   Dtype :Word;
   // output bits */
   Obits :Word;
   // output bytes, if Obits < 8 then Obytes = 0 */
   Obytes :Cardinal;
   // output pointer in IOmap buffer */
   outputs  :PByte;
   // startbit in first output byte */
   Ostartbit :Byte;
   // input bits */
   Ibits :Word;
   // input bytes, if Ibits < 8 then Ibytes = 0 */
   Ibytes :Cardinal;
   // input pointer in IOmap buffer */
   inputs :PByte;
   // startbit in first input byte */
   Istartbit :Byte;
   // SM structure */
   SM : Array[0..EC_MAXSM - 1] of ec_smt;
   //SM type =unused 1=MbxWr 2=MbxRd 3=Outputs 4=Inputs */
   SMtype :Array[0..EC_MAXSM - 1] of Byte;
   // FMMU structure */
   FMMU :Array[0..EC_MAXFMMU - 1] of ec_fmmut;
   // FMMU0 function */
   FMMU0func :Byte;
   // FMMU1 function */
   FMMU1func :Byte;
   // FMMU2 function */
   FMMU2func :Byte;
   // FMMU3 function */
   FMMU3func :Byte;
   // length of write mailbox in bytes, if no mailbox then 0 */
   mbx_l :Word;
   // mailbox write offset */
   mbx_wo :Word;
   // length of read mailbox in bytes */
   mbx_rl :Word;
   // mailbox read offset */
   mbx_ro :Word;
   // mailbox supported protocols */
   mbx_proto :Word;
   // Counter value of mailbox link layer protocol 1..7 */
   mbx_cnt :Byte;
   // has DC capability */
   hasdc :Boolean;
   // Physical type; Ebus, EtherNet combinations */
   ptype :Byte;
   // topology: 1 to 3 links */
   topology :Byte;
   // active ports bitmap : ....3210 , set if respective port is active **/
   activeports :Byte;
   // consumed ports bitmap : ....3210, used for internal delay measurement **/
   consumedports :Byte;
   // slave number for parent, 0=master */
   parent :Word;
   // port number on parent this slave is connected to **/
   parentport :Byte;
   // port number on this slave the parent is connected to **/
   entryport :Byte;
   // DC receivetimes on port A */
   DCrtA :LongInt;
   // DC receivetimes on port B */
   DCrtB :LongInt;
   // DC receivetimes on port C */
   DCrtC :LongInt;
   // DC receivetimes on port D */
   DCrtD :LongInt;
   // propagation delay */
   pdelay :LongInt;
   // next DC slave */
   DCnext :Word;
   // previous DC slave */
   DCprevious :Word;
   // DC cycle time in ns */
   DCcycle :LongInt;
   // DC shift from clock modulus boundary */
   DCshift :LongInt;
   // DC sync activation, 0=off, 1=on */
   DCactive  :Byte;
   // link to config table */
   configindex :Word;
   // link to SII config */
   SIIindex :Word;
   // 1 = 8 bytes per read, 0 = 4 bytes per read */
   eep_8byte :Byte;
   // 0 = eeprom to master , 1 = eeprom to PDI */
   eep_pdi :Byte;
   // CoE details */
   CoEdetails :Byte;
   // FoE details */
   FoEdetails :Byte;
   // EoE details */
   EoEdetails :Byte;
   // SoE details */
   SoEdetails :Byte;
   // E-bus current */
   Ebuscurrent :Word;
   // if >0 block use of LRW in processdata */
   blockLRW :Byte;
   // group */
   group :Byte;
   // first unused FMMU */
   FMMUunused :Byte;
   // Boolean for tracking whether the slave is (not) responding, not used/set by the SOEM library */
   islost :Boolean;
   // registered configuration function PO->SO */
   PO2SOconfig : function(slave: Word): SmallInt;
   // readable name */
   name: Array[0..EC_MAXNAME] of Char;
  end;

  Pec_slave = ^ec_slave;

  // Array holding our Slaves
  ec_slavet = Array[0..EC_MAXSLAVE - 1] of ec_slave;

  Pec_slavet = ^ec_slavet;

  // Record for Groups
  ec_group = record
   // logical start address for this group */
   logstartaddr :Cardinal;
   // output bytes, if Obits < 8 then Obytes = 0 */
   Obytes :Cardinal;
   // output pointer in IOmap buffer */
   outputs :PByte;
   // input bytes, if Ibits < 8 then Ibytes = 0 */
   Ibytes :Cardinal;
   // input pointer in IOmap buffer */
   inputs :PByte;
   // has DC capabillity */
   hasdc :boolean;
   // next DC slave */
   DCnext :Word;
   // E-bus current */
   Ebuscurrent :Word;
   // if >0 block use of LRW in processdata */
   blockLRW :Byte;
   // IO segegments used */
   nsegments :Word;
   // 1st input segment */
   Isegment :Word;
   // Offset in input segment */
   Ioffset :Word;
   // Expected workcounter outputs */
   outputsWKC :Word;
   // Expected workcounter inputs */
   inputsWKC :Word;
   // check slave states */
   docheckstate :boolean;
   // IO segmentation list. Datagrams must not break SM in two. */
   IOsegment : Array[0..EC_MAXIOSEGMENTS - 1] of Cardinal;
  end;

  Pec_group = ^ec_group;

  // Array holding our Groups
  ec_groupt = Array[0..EC_MAXGROUP - 1] of ec_group;

  Pec_groupt = ^ec_groupt;

  // storage for object list entry information */
  ec_OElistt = packed record
      Entries: Word;
      ValueInfo: Array[0..EC_MAXOELIST - 1] of Byte;
      DataType: Array[0..EC_MAXOELIST - 1] of Word;
      BitLength: Array[0..EC_MAXOELIST - 1] of Word;
      ObjAccess: Array[0..EC_MAXOELIST - 1] of Word;
      Name: Array[0..EC_MAXOELIST - 1, 0..EC_MAXNAME] of Char;
  end;

  Pec_OElistt = ^ec_OElistt;

  // Storage for object description list */
  ec_ODlistt = packed record
      Slave: Word;
      Entries: Word;
      Index: Array[0..EC_MAXODLIST - 1] of Word;
      ObjectCode: Array[0..EC_MAXODLIST - 1] of Byte;
      MaxSub: Array[0..EC_MAXODLIST - 1] of Byte;
      Name: Array[0..EC_MAXODLIST - 1, 0..EC_MAXNAME] of Char;
  end;

  Pec_ODlistt = ^ec_ODlistt;

  // Dynamic Loading of the Shared Library
  procedure LoadLib;
  procedure UnloadLib;

var
  dllHandle: TLibHandle;

  slavelist: Pec_slavet;
  grouplist: Pec_groupt;
  slavecounter : PInteger;

  // Common Functions
  ec_init:  function(const ifname: PChar):Integer; cdecl;
  ec_close: procedure ((*void: explicit NOTHING*)); cdecl;

  // Configuration Functions
  ec_configdc: function: boolean; cdecl;
  ec_config_init: function(usetable :Byte):Integer; cdecl;
  ec_config_map:  function(pIOmap: PIOmap):Integer; cdecl;
  ec_config_overlap:  function(usetable :Byte; pIOmap: PIOmap):Integer; cdecl;
  ec_config_map_group:  function(pIOmap: PIOmap; group: Byte):Integer; cdecl;
  ec_config_overlap_map_group:  function(pIOmap: PIOmap; group: Byte):Integer; cdecl;

  // Error Handling
  ec_iserror: function((*void: explicit NOTHING*)):Integer; cdecl;
  ec_packeterror: procedure(Slave, Index :Cardinal; SubIdx: Byte; ErrorCode: Cardinal); cdecl;
  ec_recover_slave:  function(slave: Word; timeout: Integer):Integer; cdecl;
  ec_reconfig_slave:  function(slave: Word; timeout: Integer):Integer; cdecl;

  // States
  ec_readstate: function((*void: explicit NOTHING*)):Integer; cdecl;
  ec_statecheck:  function(slave, reqstate :Cardinal; timeout: integer):Cardinal; cdecl;
  ec_writestate: function(slave :Word):Integer; cdecl;

  // Receiving Data
  ec_receive_processdata: function(timeout: integer):Integer; cdecl;
  ec_receive_processdata_group: function(group: Byte; timeout: integer):Integer; cdecl;

  // Sending Data
  ec_send_processdata: function:Integer; cdecl;
  ec_send_processdata_group: function(group: Byte):Integer; cdecl;
  ec_send_overlap_processdata: function():Integer; cdecl;
  ec_send_overlap_processdata_group: function(group: Byte):Integer; cdecl;

  // CAN over EtherCAT
  ec_SDOerror: procedure(Slave, Index: Word; SubIdx: Byte; AbortCode: Integer);
  ec_SDOread: function(slave, index: Word;subindex :Byte; CA: boolean; psize :PInteger; p : Pointer; timeout: integer):Integer; cdecl;
  ec_SDOwrite: function(slave, Index: Word;subindex :Byte; CA: boolean; psize :Integer; p : Pointer; timeout: integer):Integer; cdecl;
  ec_RxPDO: function(Slave, RxPDOnumber :Word; psize : Integer; p :Pointer):Integer; cdecl;
  ec_TxPDO: function(slave, TxPDOnumber :Word; psize :PInteger; p :Pointer; timeout: Integer):Integer; cdecl;
  ec_readPDOmap: function(Slave : Word; Osize :PInteger; Isize :PInteger):Integer; cdecl;
  ec_readPDOmapCA: function(Slave :Word; Thread_n : Integer; Osize :PInteger; Isize: PInteger):Integer; cdecl;
  ec_readODlist: function(Slave :Word; pODlist: Pec_ODlistt):Integer; cdecl;
  ec_readODdescription: function(Item :Word; pODlist: Pec_ODlistt):Integer; cdecl;
  ec_readOEsingle: function(Item :Word; SubI :Byte; pODlist :Pec_ODlistt; pOElist :Pec_OElistt):Integer; cdecl;
  ec_readOE: function(Item :Word; pODlist : Pec_ODlistt; pOElist : Pec_OElistt):Integer; cdecl;

implementation

procedure LoadLib;
begin
  dllHandle := SafeLoadLibrary(DLL);
  if dllHandle <> 0 then begin
     try
       Pointer(ec_init)       := GetProcAddress(dllHandle, 'ec_init');
       Pointer(ec_close)      := GetProcAddress(dllHandle, 'ec_close');
       Pointer(ec_configdc)   := GetProcAddress(dllHandle, 'ec_configdc');
       Pointer(ec_readstate)  := GetProcAddress(dllHandle, 'ec_readstate');
       Pointer(ec_writestate) := GetProcAddress(dllHandle, 'ec_writestate');
       Pointer(ec_statecheck) := GetProcAddress(dllHandle, 'ec_statecheck');

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

procedure UnloadLib;
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

initialization
  LoadLib;

finalization;
  UnloadLib;

end.

