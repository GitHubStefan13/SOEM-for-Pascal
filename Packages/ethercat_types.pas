unit EtherCAT_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EC_Resources;

type
  // Used to Map IO
  IOmap = Array[0..127] of Char;

  // Informations about Interface Adapters
  ec_adapter = packed record
      name: Array[0..EC_MAXLEN_ADAPTERNAME - 1] of Char;
      desc: Array[0..EC_MAXLEN_ADAPTERNAME - 1] of Char;
      next: ^ec_adapter;
  end;

  // Pointer of ec_adapter
  Pec_adapter = ^ec_adapter;

  // Record of IO for easier management
  RIOMapping = record
    Inputs: Array of Boolean;
    Outputs: Array of Boolean;
  end;

  // Pointer of the IOMap
  PIOmap = ^IOmap;

  // Pointer of a Char-Pointer
  PPChar = ^PChar;

  // Enum of possible States
  ec_state = (EC_STATE_NONE = $00,
              EC_STATE_INIT = $01,
              EC_STATE_PRE_OP = $02,
              EC_STATE_BOOT = $03,
              EC_STATE_SAFE_OP = $04,
              EC_STATE_OPERATIONAL = $08,
              EC_STATE_ACK = $10,
              EC_STATE_ERROR = $10);

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

  // Pointer of a slave
  Pec_slave = ^ec_slave;

  // Array holding our Slaves
  ec_slavet = Array[0..EC_MAXSLAVE - 1] of ec_slave;

  // Pointer of a Array[EC_MAXSLAVE] of Slaves
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

  // Pointer of ec_groups
  Pec_group = ^ec_group;

  // Array holding our Groups
  ec_groupt = Array[0..EC_MAXGROUP - 1] of ec_group;

  // Pointer of a Array[EC_MAXGROUP] of groups
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

  // Pointer of ec_OElistt
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

  // Pointer of ec_ODlistt;
  Pec_ODlistt = ^ec_ODlistt;

  // Array to Configure the Slaves
  ArrPO2SOconfig = Array[0..EC_MAXSLAVE - 1] of function(slave: Word): SmallInt;

implementation

end.

