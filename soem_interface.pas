{

***** Given functions by the SOEM.DLL  ***********************

void ec_pusherror(const ec_errort *Ec);
boolean ec_poperror(ec_errort *Ec);
boolean ec_iserror(void);
void ec_packeterror(uint16 Slave, uint16 Index, uint8 SubIdx, uint16 ErrorCode);
int ec_init(const char * ifname);
int ec_readstate(void);
int ec_writestate(uint16 slave);
uint16 ec_statecheck(uint16 slave, uint16 reqstate, int timeout);
void ec_close(void);
int ec_send_processdata_group(uint8 group);
int ec_send_overlap_processdata_group(uint8 group);
int ec_receive_processdata_group(uint8 group, int timeout);
int ec_send_processdata(void);
int ec_send_overlap_processdata(void);
int ec_receive_processdata(int timeout);
ec_slavet get_ec_slave(uint8 slaveIdx);
int get_ec_slavecount;

********************************************************************

}

unit SOEM_Interface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  {$ifdef Win32}
    DLL = 'soem.dll';
  {$else}
      {$ifdef Win64}
        DLL = 'DLL\x64\soem.dll';
      {$else}
        DLL = 'DLL\linux\soem.a';
      {$endif}
  {$endif}


  // max. entries in EtherCAT error list */
  EC_MAXELIST           = 64;
  // max. length of readable name in slavelist and Object Description List */
  EC_MAXNAME            = 40;
  // max. number of slaves in array */
  EC_MAXSLAVE           = 200;
  // max. number of groups */
  EC_MAXGROUP           = 2;
  // max. number of IO segments per group */
  EC_MAXIOSEGMENTS      = 64;
  // max. mailbox size */
  EC_MAXMBX             = 1486;
  // max. eeprom PDO entries */
  EC_MAXEEPDO           = $0200;
  // max. SM used */
  EC_MAXSM              = 8;
  // max. FMMU used */
  EC_MAXFMMU            = 4;
  // max. Adapter */
  EC_MAXLEN_ADAPTERNAME = 128;
  // define maximum number of concurrent threads in mapping */
  EC_MAX_MAPT           = 1;

type
  PIOmap = record
    bit1: byte;
    bit2: byte;
  end;

  ec_slave = record
   // state of slave */
   state :Cardinal;
   // AL status code */
   ALstatuscode :Cardinal;
   // Configured address */
   configadr :Cardinal;
   // Alias address */
   aliasadr :Cardinal;
   // Manufacturer from EEprom */
   eep_man :Cardinal;
   // ID from EEprom */
   eep_id :Cardinal;
   // revision from EEprom */
   eep_rev :Cardinal;
   // Interface type */
   Itype :Cardinal;
   // Device type */
   Dtype :Cardinal;
   // output bits */
   Obits :Cardinal;
   // output bytes, if Obits < 8 then Obytes = 0 */
   Obytes :Cardinal;
   // output pointer in IOmap buffer */
   outputs  :PByte;
   // startbit in first output byte */
   Ostartbit :Byte;
   // input bits */
   Ibits :Cardinal;
   // input bytes, if Ibits < 8 then Ibytes = 0 */
   Ibytes :Cardinal;
   // input pointer in IOmap buffer */
   inputs :PByte;
   // startbit in first input byte */
   Istartbit :Byte;
   // SM structure */
   //ec_smt           SM[EC_MAXSM]; // TODO: Fix
   //SM type =unused 1=MbxWr 2=MbxRd 3=Outputs 4=Inputs */
   SMtype :Array[0..EC_MAXSM] of Byte;
   // FMMU structure */
   //ec_fmmut         FMMU[EC_MAXFMMU]; TODO: Fix
   // FMMU0 function */
   FMMU0func :Byte;
   // FMMU1 function */
   FMMU1func :Byte;
   // FMMU2 function */
   FMMU2func :Byte;
   // FMMU3 function */
   FMMU3func :Byte;
   // length of write mailbox in bytes, if no mailbox then 0 */
   mbx_l :Cardinal;
   // mailbox write offset */
   mbx_wo :Cardinal;
   // length of read mailbox in bytes */
   mbx_rl :Cardinal;
   // mailbox read offset */
   mbx_ro :Cardinal;
   // mailbox supported protocols */
   mbx_proto :Cardinal;
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
   parent :Cardinal;
   // port number on parent this slave is connected to **/
   parentport :Byte;
   // port number on this slave the parent is connected to **/
   entryport :Byte;
   // DC receivetimes on port A */
   DCrtA :Cardinal;
   // DC receivetimes on port B */
   DCrtB :Cardinal;
   // DC receivetimes on port C */
   DCrtC :Cardinal;
   // DC receivetimes on port D */
   DCrtD :Cardinal;
   // propagation delay */
   pdelay :Cardinal;
   // next DC slave */
   DCnext :Cardinal;
   // previous DC slave */
   DCprevious :Cardinal;
   // DC cycle time in ns */
   DCcycle :Cardinal;
   // DC shift from clock modulus boundary */
   DCshift :Cardinal;
   // DC sync activation, 0=off, 1=on */
   DCactive  :Byte;
   // link to config table */
   configindex :Cardinal;
   // link to SII config */
   SIIindex :Cardinal;
   // 1 = 8 bytes per read, 0 = 4 bytes per read */
   eep_8byte :Byte;
   // 0 = eeprom to master , 1 = eeprom to PDI */
   eep_pdi :Cardinal;
   // CoE details */
   CoEdetails :Cardinal;
   // FoE details */
   FoEdetails :Cardinal;
   // EoE details */
   EoEdetails :Cardinal;
   // SoE details */
   SoEdetails :Cardinal;
   // E-bus current */
   Ebuscurrent :Cardinal;
   // if >0 block use of LRW in processdata */
   blockLRW :Byte;
   // group */
   group :Cardinal;
   // first unused FMMU */
   FMMUunused :Byte;
   // Boolean for tracking whether the slave is (not) responding, not used/set by the SOEM library */
   islost :Boolean;
   // registered configuration function PO->SO */
   //int              (*PO2SOconfig)(uint16 slave);   TODO: Fix
   // readable name */
   name: Array[0..EC_MAXNAME + 1] of Char;
  end;

  ec_slavet = Array[0..EC_MAXSLAVE] of ^ec_slave;

  // Common Functions
  function ec_init(const ifname: PChar):Integer; cdecl; external DLL;
  function ec_close((*void: explicit NOTHING*)):Integer; cdecl; external DLL;
  //function ec_config_init(usetable :boolean):Integer; cdecl; external DLL;
  function ec_config_map(piomap: PIOmap):Integer; cdecl; external DLL;

  // Error Handling
  function ec_iserror():Integer; cdecl; external DLL;
  procedure ec_packeterror(Slave, Index :Cardinal; SubIdx: Byte; ErrorCode: Cardinal); cdecl; external DLL;
  //function ec_poperror(ec_eccort ^Ec):Integer; cdecl; external DLL;
  //function ec_pusherror(const ec_errort ^Ec):Integer; cdecl; external DLL;

  // States
  function ec_readstate():Integer; cdecl; external DLL;
  function ec_statecheck(slave, reqstate :Cardinal; timeout: integer):Cardinal; cdecl; external DLL;
  function ec_writestate(slave :Cardinal):Integer; cdecl; external DLL;

  // Receiving Data
  function ec_receive_processdata(timeout: integer):Integer; cdecl; external DLL;
  function ec_receive_processdata_group(group: Byte; timeout: integer):Integer; cdecl; external DLL;

  // Sending Data
  function ec_send_processdata():Integer; cdecl; external DLL;
  function ec_send_processdata_group(group: Byte):Integer; cdecl; external DLL;
  function ec_send_overlap_processdata():Integer; cdecl; external DLL;
  function ec_send_overlap_processdata_group(group: Byte):Integer; cdecl; external DLL;

  // Getters
  function get_ec_slavet: ec_slavet; cdecl; external DLL;
  function get_ec_slavecounter: integer; cdecl; external DLL;

var
  slaves: ec_slavet;
  slavecounter: integer;

implementation

end.

