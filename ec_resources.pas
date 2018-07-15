unit EC_Resources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
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
  // return value general error */
  EC_ERROR           = -3;
  // return value no frame returned */
  EC_NOFRAME         = -1;
  // return value unknown frame received */
  EC_OTHERFRAME      = -2;
  // maximum EtherCAT frame length in bytes */
  EC_MAXECATFRAME    = 1518;
  // maximum EtherCAT LRW frame length in bytes */
  // MTU - Ethernet header - length - datagram header - WCK - FCS */
  EC_MAXLRWDATA      = (EC_MAXECATFRAME - 14 - 2 - 10 - 2 - 4);
  // size of DC datagram used in first LRW frame */
  EC_FIRSTDCDATAGRAM = 20;
  // standard frame buffer size in bytes */
  EC_BUFSIZE         = EC_MAXECATFRAME;
  // datagram type EtherCAT */
  EC_ECATTYPE        = $1000;
  // number of frame buffers per channel (tx, rx1 rx2) */
  EC_MAXBUF          = 16;
  // timeout value in us for tx frame to return to rx */
  EC_TIMEOUTRET      = 2000;
  // timeout value in us for safe data transfer, max. triple retry */
  EC_TIMEOUTRET3     = (EC_TIMEOUTRET * 3);
  // timeout value in us for return "safe" variant (f.e. wireless) */
  EC_TIMEOUTSAFE     = 20000;
  // timeout value in us for EEPROM access */
  EC_TIMEOUTEEP      = 20000;
  // timeout value in us for tx mailbox cycle */
  EC_TIMEOUTTXM      = 20000;
  // timeout value in us for rx mailbox cycle */
  EC_TIMEOUTRXM      = 700000;
  // timeout value in us for check statechange */
  EC_TIMEOUTSTATE    = 2000000;
  // size of EEPROM bitmap cache */
  EC_MAXEEPBITMAP    = 128;
  // size of EEPROM cache buffer */
  EC_MAXEEPBUF       = EC_MAXEEPBITMAP shl 5;
  // default number of retries if wkc <= 0 */
  EC_DEFAULTRETRIES  = 3;
  // max entries in Object Description list */
  EC_MAXODLIST       = 1024;
  // max entries in Object Entry list */
  EC_MAXOELIST       = 256;


implementation

end.

