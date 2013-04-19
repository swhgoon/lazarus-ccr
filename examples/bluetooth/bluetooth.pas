{ Access Linux Bluetooth via BlueZ lib.

  Copyright (C) 2008 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit Bluetooth;

{$mode objfpc}{$H+}

interface

uses
  ctypes;


const
  ETH_ALEN = 6;

type
  sa_family_t = cushort;

{$IfNDef __BLUETOOTH_H}
  {$IfNDef AF_BLUETOOTH}
    
    const
      AF_BLUETOOTH = 31;
      PF_BLUETOOTH = AF_BLUETOOTH;
    
  {$EndIf}
  
  const
    BTPROTO_L2CAP = 0;
    BTPROTO_HCI = 1;
    BTPROTO_SCO = 2;
    BTPROTO_RFCOMM = 3;
    BTPROTO_BNEP = 4;
    BTPROTO_CMTP = 5;
    BTPROTO_HIDP = 6;
    BTPROTO_AVDTP = 7;
    SOL_HCI = 0;
    SOL_L2CAP = 6;
    SOL_SCO = 17;
    SOL_RFCOMM = 18;
  
  type
    enumBT_CONNECTED1BT_OPENBT_BOUNDBT_LISTENBT_CONNECTBT_CONNECT2 = (
      BT_CONNECTED = 1,
      BT_OPEN,
      BT_BOUND,
      BT_LISTEN,
      BT_CONNECT,
      BT_CONNECT2,
      BT_CONFIG,
      BT_DISCONN,
      BT_CLOSED
    );
  
  {$If defined(ENDIAN_LITTLE)}
  {$ElseIf defined(ENDIAN_BIG)}
  {$Else}
    {$Error "Unknown byte order"}
  {$EndIf}
  
  type
    TArray0to61Ofcuint8 = array[0..6-1] of cuint8;
    bdaddr_t = record
      b: TArray0to61Ofcuint8;
    end;
  
  {off $Define BDADDR_ANY:=(&(bdaddr_t) #123#1230, 0, 0, 0, 0, 0#125#125)}
  {off $Define BDADDR_ALL:=(&(bdaddr_t) #123#1230xff, 0xff, 0xff, 0xff, 0xff, 0xff#125#125)}
  {off $Define BDADDR_LOCAL:=(&(bdaddr_t) #123#1230, 0, 0, 0xff, 0xff, 0xff#125#125)}
  
  type
    Pbdaddr_t = ^bdaddr_t;
  
  procedure baswap(dst: Pbdaddr_t; src: Pbdaddr_t); cdecl; external;
  function strtoba(str: pcchar): Pbdaddr_t; cdecl; external;
  function batostr(ba: Pbdaddr_t): pcchar; cdecl; external;
  function ba2str(ba: Pbdaddr_t; str: pcchar): cint; cdecl; external;
  function str2ba(str: pcchar; ba: Pbdaddr_t): cint; cdecl; external;
  function ba2oui(ba: Pbdaddr_t; oui: pcchar): cint; cdecl; external;
  function bachk(str: pcchar): cint; cdecl; external;
  function baprintf(format: pcchar; param1arrayof: array of const): cint; cdecl; external;
  
  type
    PFILE = ^FILE;
  
  function bafprintf(stream: PFILE; format: pcchar; param1arrayof: array of const): cint; cdecl; external;
  function basprintf(str: pcchar; format: pcchar; param1arrayof: array of const): cint; cdecl; external;
  function basnprintf(str: pcchar; size: PtrUInt; format: pcchar; param1arrayof: array of const): cint; cdecl; external;
  function bt_malloc(size: PtrUInt): pointer; cdecl; external;
  procedure bt_free(ptr: pointer); cdecl; external;
  function bt_error(code: cuint16): cint; cdecl; external;
  function bt_compidtostr(id: cint): pcchar; cdecl; external;
{$EndIf}
{$IfNDef __BNEP_H}
  {off $Define BNEP_BASE_UUID:=0x0000000000001000800000805F9B34FB}
  
  const
    BNEP_UUID16 = $02;
    BNEP_UUID32 = $04;
    BNEP_UUID128 = $16;
    BNEP_SVC_PANU = $1115;
    BNEP_SVC_NAP = $1116;
    BNEP_SVC_GN = $1117;
    BNEP_GENERAL = $00;
    BNEP_CONTROL = $01;
    BNEP_COMPRESSED = $02;
    BNEP_COMPRESSED_SRC_ONLY = $03;
    BNEP_COMPRESSED_DST_ONLY = $04;
    BNEP_CMD_NOT_UNDERSTOOD = $00;
    BNEP_SETUP_CONN_REQ_1 = $01;
    BNEP_SETUP_CONN_RSP = $02;
    BNEP_FILTER_NET_TYPE_SET = $03;
    BNEP_FILTER_NET_TYPE_RSP = $04;
    BNEP_FILTER_MULT_ADDR_SET = $05;
    BNEP_FILTER_MULT_ADDR_RSP = $06;
    BNEP_SUCCESS = $00;
    BNEP_CONN_INVALID_DST = $01;
    BNEP_CONN_INVALID_SRC = $02;
    BNEP_CONN_INVALID_SVC = $03;
    BNEP_CONN_NOT_ALLOWED = $04;
    BNEP_FILTER_UNSUPPORTED_REQ = $01;
    BNEP_FILTER_INVALID_RANGE = $02;
    BNEP_FILTER_INVALID_MCADDR = $02;
    BNEP_FILTER_LIMIT_REACHED = $03;
    BNEP_FILTER_DENIED_SECURITY = $04;
    BNEP_MTU = 1691;
    BNEP_FLUSH_TO = $ffff;
    BNEP_CONNECT_TO = 15;
    BNEP_FILTER_TO = 15;
  
  {$IfNDef BNEP_PSM}
    
    const
      BNEP_PSM = $0f;
    
  {$EndIf}
  
  const
    BNEP_TYPE_MASK = $7f;
    BNEP_EXT_HEADER = $80;
  
  type
    TArray0to0Ofcuint8 = array[0..0] of cuint8;
    bnep_setup_conn_req = record
      type_: cuint8;
      ctrl: cuint8;
      uuid_size: cuint8;
      service: TArray0to0Ofcuint8;
    end;
    bnep_set_filter_req = record
      type_: cuint8;
      ctrl: cuint8;
      len: cuint16;
      list: TArray0to0Ofcuint8;
    end;
    bnep_control_rsp = record
      type_: cuint8;
      ctrl: cuint8;
      resp: cuint16;
    end;
    bnep_ext_hdr = record
      type_: cuint8;
      len: cuint8;
      data: TArray0to0Ofcuint8;
    end;
  
  {off $Define BNEPCONNADD:=_IOW('B', 200, int)}
  {off $Define BNEPCONNDEL:=_IOW('B', 201, int)}
  {off $Define BNEPGETCONNLIST:=_IOR('B', 210, int)}
  {off $Define BNEPGETCONNINFO:=_IOR('B', 211, int)}
  
  type
    TArray0to161Ofcchar = array[0..16-1] of cchar;
    bnep_connadd_req = record
      sock: cint;
      flags: cuint32;
      role: cuint16;
      device: TArray0to161Ofcchar;
    end;
    TArray0toETH_ALEN1Ofcuint8 = array[0..ETH_ALEN-1] of cuint8;
    bnep_conndel_req = record
      flags: cuint32;
      dst: TArray0toETH_ALEN1Ofcuint8;
    end;
    bnep_conninfo = record
      flags: cuint32;
      role: cuint16;
      state: cuint16;
      dst: TArray0toETH_ALEN1Ofcuint8;
      device: TArray0to161Ofcchar;
    end;
    Pbnep_conninfo = ^bnep_conninfo;
    bnep_connlist_req = record
      cnum: cuint32;
      ci: Pbnep_conninfo;
    end;
  
{$EndIf}
{$IfNDef __CMTP_H}
  
  const
    CMTP_MINIMUM_MTU = 152;
    CMTP_DEFAULT_MTU = 672;
  
  {off $Define CMTPCONNADD:=_IOW('C', 200, int)}
  {off $Define CMTPCONNDEL:=_IOW('C', 201, int)}
  {off $Define CMTPGETCONNLIST:=_IOR('C', 210, int)}
  {off $Define CMTPGETCONNINFO:=_IOR('C', 211, int)}
  
  const
    CMTP_LOOPBACK = 0;
  
  type
    cmtp_connadd_req = record
      sock: cint;
      flags: cuint32;
    end;
    cmtp_conndel_req = record
      bdaddr: bdaddr_t;
      flags: cuint32;
    end;
    cmtp_conninfo = record
      bdaddr: bdaddr_t;
      flags: cuint32;
      state: cuint16;
      num: cint;
    end;
    Pcmtp_conninfo = ^cmtp_conninfo;
    cmtp_connlist_req = record
      cnum: cuint32;
      ci: Pcmtp_conninfo;
    end;
  
{$EndIf}
{$IfNDef __HCI_H}
  
  const
    HCI_MAX_DEV = 16;
    HCI_MAX_ACL_SIZE = 1024;
    HCI_MAX_SCO_SIZE = 255;
    HCI_MAX_EVENT_SIZE = 260;
    HCI_MAX_FRAME_SIZE = (HCI_MAX_ACL_SIZE + 4);
    HCI_DEV_REG = 1;
    HCI_DEV_UNREG = 2;
    HCI_DEV_UP = 3;
    HCI_DEV_DOWN = 4;
    HCI_DEV_SUSPEND = 5;
    HCI_DEV_RESUME = 6;
    HCI_VIRTUAL = 0;
    HCI_USB = 1;
    HCI_PCCARD = 2;
    HCI_UART = 3;
    HCI_RS232 = 4;
    HCI_PCI = 5;
    HCI_SDIO = 6;
  
  type
    enumHCI_UPHCI_INITHCI_RUNNINGHCI_PSCANHCI_ISCANHCI_AUTHHCI_ENCRYPT = (
      HCI_UP,
      HCI_INIT,
      HCI_RUNNING,
      HCI_PSCAN,
      HCI_ISCAN,
      HCI_AUTH,
      HCI_ENCRYPT,
      HCI_INQUIRY,
      HCI_RAW,
      HCI_SECMGR
    );
  
  {off $Define HCIDEVUP:=_IOW('H', 201, int)}
  {off $Define HCIDEVDOWN:=_IOW('H', 202, int)}
  {off $Define HCIDEVRESET:=_IOW('H', 203, int)}
  {off $Define HCIDEVRESTAT:=_IOW('H', 204, int)}
  {off $Define HCIGETDEVLIST:=_IOR('H', 210, int)}
  {off $Define HCIGETDEVINFO:=_IOR('H', 211, int)}
  {off $Define HCIGETCONNLIST:=_IOR('H', 212, int)}
  {off $Define HCIGETCONNINFO:=_IOR('H', 213, int)}
  {off $Define HCISETRAW:=_IOW('H', 220, int)}
  {off $Define HCISETSCAN:=_IOW('H', 221, int)}
  {off $Define HCISETAUTH:=_IOW('H', 222, int)}
  {off $Define HCISETENCRYPT:=_IOW('H', 223, int)}
  {off $Define HCISETPTYPE:=_IOW('H', 224, int)}
  {off $Define HCISETLINKPOL:=_IOW('H', 225, int)}
  {off $Define HCISETLINKMODE:=_IOW('H', 226, int)}
  {off $Define HCISETACLMTU:=_IOW('H', 227, int)}
  {off $Define HCISETSCOMTU:=_IOW('H', 228, int)}
  {off $Define HCISETSECMGR:=_IOW('H', 230, int)}
  {off $Define HCIINQUIRY:=_IOR('H', 240, int)}
  {$IfNDef __NO_HCI_DEFS}
    
    const
      HCI_COMMAND_PKT = $01;
      HCI_ACLDATA_PKT = $02;
      HCI_SCODATA_PKT = $03;
      HCI_EVENT_PKT = $04;
      HCI_VENDOR_PKT = $ff;
      HCI_2DH1 = $0002;
      HCI_3DH1 = $0004;
      HCI_DM1 = $0008;
      HCI_DH1 = $0010;
      HCI_2DH3 = $0100;
      HCI_3DH3 = $0200;
      HCI_DM3 = $0400;
      HCI_DH3 = $0800;
      HCI_2DH5 = $1000;
      HCI_3DH5 = $2000;
      HCI_DM5 = $4000;
      HCI_DH5 = $8000;
      HCI_HV1 = $0020;
      HCI_HV2 = $0040;
      HCI_HV3 = $0080;
      HCI_EV3 = $0008;
      HCI_EV4 = $0010;
      HCI_EV5 = $0020;
      HCI_2EV3 = $0040;
      HCI_3EV3 = $0080;
      HCI_2EV5 = $0100;
      HCI_3EV5 = $0200;
      SCO_PTYPE_MASK = (HCI_HV1 or HCI_HV2 or HCI_HV3);
      ACL_PTYPE_MASK = (HCI_DM1 or HCI_DH1 or HCI_DM3 or HCI_DH3 or HCI_DM5 or HCI_DH5);
      HCI_UNKNOWN_COMMAND = $01;
      HCI_NO_CONNECTION = $02;
      HCI_HARDWARE_FAILURE = $03;
      HCI_PAGE_TIMEOUT = $04;
      HCI_AUTHENTICATION_FAILURE = $05;
      HCI_PIN_OR_KEY_MISSING = $06;
      HCI_MEMORY_FULL = $07;
      HCI_CONNECTION_TIMEOUT = $08;
      HCI_MAX_NUMBER_OF_CONNECTIONS = $09;
      HCI_MAX_NUMBER_OF_SCO_CONNECTIONS = $0a;
      HCI_ACL_CONNECTION_EXISTS = $0b;
      HCI_COMMAND_DISALLOWED = $0c;
      HCI_REJECTED_LIMITED_RESOURCES = $0d;
      HCI_REJECTED_SECURITY = $0e;
      HCI_REJECTED_PERSONAL = $0f;
      HCI_HOST_TIMEOUT = $10;
      HCI_UNSUPPORTED_FEATURE = $11;
      HCI_INVALID_PARAMETERS = $12;
      HCI_OE_USER_ENDED_CONNECTION = $13;
      HCI_OE_LOW_RESOURCES = $14;
      HCI_OE_POWER_OFF = $15;
      HCI_CONNECTION_TERMINATED = $16;
      HCI_REPEATED_ATTEMPTS = $17;
      HCI_PAIRING_NOT_ALLOWED = $18;
      HCI_UNKNOWN_LMP_PDU = $19;
      HCI_UNSUPPORTED_REMOTE_FEATURE = $1a;
      HCI_SCO_OFFSET_REJECTED = $1b;
      HCI_SCO_INTERVAL_REJECTED = $1c;
      HCI_AIR_MODE_REJECTED = $1d;
      HCI_INVALID_LMP_PARAMETERS = $1e;
      HCI_UNSPECIFIED_ERROR = $1f;
      HCI_UNSUPPORTED_LMP_PARAMETER_VALUE = $20;
      HCI_ROLE_CHANGE_NOT_ALLOWED = $21;
      HCI_LMP_RESPONSE_TIMEOUT = $22;
      HCI_LMP_ERROR_TRANSACTION_COLLISION = $23;
      HCI_LMP_PDU_NOT_ALLOWED = $24;
      HCI_ENCRYPTION_MODE_NOT_ACCEPTED = $25;
      HCI_UNIT_LINK_KEY_USED = $26;
      HCI_QOS_NOT_SUPPORTED = $27;
      HCI_INSTANT_PASSED = $28;
      HCI_PAIRING_NOT_SUPPORTED = $29;
      HCI_TRANSACTION_COLLISION = $2a;
      HCI_QOS_UNACCEPTABLE_PARAMETER = $2c;
      HCI_QOS_REJECTED = $2d;
      HCI_CLASSIFICATION_NOT_SUPPORTED = $2e;
      HCI_INSUFFICIENT_SECURITY = $2f;
      HCI_PARAMETER_OUT_OF_RANGE = $30;
      HCI_ROLE_SWITCH_PENDING = $32;
      HCI_SLOT_VIOLATION = $34;
      HCI_ROLE_SWITCH_FAILED = $35;
      HCI_EIR_TOO_LARGE = $36;
      HCI_SIMPLE_PAIRING_NOT_SUPPORTED = $37;
      HCI_HOST_BUSY_PAIRING = $38;
      ACL_CONT = $01;
      ACL_START = $02;
      ACL_ACTIVE_BCAST = $04;
      ACL_PICO_BCAST = $08;
      SCO_LINK = $00;
      ACL_LINK = $01;
      ESCO_LINK = $02;
      LMP_3SLOT = $01;
      LMP_5SLOT = $02;
      LMP_ENCRYPT = $04;
      LMP_SOFFSET = $08;
      LMP_TACCURACY = $10;
      LMP_RSWITCH = $20;
      LMP_HOLD = $40;
      LMP_SNIFF = $80;
      LMP_PARK = $01;
      LMP_RSSI = $02;
      LMP_QUALITY = $04;
      LMP_SCO = $08;
      LMP_HV2 = $10;
      LMP_HV3 = $20;
      LMP_ULAW = $40;
      LMP_ALAW = $80;
      LMP_CVSD = $01;
      LMP_PSCHEME = $02;
      LMP_PCONTROL = $04;
      LMP_TRSP_SCO = $08;
      LMP_BCAST_ENC = $80;
      LMP_EDR_ACL_2M = $02;
      LMP_EDR_ACL_3M = $04;
      LMP_ENH_ISCAN = $08;
      LMP_ILACE_ISCAN = $10;
      LMP_ILACE_PSCAN = $20;
      LMP_RSSI_INQ = $40;
      LMP_ESCO = $80;
      LMP_EV4 = $01;
      LMP_EV5 = $02;
      LMP_AFH_CAP_SLV = $08;
      LMP_AFH_CLS_SLV = $10;
      LMP_EDR_3SLOT = $80;
      LMP_EDR_5SLOT = $01;
      LMP_SNIFF_SUBR = $02;
      LMP_PAUSE_ENC = $04;
      LMP_AFH_CAP_MST = $08;
      LMP_AFH_CLS_MST = $10;
      LMP_EDR_ESCO_2M = $20;
      LMP_EDR_ESCO_3M = $40;
      LMP_EDR_3S_ESCO = $80;
      LMP_EXT_INQ = $01;
      LMP_SIMPLE_PAIR = $08;
      LMP_ENCAPS_PDU = $10;
      LMP_ERR_DAT_REP = $20;
      LMP_NFLUSH_PKTS = $40;
      LMP_LSTO = $01;
      LMP_INQ_TX_PWR = $02;
      LMP_EXT_FEAT = $80;
      HCI_LP_RSWITCH = $0001;
      HCI_LP_HOLD = $0002;
      HCI_LP_SNIFF = $0004;
      HCI_LP_PARK = $0008;
      HCI_LM_ACCEPT = $8000;
      HCI_LM_MASTER = $0001;
      HCI_LM_AUTH = $0002;
      HCI_LM_ENCRYPT = $0004;
      HCI_LM_TRUSTED = $0008;
      HCI_LM_RELIABLE = $0010;
      HCI_LM_SECURE = $0020;
      OGF_LINK_CTL = $01;
      OCF_INQUIRY = $0001;
    
    type
      TArray0to31Ofcuint8 = array[0..3-1] of cuint8;
      inquiry_cp = record
        lap: TArray0to31Ofcuint8;
        length: cuint8;
        num_rsp: cuint8;
      end;
    
    const
      INQUIRY_CP_SIZE = 5;
    
    type
      status_bdaddr_rp = record
        status: cuint8;
        bdaddr: bdaddr_t;
      end;
    
    const
      STATUS_BDADDR_RP_SIZE = 7;
      OCF_INQUIRY_CANCEL = $0002;
      OCF_PERIODIC_INQUIRY = $0003;
    
    type
      periodic_inquiry_cp = record
        max_period: cuint16;
        min_period: cuint16;
        lap: TArray0to31Ofcuint8;
        length: cuint8;
        num_rsp: cuint8;
      end;
    
    const
      PERIODIC_INQUIRY_CP_SIZE = 9;
      OCF_EXIT_PERIODIC_INQUIRY = $0004;
      OCF_CREATE_CONN = $0005;
    
    type
      create_conn_cp = record
        bdaddr: bdaddr_t;
        pkt_type: cuint16;
        pscan_rep_mode: cuint8;
        pscan_mode: cuint8;
        clock_offset: cuint16;
        role_switch: cuint8;
      end;
    
    const
      CREATE_CONN_CP_SIZE = 13;
      OCF_DISCONNECT = $0006;
    
    type
      disconnect_cp = record
        handle: cuint16;
        reason: cuint8;
      end;
    
    const
      DISCONNECT_CP_SIZE = 3;
      OCF_ADD_SCO = $0007;
    
    type
      add_sco_cp = record
        handle: cuint16;
        pkt_type: cuint16;
      end;
    
    const
      ADD_SCO_CP_SIZE = 4;
      OCF_CREATE_CONN_CANCEL = $0008;
    
    type
      create_conn_cancel_cp = record
        bdaddr: bdaddr_t;
      end;
    
    const
      CREATE_CONN_CANCEL_CP_SIZE = 6;
      OCF_ACCEPT_CONN_REQ = $0009;
    
    type
      accept_conn_req_cp = record
        bdaddr: bdaddr_t;
        role: cuint8;
      end;
    
    const
      ACCEPT_CONN_REQ_CP_SIZE = 7;
      OCF_REJECT_CONN_REQ = $000A;
    
    type
      reject_conn_req_cp = record
        bdaddr: bdaddr_t;
        reason: cuint8;
      end;
    
    const
      REJECT_CONN_REQ_CP_SIZE = 7;
      OCF_LINK_KEY_REPLY = $000B;
    
    type
      TArray0to161Ofcuint8 = array[0..16-1] of cuint8;
      link_key_reply_cp = record
        bdaddr: bdaddr_t;
        link_key: TArray0to161Ofcuint8;
      end;
    
    const
      LINK_KEY_REPLY_CP_SIZE = 22;
      OCF_LINK_KEY_NEG_REPLY = $000C;
      OCF_PIN_CODE_REPLY = $000D;
    
    type
      pin_code_reply_cp = record
        bdaddr: bdaddr_t;
        pin_len: cuint8;
        pin_code: TArray0to161Ofcuint8;
      end;
    
    const
      PIN_CODE_REPLY_CP_SIZE = 23;
      OCF_PIN_CODE_NEG_REPLY = $000E;
      OCF_SET_CONN_PTYPE = $000F;
    
    type
      set_conn_ptype_cp = record
        handle: cuint16;
        pkt_type: cuint16;
      end;
    
    const
      SET_CONN_PTYPE_CP_SIZE = 4;
      OCF_AUTH_REQUESTED = $0011;
    
    type
      auth_requested_cp = record
        handle: cuint16;
      end;
    
    const
      AUTH_REQUESTED_CP_SIZE = 2;
      OCF_SET_CONN_ENCRYPT = $0013;
    
    type
      set_conn_encrypt_cp = record
        handle: cuint16;
        encrypt: cuint8;
      end;
    
    const
      SET_CONN_ENCRYPT_CP_SIZE = 3;
      OCF_CHANGE_CONN_LINK_KEY = $0015;
    
    type
      change_conn_link_key_cp = record
        handle: cuint16;
      end;
    
    const
      CHANGE_CONN_LINK_KEY_CP_SIZE = 2;
      OCF_MASTER_LINK_KEY = $0017;
    
    type
      master_link_key_cp = record
        key_flag: cuint8;
      end;
    
    const
      MASTER_LINK_KEY_CP_SIZE = 1;
      OCF_REMOTE_NAME_REQ = $0019;
    
    type
      remote_name_req_cp = record
        bdaddr: bdaddr_t;
        pscan_rep_mode: cuint8;
        pscan_mode: cuint8;
        clock_offset: cuint16;
      end;
    
    const
      REMOTE_NAME_REQ_CP_SIZE = 10;
      OCF_REMOTE_NAME_REQ_CANCEL = $001A;
    
    type
      remote_name_req_cancel_cp = record
        bdaddr: bdaddr_t;
      end;
    
    const
      REMOTE_NAME_REQ_CANCEL_CP_SIZE = 6;
      OCF_READ_REMOTE_FEATURES = $001B;
    
    type
      read_remote_features_cp = record
        handle: cuint16;
      end;
    
    const
      READ_REMOTE_FEATURES_CP_SIZE = 2;
      OCF_READ_REMOTE_EXT_FEATURES = $001C;
    
    type
      read_remote_ext_features_cp = record
        handle: cuint16;
        page_num: cuint8;
      end;
    
    const
      READ_REMOTE_EXT_FEATURES_CP_SIZE = 3;
      OCF_READ_REMOTE_VERSION = $001D;
    
    type
      read_remote_version_cp = record
        handle: cuint16;
      end;
    
    const
      READ_REMOTE_VERSION_CP_SIZE = 2;
      OCF_READ_CLOCK_OFFSET = $001F;
    
    type
      read_clock_offset_cp = record
        handle: cuint16;
      end;
    
    const
      READ_CLOCK_OFFSET_CP_SIZE = 2;
      OCF_READ_LMP_HANDLE = $0020;
      OCF_SETUP_SYNC_CONN = $0028;
    
    type
      setup_sync_conn_cp = record
        handle: cuint16;
        tx_bandwith: cuint32;
        rx_bandwith: cuint32;
        max_latency: cuint16;
        voice_setting: cuint16;
        retrans_effort: cuint8;
        pkt_type: cuint16;
      end;
    
    const
      SETUP_SYNC_CONN_CP_SIZE = 17;
      OCF_ACCEPT_SYNC_CONN_REQ = $0029;
    
    type
      accept_sync_conn_req_cp = record
        bdaddr: bdaddr_t;
        tx_bandwith: cuint32;
        rx_bandwith: cuint32;
        max_latency: cuint16;
        voice_setting: cuint16;
        retrans_effort: cuint8;
        pkt_type: cuint16;
      end;
    
    const
      ACCEPT_SYNC_CONN_REQ_CP_SIZE = 21;
      OCF_REJECT_SYNC_CONN_REQ = $002A;
    
    type
      reject_sync_conn_req_cp = record
        bdaddr: bdaddr_t;
        reason: cuint8;
      end;
    
    const
      REJECT_SYNC_CONN_REQ_CP_SIZE = 7;
      OCF_IO_CAPABILITY_REPLY = $002B;
    
    type
      io_capability_reply_cp = record
        bdaddr: bdaddr_t;
        capability: cuint8;
        oob_data: cuint8;
        authentication: cuint8;
      end;
    
    const
      IO_CAPABILITY_REPLY_CP_SIZE = 9;
      OCF_USER_CONFIRM_REPLY = $002C;
    
    type
      user_confirm_reply_cp = record
        bdaddr: bdaddr_t;
      end;
    
    const
      USER_CONFIRM_REPLY_CP_SIZE = 6;
      OCF_USER_CONFIRM_NEG_REPLY = $002D;
      OCF_USER_PASSKEY_REPLY = $002E;
    
    type
      user_passkey_reply_cp = record
        bdaddr: bdaddr_t;
        passkey: cuint32;
      end;
    
    const
      USER_PASSKEY_REPLY_CP_SIZE = 10;
      OCF_USER_PASSKEY_NEG_REPLY = $002F;
      OCF_REMOTE_OOB_DATA_REPLY = $0030;
    
    type
      remote_oob_data_reply_cp = record
        bdaddr: bdaddr_t;
        hash: TArray0to161Ofcuint8;
        randomizer: TArray0to161Ofcuint8;
      end;
    
    const
      REMOTE_OOB_DATA_REPLY_CP_SIZE = 38;
      OCF_REMOTE_OOB_DATA_NEG_REPLY = $0033;
      OCF_IO_CAPABILITY_NEG_REPLY = $0034;
    
    type
      io_capability_neg_reply_cp = record
        bdaddr: bdaddr_t;
        reason: cuint8;
      end;
    
    const
      IO_CAPABILITY_NEG_REPLY_CP_SIZE = 7;
      OGF_LINK_POLICY = $02;
      OCF_HOLD_MODE = $0001;
    
    type
      hold_mode_cp = record
        handle: cuint16;
        max_interval: cuint16;
        min_interval: cuint16;
      end;
    
    const
      HOLD_MODE_CP_SIZE = 6;
      OCF_SNIFF_MODE = $0003;
    
    type
      sniff_mode_cp = record
        handle: cuint16;
        max_interval: cuint16;
        min_interval: cuint16;
        attempt: cuint16;
        timeout: cuint16;
      end;
    
    const
      SNIFF_MODE_CP_SIZE = 10;
      OCF_EXIT_SNIFF_MODE = $0004;
    
    type
      exit_sniff_mode_cp = record
        handle: cuint16;
      end;
    
    const
      EXIT_SNIFF_MODE_CP_SIZE = 2;
      OCF_PARK_MODE = $0005;
    
    type
      park_mode_cp = record
        handle: cuint16;
        max_interval: cuint16;
        min_interval: cuint16;
      end;
    
    const
      PARK_MODE_CP_SIZE = 6;
      OCF_EXIT_PARK_MODE = $0006;
    
    type
      exit_park_mode_cp = record
        handle: cuint16;
      end;
    
    const
      EXIT_PARK_MODE_CP_SIZE = 2;
      OCF_QOS_SETUP = $0007;
    
    type
      hci_qos = record
        service_type: cuint8;
        token_rate: cuint32;
        peak_bandwidth: cuint32;
        latency: cuint32;
        delay_variation: cuint32;
      end;
    
    const
      HCI_QOS_CP_SIZE = 17;
    
    type
      qos_setup_cp = record
        handle: cuint16;
        flags: cuint8;
        qos: hci_qos;
      end;
    
    const
      QOS_SETUP_CP_SIZE = (3 + HCI_QOS_CP_SIZE);
      OCF_ROLE_DISCOVERY = $0009;
    
    type
      role_discovery_cp = record
        handle: cuint16;
      end;
    
    const
      ROLE_DISCOVERY_CP_SIZE = 2;
    
    type
      role_discovery_rp = record
        status: cuint8;
        handle: cuint16;
        role: cuint8;
      end;
    
    const
      ROLE_DISCOVERY_RP_SIZE = 4;
      OCF_SWITCH_ROLE = $000B;
    
    type
      switch_role_cp = record
        bdaddr: bdaddr_t;
        role: cuint8;
      end;
    
    const
      SWITCH_ROLE_CP_SIZE = 7;
      OCF_READ_LINK_POLICY = $000C;
    
    type
      read_link_policy_cp = record
        handle: cuint16;
      end;
    
    const
      READ_LINK_POLICY_CP_SIZE = 2;
    
    type
      read_link_policy_rp = record
        status: cuint8;
        handle: cuint16;
        policy: cuint16;
      end;
    
    const
      READ_LINK_POLICY_RP_SIZE = 5;
      OCF_WRITE_LINK_POLICY = $000D;
    
    type
      write_link_policy_cp = record
        handle: cuint16;
        policy: cuint16;
      end;
    
    const
      WRITE_LINK_POLICY_CP_SIZE = 4;
    
    type
      write_link_policy_rp = record
        status: cuint8;
        handle: cuint16;
      end;
    
    const
      WRITE_LINK_POLICY_RP_SIZE = 3;
      OCF_READ_DEFAULT_LINK_POLICY = $000E;
      OCF_WRITE_DEFAULT_LINK_POLICY = $000F;
      OCF_FLOW_SPECIFICATION = $0010;
      OCF_SNIFF_SUBRATING = $0011;
    
    type
      sniff_subrating_cp = record
        handle: cuint16;
        max_latency: cuint16;
        min_remote_timeout: cuint16;
        min_local_timeout: cuint16;
      end;
    
    const
      SNIFF_SUBRATING_CP_SIZE = 8;
      OGF_HOST_CTL = $03;
      OCF_SET_EVENT_MASK = $0001;
    
    type
      TArray0to81Ofcuint8 = array[0..8-1] of cuint8;
      set_event_mask_cp = record
        mask: TArray0to81Ofcuint8;
      end;
    
    const
      SET_EVENT_MASK_CP_SIZE = 8;
      OCF_RESET = $0003;
      OCF_SET_EVENT_FLT = $0005;
    
    type
      set_event_flt_cp = record
        flt_type: cuint8;
        cond_type: cuint8;
        condition: TArray0to0Ofcuint8;
      end;
    
    const
      SET_EVENT_FLT_CP_SIZE = 2;
      FLT_CLEAR_ALL = $00;
      FLT_INQ_RESULT = $01;
      FLT_CONN_SETUP = $02;
      INQ_RESULT_RETURN_ALL = $00;
      INQ_RESULT_RETURN_CLASS = $01;
      INQ_RESULT_RETURN_BDADDR = $02;
      CONN_SETUP_ALLOW_ALL = $00;
      CONN_SETUP_ALLOW_CLASS = $01;
      CONN_SETUP_ALLOW_BDADDR = $02;
      CONN_SETUP_AUTO_OFF = $01;
      CONN_SETUP_AUTO_ON = $02;
      OCF_FLUSH = $0008;
      OCF_READ_PIN_TYPE = $0009;
    
    type
      read_pin_type_rp = record
        status: cuint8;
        pin_type: cuint8;
      end;
    
    const
      READ_PIN_TYPE_RP_SIZE = 2;
      OCF_WRITE_PIN_TYPE = $000A;
    
    type
      write_pin_type_cp = record
        pin_type: cuint8;
      end;
    
    const
      WRITE_PIN_TYPE_CP_SIZE = 1;
      OCF_CREATE_NEW_UNIT_KEY = $000B;
      OCF_READ_STORED_LINK_KEY = $000D;
    
    type
      read_stored_link_key_cp = record
        bdaddr: bdaddr_t;
        read_all: cuint8;
      end;
    
    const
      READ_STORED_LINK_KEY_CP_SIZE = 7;
    
    type
      read_stored_link_key_rp = record
        status: cuint8;
        max_keys: cuint16;
        num_keys: cuint16;
      end;
    
    const
      READ_STORED_LINK_KEY_RP_SIZE = 5;
      OCF_WRITE_STORED_LINK_KEY = $0011;
    
    type
      write_stored_link_key_cp = record
        num_keys: cuint8;
      end;
    
    const
      WRITE_STORED_LINK_KEY_CP_SIZE = 1;
    
    type
      write_stored_link_key_rp = record
        status: cuint8;
        num_keys: cuint8;
      end;
    
    const
      READ_WRITE_LINK_KEY_RP_SIZE = 2;
      OCF_DELETE_STORED_LINK_KEY = $0012;
    
    type
      delete_stored_link_key_cp = record
        bdaddr: bdaddr_t;
        delete_all: cuint8;
      end;
    
    const
      DELETE_STORED_LINK_KEY_CP_SIZE = 7;
    
    type
      delete_stored_link_key_rp = record
        status: cuint8;
        num_keys: cuint16;
      end;
    
    const
      DELETE_STORED_LINK_KEY_RP_SIZE = 3;
      OCF_CHANGE_LOCAL_NAME = $0013;
    
    type
      TArray0to2481Ofcuint8 = array[0..248-1] of cuint8;
      change_local_name_cp = record
        name: TArray0to2481Ofcuint8;
      end;
    
    const
      CHANGE_LOCAL_NAME_CP_SIZE = 248;
      OCF_READ_LOCAL_NAME = $0014;
    
    type
      read_local_name_rp = record
        status: cuint8;
        name: TArray0to2481Ofcuint8;
      end;
    
    const
      READ_LOCAL_NAME_RP_SIZE = 249;
      OCF_READ_CONN_ACCEPT_TIMEOUT = $0015;
    
    type
      read_conn_accept_timeout_rp = record
        status: cuint8;
        timeout: cuint16;
      end;
    
    const
      READ_CONN_ACCEPT_TIMEOUT_RP_SIZE = 3;
      OCF_WRITE_CONN_ACCEPT_TIMEOUT = $0016;
    
    type
      write_conn_accept_timeout_cp = record
        timeout: cuint16;
      end;
    
    const
      WRITE_CONN_ACCEPT_TIMEOUT_CP_SIZE = 2;
      OCF_READ_PAGE_TIMEOUT = $0017;
    
    type
      read_page_timeout_rp = record
        status: cuint8;
        timeout: cuint16;
      end;
    
    const
      READ_PAGE_TIMEOUT_RP_SIZE = 3;
      OCF_WRITE_PAGE_TIMEOUT = $0018;
    
    type
      write_page_timeout_cp = record
        timeout: cuint16;
      end;
    
    const
      WRITE_PAGE_TIMEOUT_CP_SIZE = 2;
      OCF_READ_SCAN_ENABLE = $0019;
    
    type
      read_scan_enable_rp = record
        status: cuint8;
        enable: cuint8;
      end;
    
    const
      READ_SCAN_ENABLE_RP_SIZE = 2;
      OCF_WRITE_SCAN_ENABLE = $001A;
      SCAN_DISABLED = $00;
      SCAN_INQUIRY = $01;
      SCAN_PAGE = $02;
      OCF_READ_PAGE_ACTIVITY = $001B;
    
    type
      read_page_activity_rp = record
        status: cuint8;
        interval: cuint16;
        window: cuint16;
      end;
    
    const
      READ_PAGE_ACTIVITY_RP_SIZE = 5;
      OCF_WRITE_PAGE_ACTIVITY = $001C;
    
    type
      write_page_activity_cp = record
        interval: cuint16;
        window: cuint16;
      end;
    
    const
      WRITE_PAGE_ACTIVITY_CP_SIZE = 4;
      OCF_READ_INQ_ACTIVITY = $001D;
    
    type
      read_inq_activity_rp = record
        status: cuint8;
        interval: cuint16;
        window: cuint16;
      end;
    
    const
      READ_INQ_ACTIVITY_RP_SIZE = 5;
      OCF_WRITE_INQ_ACTIVITY = $001E;
    
    type
      write_inq_activity_cp = record
        interval: cuint16;
        window: cuint16;
      end;
    
    const
      WRITE_INQ_ACTIVITY_CP_SIZE = 4;
      OCF_READ_AUTH_ENABLE = $001F;
      OCF_WRITE_AUTH_ENABLE = $0020;
      AUTH_DISABLED = $00;
      AUTH_ENABLED = $01;
      OCF_READ_ENCRYPT_MODE = $0021;
      OCF_WRITE_ENCRYPT_MODE = $0022;
      ENCRYPT_DISABLED = $00;
      ENCRYPT_P2P = $01;
      ENCRYPT_BOTH = $02;
      OCF_READ_CLASS_OF_DEV = $0023;
    
    type
      read_class_of_dev_rp = record
        status: cuint8;
        dev_class: TArray0to31Ofcuint8;
      end;
    
    const
      READ_CLASS_OF_DEV_RP_SIZE = 4;
      OCF_WRITE_CLASS_OF_DEV = $0024;
    
    type
      write_class_of_dev_cp = record
        dev_class: TArray0to31Ofcuint8;
      end;
    
    const
      WRITE_CLASS_OF_DEV_CP_SIZE = 3;
      OCF_READ_VOICE_SETTING = $0025;
    
    type
      read_voice_setting_rp = record
        status: cuint8;
        voice_setting: cuint16;
      end;
    
    const
      READ_VOICE_SETTING_RP_SIZE = 3;
      OCF_WRITE_VOICE_SETTING = $0026;
    
    type
      write_voice_setting_cp = record
        voice_setting: cuint16;
      end;
    
    const
      WRITE_VOICE_SETTING_CP_SIZE = 2;
      OCF_READ_AUTOMATIC_FLUSH_TIMEOUT = $0027;
      OCF_WRITE_AUTOMATIC_FLUSH_TIMEOUT = $0028;
      OCF_READ_NUM_BROADCAST_RETRANS = $0029;
      OCF_WRITE_NUM_BROADCAST_RETRANS = $002A;
      OCF_READ_HOLD_MODE_ACTIVITY = $002B;
      OCF_WRITE_HOLD_MODE_ACTIVITY = $002C;
      OCF_READ_TRANSMIT_POWER_LEVEL = $002D;
    
    type
      read_transmit_power_level_cp = record
        handle: cuint16;
        type_: cuint8;
      end;
    
    const
      READ_TRANSMIT_POWER_LEVEL_CP_SIZE = 3;
    
    type
      read_transmit_power_level_rp = record
        status: cuint8;
        handle: cuint16;
        level: cint8;
      end;
    
    const
      READ_TRANSMIT_POWER_LEVEL_RP_SIZE = 4;
      OCF_SET_CONTROLLER_TO_HOST_FC = $0031;
      OCF_HOST_BUFFER_SIZE = $0033;
    
    type
      host_buffer_size_cp = record
        acl_mtu: cuint16;
        sco_mtu: cuint8;
        acl_max_pkt: cuint16;
        sco_max_pkt: cuint16;
      end;
    
    const
      HOST_BUFFER_SIZE_CP_SIZE = 7;
      OCF_HOST_NUM_COMP_PKTS = $0035;
    
    type
      host_num_comp_pkts_cp = record
        num_hndl: cuint8;
      end;
    
    const
      HOST_NUM_COMP_PKTS_CP_SIZE = 1;
      OCF_READ_LINK_SUPERVISION_TIMEOUT = $0036;
    
    type
      read_link_supervision_timeout_rp = record
        status: cuint8;
        handle: cuint16;
        timeout: cuint16;
      end;
    
    const
      READ_LINK_SUPERVISION_TIMEOUT_RP_SIZE = 5;
      OCF_WRITE_LINK_SUPERVISION_TIMEOUT = $0037;
    
    type
      write_link_supervision_timeout_cp = record
        handle: cuint16;
        timeout: cuint16;
      end;
    
    const
      WRITE_LINK_SUPERVISION_TIMEOUT_CP_SIZE = 4;
    
    type
      write_link_supervision_timeout_rp = record
        status: cuint8;
        handle: cuint16;
      end;
    
    const
      WRITE_LINK_SUPERVISION_TIMEOUT_RP_SIZE = 3;
      OCF_READ_NUM_SUPPORTED_IAC = $0038;
      MAX_IAC_LAP = $40;
      OCF_READ_CURRENT_IAC_LAP = $0039;
    
    type
      TArray0to31OfArray0toMAX_IAC_LAP1Ofcuint8 = array[0..3-1] of array[0..MAX_IAC_LAP-1] of cuint8;
      read_current_iac_lap_rp = record
        status: cuint8;
        num_current_iac: cuint8;
        lap: TArray0to31OfArray0toMAX_IAC_LAP1Ofcuint8;
      end;
    
    const
      READ_CURRENT_IAC_LAP_RP_SIZE = 2+3*MAX_IAC_LAP;
      OCF_WRITE_CURRENT_IAC_LAP = $003A;
    
    type
      write_current_iac_lap_cp = record
        num_current_iac: cuint8;
        lap: TArray0to31OfArray0toMAX_IAC_LAP1Ofcuint8;
      end;
    
    const
      WRITE_CURRENT_IAC_LAP_CP_SIZE = 1+3*MAX_IAC_LAP;
      OCF_READ_PAGE_SCAN_PERIOD_MODE = $003B;
      OCF_WRITE_PAGE_SCAN_PERIOD_MODE = $003C;
      OCF_READ_PAGE_SCAN_MODE = $003D;
      OCF_WRITE_PAGE_SCAN_MODE = $003E;
      OCF_SET_AFH_CLASSIFICATION = $003F;
    
    type
      TArray0to101Ofcuint8 = array[0..10-1] of cuint8;
      set_afh_classification_cp = record
        map: TArray0to101Ofcuint8;
      end;
    
    const
      SET_AFH_CLASSIFICATION_CP_SIZE = 10;
    
    type
      set_afh_classification_rp = record
        status: cuint8;
      end;
    
    const
      SET_AFH_CLASSIFICATION_RP_SIZE = 1;
      OCF_READ_INQUIRY_SCAN_TYPE = $0042;
    
    type
      read_inquiry_scan_type_rp = record
        status: cuint8;
        type_: cuint8;
      end;
    
    const
      READ_INQUIRY_SCAN_TYPE_RP_SIZE = 2;
      OCF_WRITE_INQUIRY_SCAN_TYPE = $0043;
    
    type
      write_inquiry_scan_type_cp = record
        type_: cuint8;
      end;
    
    const
      WRITE_INQUIRY_SCAN_TYPE_CP_SIZE = 1;
    
    type
      write_inquiry_scan_type_rp = record
        status: cuint8;
      end;
    
    const
      WRITE_INQUIRY_SCAN_TYPE_RP_SIZE = 1;
      OCF_READ_INQUIRY_MODE = $0044;
    
    type
      read_inquiry_mode_rp = record
        status: cuint8;
        mode: cuint8;
      end;
    
    const
      READ_INQUIRY_MODE_RP_SIZE = 2;
      OCF_WRITE_INQUIRY_MODE = $0045;
    
    type
      write_inquiry_mode_cp = record
        mode: cuint8;
      end;
    
    const
      WRITE_INQUIRY_MODE_CP_SIZE = 1;
    
    type
      write_inquiry_mode_rp = record
        status: cuint8;
      end;
    
    const
      WRITE_INQUIRY_MODE_RP_SIZE = 1;
      OCF_READ_PAGE_SCAN_TYPE = $0046;
      OCF_WRITE_PAGE_SCAN_TYPE = $0047;
      OCF_READ_AFH_MODE = $0048;
    
    type
      read_afh_mode_rp = record
        status: cuint8;
        mode: cuint8;
      end;
    
    const
      READ_AFH_MODE_RP_SIZE = 2;
      OCF_WRITE_AFH_MODE = $0049;
    
    type
      write_afh_mode_cp = record
        mode: cuint8;
      end;
    
    const
      WRITE_AFH_MODE_CP_SIZE = 1;
    
    type
      write_afh_mode_rp = record
        status: cuint8;
      end;
    
    const
      WRITE_AFH_MODE_RP_SIZE = 1;
      OCF_READ_EXT_INQUIRY_RESPONSE = $0051;
    
    type
      TArray0to2401Ofcuint8 = array[0..240-1] of cuint8;
      read_ext_inquiry_response_rp = record
        status: cuint8;
        fec: cuint8;
        data: TArray0to2401Ofcuint8;
      end;
    
    const
      READ_EXT_INQUIRY_RESPONSE_RP_SIZE = 242;
      OCF_WRITE_EXT_INQUIRY_RESPONSE = $0052;
    
    type
      write_ext_inquiry_response_cp = record
        fec: cuint8;
        data: TArray0to2401Ofcuint8;
      end;
    
    const
      WRITE_EXT_INQUIRY_RESPONSE_CP_SIZE = 241;
    
    type
      write_ext_inquiry_response_rp = record
        status: cuint8;
      end;
    
    const
      WRITE_EXT_INQUIRY_RESPONSE_RP_SIZE = 1;
      OCF_REFRESH_ENCRYPTION_KEY = $0053;
    
    type
      refresh_encryption_key_cp = record
        handle: cuint16;
      end;
    
    const
      REFRESH_ENCRYPTION_KEY_CP_SIZE = 2;
    
    type
      refresh_encryption_key_rp = record
        status: cuint8;
      end;
    
    const
      REFRESH_ENCRYPTION_KEY_RP_SIZE = 1;
      OCF_READ_SIMPLE_PAIRING_MODE = $0055;
    
    type
      read_simple_pairing_mode_rp = record
        status: cuint8;
        mode: cuint8;
      end;
    
    const
      READ_SIMPLE_PAIRING_MODE_RP_SIZE = 2;
      OCF_WRITE_SIMPLE_PAIRING_MODE = $0056;
    
    type
      write_simple_pairing_mode_cp = record
        mode: cuint8;
      end;
    
    const
      WRITE_SIMPLE_PAIRING_MODE_CP_SIZE = 1;
    
    type
      write_simple_pairing_mode_rp = record
        status: cuint8;
      end;
    
    const
      WRITE_SIMPLE_PAIRING_MODE_RP_SIZE = 1;
      OCF_READ_LOCAL_OOB_DATA = $0057;
    
    type
      read_local_oob_data_rp = record
        status: cuint8;
        hash: TArray0to161Ofcuint8;
        randomizer: TArray0to161Ofcuint8;
      end;
    
    const
      READ_LOCAL_OOB_DATA_RP_SIZE = 33;
      OCF_READ_INQUIRY_TRANSMIT_POWER_LEVEL = $0058;
    
    type
      read_inquiry_transmit_power_level_rp = record
        status: cuint8;
        level: cint8;
      end;
    
    const
      READ_INQUIRY_TRANSMIT_POWER_LEVEL_RP_SIZE = 2;
      OCF_WRITE_INQUIRY_TRANSMIT_POWER_LEVEL = $0059;
    
    type
      write_inquiry_transmit_power_level_cp = record
        level: cint8;
      end;
    
    const
      WRITE_INQUIRY_TRANSMIT_POWER_LEVEL_CP_SIZE = 1;
    
    type
      write_inquiry_transmit_power_level_rp = record
        status: cuint8;
      end;
    
    const
      WRITE_INQUIRY_TRANSMIT_POWER_LEVEL_RP_SIZE = 1;
      OCF_READ_DEFAULT_ERROR_DATA_REPORTING = $005A;
    
    type
      read_default_error_data_reporting_rp = record
        status: cuint8;
        reporting: cuint8;
      end;
    
    const
      READ_DEFAULT_ERROR_DATA_REPORTING_RP_SIZE = 2;
      OCF_WRITE_DEFAULT_ERROR_DATA_REPORTING = $005B;
    
    type
      write_default_error_data_reporting_cp = record
        reporting: cuint8;
      end;
    
    const
      WRITE_DEFAULT_ERROR_DATA_REPORTING_CP_SIZE = 1;
    
    type
      write_default_error_data_reporting_rp = record
        status: cuint8;
      end;
    
    const
      WRITE_DEFAULT_ERROR_DATA_REPORTING_RP_SIZE = 1;
      OCF_ENHANCED_FLUSH = $005F;
    
    type
      enhanced_flush_cp = record
        handle: cuint16;
        type_: cuint8;
      end;
    
    const
      ENHANCED_FLUSH_CP_SIZE = 3;
      OCF_SEND_KEYPRESS_NOTIFY = $0060;
    
    type
      send_keypress_notify_cp = record
        bdaddr: bdaddr_t;
        type_: cuint8;
      end;
    
    const
      SEND_KEYPRESS_NOTIFY_CP_SIZE = 7;
    
    type
      send_keypress_notify_rp = record
        status: cuint8;
      end;
    
    const
      SEND_KEYPRESS_NOTIFY_RP_SIZE = 1;
      OGF_INFO_PARAM = $04;
      OCF_READ_LOCAL_VERSION = $0001;
    
    type
      read_local_version_rp = record
        status: cuint8;
        hci_ver: cuint8;
        hci_rev: cuint16;
        lmp_ver: cuint8;
        manufacturer: cuint16;
        lmp_subver: cuint16;
      end;
    
    const
      READ_LOCAL_VERSION_RP_SIZE = 9;
      OCF_READ_LOCAL_COMMANDS = $0002;
    
    type
      TArray0to641Ofcuint8 = array[0..64-1] of cuint8;
      read_local_commands_rp = record
        status: cuint8;
        commands: TArray0to641Ofcuint8;
      end;
    
    const
      READ_LOCAL_COMMANDS_RP_SIZE = 65;
      OCF_READ_LOCAL_FEATURES = $0003;
    
    type
      read_local_features_rp = record
        status: cuint8;
        features: TArray0to81Ofcuint8;
      end;
    
    const
      READ_LOCAL_FEATURES_RP_SIZE = 9;
      OCF_READ_LOCAL_EXT_FEATURES = $0004;
    
    type
      read_local_ext_features_cp = record
        page_num: cuint8;
      end;
    
    const
      READ_LOCAL_EXT_FEATURES_CP_SIZE = 1;
    
    type
      read_local_ext_features_rp = record
        status: cuint8;
        page_num: cuint8;
        max_page_num: cuint8;
        features: TArray0to81Ofcuint8;
      end;
    
    const
      READ_LOCAL_EXT_FEATURES_RP_SIZE = 11;
      OCF_READ_BUFFER_SIZE = $0005;
    
    type
      read_buffer_size_rp = record
        status: cuint8;
        acl_mtu: cuint16;
        sco_mtu: cuint8;
        acl_max_pkt: cuint16;
        sco_max_pkt: cuint16;
      end;
    
    const
      READ_BUFFER_SIZE_RP_SIZE = 8;
      OCF_READ_COUNTRY_CODE = $0007;
      OCF_READ_BD_ADDR = $0009;
    
    type
      read_bd_addr_rp = record
        status: cuint8;
        bdaddr: bdaddr_t;
      end;
    
    const
      READ_BD_ADDR_RP_SIZE = 7;
      OGF_STATUS_PARAM = $05;
      OCF_READ_FAILED_CONTACT_COUNTER = $0001;
    
    type
      read_failed_contact_counter_rp = record
        status: cuint8;
        handle: cuint16;
        counter: cuint8;
      end;
    
    const
      READ_FAILED_CONTACT_COUNTER_RP_SIZE = 4;
      OCF_RESET_FAILED_CONTACT_COUNTER = $0002;
    
    type
      reset_failed_contact_counter_rp = record
        status: cuint8;
        handle: cuint16;
      end;
    
    const
      RESET_FAILED_CONTACT_COUNTER_RP_SIZE = 4;
      OCF_READ_LINK_QUALITY = $0003;
    
    type
      read_link_quality_rp = record
        status: cuint8;
        handle: cuint16;
        link_quality: cuint8;
      end;
    
    const
      READ_LINK_QUALITY_RP_SIZE = 4;
      OCF_READ_RSSI = $0005;
    
    type
      read_rssi_rp = record
        status: cuint8;
        handle: cuint16;
        rssi: cint8;
      end;
    
    const
      READ_RSSI_RP_SIZE = 4;
      OCF_READ_AFH_MAP = $0006;
    
    type
      read_afh_map_rp = record
        status: cuint8;
        handle: cuint16;
        mode: cuint8;
        map: TArray0to101Ofcuint8;
      end;
    
    const
      READ_AFH_MAP_RP_SIZE = 14;
      OCF_READ_CLOCK = $0007;
    
    type
      read_clock_cp = record
        handle: cuint16;
        which_clock: cuint8;
      end;
    
    const
      READ_CLOCK_CP_SIZE = 3;
    
    type
      read_clock_rp = record
        status: cuint8;
        handle: cuint16;
        clock: cuint32;
        accuracy: cuint16;
      end;
    
    const
      READ_CLOCK_RP_SIZE = 9;
      OGF_TESTING_CMD = $3e;
      OCF_READ_LOOPBACK_MODE = $0001;
      OCF_WRITE_LOOPBACK_MODE = $0002;
      OCF_ENABLE_DEVICE_UNDER_TEST_MODE = $0003;
      OCF_WRITE_SIMPLE_PAIRING_DEBUG_MODE = $0004;
    
    type
      write_simple_pairing_debug_mode_cp = record
        mode: cuint8;
      end;
    
    const
      WRITE_SIMPLE_PAIRING_DEBUG_MODE_CP_SIZE = 1;
    
    type
      write_simple_pairing_debug_mode_rp = record
        status: cuint8;
      end;
    
    const
      WRITE_SIMPLE_PAIRING_DEBUG_MODE_RP_SIZE = 1;
      OGF_VENDOR_CMD = $3f;
      EVT_INQUIRY_COMPLETE = $01;
      EVT_INQUIRY_RESULT = $02;
    
    type
      inquiry_info = record
        bdaddr: bdaddr_t;
        pscan_rep_mode: cuint8;
        pscan_period_mode: cuint8;
        pscan_mode: cuint8;
        dev_class: TArray0to31Ofcuint8;
        clock_offset: cuint16;
      end;
    
    const
      INQUIRY_INFO_SIZE = 14;
      EVT_CONN_COMPLETE_1 = $03;
    
    type
      evt_conn_complete = record
        status: cuint8;
        handle: cuint16;
        bdaddr: bdaddr_t;
        link_type: cuint8;
        encr_mode: cuint8;
      end;
    
    const
      EVT_CONN_COMPLETE_SIZE = 13;
      EVT_CONN_REQUEST_1 = $04;
    
    type
      evt_conn_request = record
        bdaddr: bdaddr_t;
        dev_class: TArray0to31Ofcuint8;
        link_type: cuint8;
      end;
    
    const
      EVT_CONN_REQUEST_SIZE = 10;
      EVT_DISCONN_COMPLETE_1 = $05;
    
    type
      evt_disconn_complete = record
        status: cuint8;
        handle: cuint16;
        reason: cuint8;
      end;
    
    const
      EVT_DISCONN_COMPLETE_SIZE = 4;
      EVT_AUTH_COMPLETE_1 = $06;
    
    type
      evt_auth_complete = record
        status: cuint8;
        handle: cuint16;
      end;
    
    const
      EVT_AUTH_COMPLETE_SIZE = 3;
      EVT_REMOTE_NAME_REQ_COMPLETE_1 = $07;
    
    type
      evt_remote_name_req_complete = record
        status: cuint8;
        bdaddr: bdaddr_t;
        name: TArray0to2481Ofcuint8;
      end;
    
    const
      EVT_REMOTE_NAME_REQ_COMPLETE_SIZE = 255;
      EVT_ENCRYPT_CHANGE_1 = $08;
    
    type
      evt_encrypt_change = record
        status: cuint8;
        handle: cuint16;
        encrypt: cuint8;
      end;
    
    const
      EVT_ENCRYPT_CHANGE_SIZE = 5;
      EVT_CHANGE_CONN_LINK_KEY_COMPLETE_1 = $09;
    
    type
      evt_change_conn_link_key_complete = record
        status: cuint8;
        handle: cuint16;
      end;
    
    const
      EVT_CHANGE_CONN_LINK_KEY_COMPLETE_SIZE = 3;
      EVT_MASTER_LINK_KEY_COMPLETE_1 = $0A;
    
    type
      evt_master_link_key_complete = record
        status: cuint8;
        handle: cuint16;
        key_flag: cuint8;
      end;
    
    const
      EVT_MASTER_LINK_KEY_COMPLETE_SIZE = 4;
      EVT_READ_REMOTE_FEATURES_COMPLETE_1 = $0B;
    
    type
      evt_read_remote_features_complete = record
        status: cuint8;
        handle: cuint16;
        features: TArray0to81Ofcuint8;
      end;
    
    const
      EVT_READ_REMOTE_FEATURES_COMPLETE_SIZE = 11;
      EVT_READ_REMOTE_VERSION_COMPLETE_1 = $0C;
    
    type
      evt_read_remote_version_complete = record
        status: cuint8;
        handle: cuint16;
        lmp_ver: cuint8;
        manufacturer: cuint16;
        lmp_subver: cuint16;
      end;
    
    const
      EVT_READ_REMOTE_VERSION_COMPLETE_SIZE = 8;
      EVT_QOS_SETUP_COMPLETE_1 = $0D;
    
    type
      evt_qos_setup_complete = record
        status: cuint8;
        handle: cuint16;
        flags: cuint8;
        qos: hci_qos;
      end;
    
    const
      EVT_QOS_SETUP_COMPLETE_SIZE = (4 + HCI_QOS_CP_SIZE);
      EVT_CMD_COMPLETE_1 = $0E;
    
    type
      evt_cmd_complete = record
        ncmd: cuint8;
        opcode: cuint16;
      end;
    
    const
      EVT_CMD_COMPLETE_SIZE = 3;
      EVT_CMD_STATUS_1 = $0F;
    
    type
      evt_cmd_status = record
        status: cuint8;
        ncmd: cuint8;
        opcode: cuint16;
      end;
    
    const
      EVT_CMD_STATUS_SIZE = 4;
      EVT_HARDWARE_ERROR_1 = $10;
    
    type
      evt_hardware_error = record
        code: cuint8;
      end;
    
    const
      EVT_HARDWARE_ERROR_SIZE = 1;
      EVT_FLUSH_OCCURRED = $11;
    
    type
      evt_flush_occured = record
        handle: cuint16;
      end;
    
    const
      EVT_FLUSH_OCCURRED_SIZE = 2;
      EVT_ROLE_CHANGE_1 = $12;
    
    type
      evt_role_change = record
        status: cuint8;
        bdaddr: bdaddr_t;
        role: cuint8;
      end;
    
    const
      EVT_ROLE_CHANGE_SIZE = 8;
      EVT_NUM_COMP_PKTS_1 = $13;
    
    type
      evt_num_comp_pkts = record
        num_hndl: cuint8;
      end;
    
    const
      EVT_NUM_COMP_PKTS_SIZE = 1;
      EVT_MODE_CHANGE_1 = $14;
    
    type
      evt_mode_change = record
        status: cuint8;
        handle: cuint16;
        mode: cuint8;
        interval: cuint16;
      end;
    
    const
      EVT_MODE_CHANGE_SIZE = 6;
      EVT_RETURN_LINK_KEYS_1 = $15;
    
    type
      evt_return_link_keys = record
        num_keys: cuint8;
      end;
    
    const
      EVT_RETURN_LINK_KEYS_SIZE = 1;
      EVT_PIN_CODE_REQ_1 = $16;
    
    type
      evt_pin_code_req = record
        bdaddr: bdaddr_t;
      end;
    
    const
      EVT_PIN_CODE_REQ_SIZE = 6;
      EVT_LINK_KEY_REQ_1 = $17;
    
    type
      evt_link_key_req = record
        bdaddr: bdaddr_t;
      end;
    
    const
      EVT_LINK_KEY_REQ_SIZE = 6;
      EVT_LINK_KEY_NOTIFY_1 = $18;
    
    type
      evt_link_key_notify = record
        bdaddr: bdaddr_t;
        link_key: TArray0to161Ofcuint8;
        key_type: cuint8;
      end;
    
    const
      EVT_LINK_KEY_NOTIFY_SIZE = 23;
      EVT_LOOPBACK_COMMAND = $19;
      EVT_DATA_BUFFER_OVERFLOW_1 = $1A;
    
    type
      evt_data_buffer_overflow = record
        link_type: cuint8;
      end;
    
    const
      EVT_DATA_BUFFER_OVERFLOW_SIZE = 1;
      EVT_MAX_SLOTS_CHANGE_1 = $1B;
    
    type
      evt_max_slots_change = record
        handle: cuint16;
        max_slots: cuint8;
      end;
    
    const
      EVT_MAX_SLOTS_CHANGE_SIZE = 3;
      EVT_READ_CLOCK_OFFSET_COMPLETE_1 = $1C;
    
    type
      evt_read_clock_offset_complete = record
        status: cuint8;
        handle: cuint16;
        clock_offset: cuint16;
      end;
    
    const
      EVT_READ_CLOCK_OFFSET_COMPLETE_SIZE = 5;
      EVT_CONN_PTYPE_CHANGED_1 = $1D;
    
    type
      evt_conn_ptype_changed = record
        status: cuint8;
        handle: cuint16;
        ptype: cuint16;
      end;
    
    const
      EVT_CONN_PTYPE_CHANGED_SIZE = 5;
      EVT_QOS_VIOLATION_1 = $1E;
    
    type
      evt_qos_violation = record
        handle: cuint16;
      end;
    
    const
      EVT_QOS_VIOLATION_SIZE = 2;
      EVT_PSCAN_REP_MODE_CHANGE_1 = $20;
    
    type
      evt_pscan_rep_mode_change = record
        bdaddr: bdaddr_t;
        pscan_rep_mode: cuint8;
      end;
    
    const
      EVT_PSCAN_REP_MODE_CHANGE_SIZE = 7;
      EVT_FLOW_SPEC_COMPLETE_1 = $21;
    
    type
      evt_flow_spec_complete = record
        status: cuint8;
        handle: cuint16;
        flags: cuint8;
        direction: cuint8;
        qos: hci_qos;
      end;
    
    const
      EVT_FLOW_SPEC_COMPLETE_SIZE = (5 + HCI_QOS_CP_SIZE);
      EVT_INQUIRY_RESULT_WITH_RSSI = $22;
    
    type
      inquiry_info_with_rssi = record
        bdaddr: bdaddr_t;
        pscan_rep_mode: cuint8;
        pscan_period_mode: cuint8;
        dev_class: TArray0to31Ofcuint8;
        clock_offset: cuint16;
        rssi: cint8;
      end;
    
    const
      INQUIRY_INFO_WITH_RSSI_SIZE = 14;
    
    type
      inquiry_info_with_rssi_and_pscan_mode = record
        bdaddr: bdaddr_t;
        pscan_rep_mode: cuint8;
        pscan_period_mode: cuint8;
        pscan_mode: cuint8;
        dev_class: TArray0to31Ofcuint8;
        clock_offset: cuint16;
        rssi: cint8;
      end;
    
    const
      INQUIRY_INFO_WITH_RSSI_AND_PSCAN_MODE_SIZE = 15;
      EVT_READ_REMOTE_EXT_FEATURES_COMPLETE_1 = $23;
    
    type
      evt_read_remote_ext_features_complete = record
        status: cuint8;
        handle: cuint16;
        page_num: cuint8;
        max_page_num: cuint8;
        features: TArray0to81Ofcuint8;
      end;
    
    const
      EVT_READ_REMOTE_EXT_FEATURES_COMPLETE_SIZE = 13;
      EVT_SYNC_CONN_COMPLETE_1 = $2C;
    
    type
      evt_sync_conn_complete = record
        status: cuint8;
        handle: cuint16;
        bdaddr: bdaddr_t;
        link_type: cuint8;
        trans_interval: cuint8;
        retrans_window: cuint8;
        rx_pkt_len: cuint16;
        tx_pkt_len: cuint16;
        air_mode: cuint8;
      end;
    
    const
      EVT_SYNC_CONN_COMPLETE_SIZE = 17;
      EVT_SYNC_CONN_CHANGED_1 = $2D;
    
    type
      evt_sync_conn_changed = record
        status: cuint8;
        handle: cuint16;
        trans_interval: cuint8;
        retrans_window: cuint8;
        rx_pkt_len: cuint16;
        tx_pkt_len: cuint16;
      end;
    
    const
      EVT_SYNC_CONN_CHANGED_SIZE = 9;
      EVT_SNIFF_SUBRATING_1 = $2E;
    
    type
      evt_sniff_subrating = record
        status: cuint8;
        handle: cuint16;
        max_tx_latency: cuint16;
        max_rx_latency: cuint16;
        min_remote_timeout: cuint16;
        min_local_timeout: cuint16;
      end;
    
    const
      EVT_SNIFF_SUBRATING_SIZE = 11;
      EVT_EXTENDED_INQUIRY_RESULT = $2F;
    
    type
      extended_inquiry_info = record
        bdaddr: bdaddr_t;
        pscan_rep_mode: cuint8;
        pscan_period_mode: cuint8;
        dev_class: TArray0to31Ofcuint8;
        clock_offset: cuint16;
        rssi: cint8;
        data: TArray0to2401Ofcuint8;
      end;
    
    const
      EXTENDED_INQUIRY_INFO_SIZE = 254;
      EVT_ENCRYPTION_KEY_REFRESH_COMPLETE_1 = $30;
    
    type
      evt_encryption_key_refresh_complete = record
        status: cuint8;
        handle: cuint16;
      end;
    
    const
      EVT_ENCRYPTION_KEY_REFRESH_COMPLETE_SIZE = 3;
      EVT_IO_CAPABILITY_REQUEST_1 = $31;
    
    type
      evt_io_capability_request = record
        bdaddr: bdaddr_t;
      end;
    
    const
      EVT_IO_CAPABILITY_REQUEST_SIZE = 6;
      EVT_IO_CAPABILITY_RESPONSE_1 = $32;
    
    type
      evt_io_capability_response = record
        bdaddr: bdaddr_t;
        capability: cuint8;
        oob_data: cuint8;
        authentication: cuint8;
      end;
    
    const
      EVT_IO_CAPABILITY_RESPONSE_SIZE = 9;
      EVT_USER_CONFIRM_REQUEST_1 = $33;
    
    type
      evt_user_confirm_request = record
        bdaddr: bdaddr_t;
        passkey: cuint32;
      end;
    
    const
      EVT_USER_CONFIRM_REQUEST_SIZE = 10;
      EVT_USER_PASSKEY_REQUEST_1 = $34;
    
    type
      evt_user_passkey_request = record
        bdaddr: bdaddr_t;
      end;
    
    const
      EVT_USER_PASSKEY_REQUEST_SIZE = 6;
      EVT_REMOTE_OOB_DATA_REQUEST_1 = $35;
    
    type
      evt_remote_oob_data_request = record
        bdaddr: bdaddr_t;
      end;
    
    const
      EVT_REMOTE_OOB_DATA_REQUEST_SIZE = 6;
      EVT_SIMPLE_PAIRING_COMPLETE_1 = $36;
    
    type
      evt_simple_pairing_complete = record
        status: cuint8;
        bdaddr: bdaddr_t;
      end;
    
    const
      EVT_SIMPLE_PAIRING_COMPLETE_SIZE = 7;
      EVT_LINK_SUPERVISION_TIMEOUT_CHANGED_1 = $38;
    
    type
      evt_link_supervision_timeout_changed = record
        handle: cuint16;
        timeout: cuint16;
      end;
    
    const
      EVT_LINK_SUPERVISION_TIMEOUT_CHANGED_SIZE = 4;
      EVT_ENHANCED_FLUSH_COMPLETE_1 = $39;
    
    type
      evt_enhanced_flush_complete = record
        handle: cuint16;
      end;
    
    const
      EVT_ENHANCED_FLUSH_COMPLETE_SIZE = 2;
      EVT_USER_PASSKEY_NOTIFY_1 = $3B;
    
    type
      evt_user_passkey_notify = record
        bdaddr: bdaddr_t;
        passkey: cuint32;
      end;
    
    const
      EVT_USER_PASSKEY_NOTIFY_SIZE = 10;
      EVT_KEYPRESS_NOTIFY_1 = $3C;
    
    type
      evt_keypress_notify = record
        bdaddr: bdaddr_t;
        type_: cuint8;
      end;
    
    const
      EVT_KEYPRESS_NOTIFY_SIZE = 7;
      EVT_REMOTE_HOST_FEATURES_NOTIFY_1 = $3D;
    
    type
      evt_remote_host_features_notify = record
        bdaddr: bdaddr_t;
        features: TArray0to81Ofcuint8;
      end;
    
    const
      EVT_REMOTE_HOST_FEATURES_NOTIFY_SIZE = 14;
      EVT_TESTING = $FE;
      EVT_VENDOR = $FF;
      EVT_STACK_INTERNAL_1 = $FD;
    
    type
      evt_stack_internal = record
        type_: cuint16;
        data: TArray0to0Ofcuint8;
      end;
    
    const
      EVT_STACK_INTERNAL_SIZE = 2;
      EVT_SI_DEVICE_1 = $01;
    
    type
      evt_si_device = record
        event: cuint16;
        dev_id: cuint16;
      end;
    
    const
      EVT_SI_DEVICE_SIZE = 4;
      EVT_SI_SECURITY_1 = $02;
    
    type
      evt_si_security = record
        event: cuint16;
        proto: cuint16;
        subproto: cuint16;
        incoming: cuint8;
      end;
    
    const
      HCI_TYPE_LEN = 1;
    
    type
      hci_command_hdr = record
        opcode: cuint16;
        plen: cuint8;
      end;
    
    const
      HCI_COMMAND_HDR_SIZE = 3;
    
    type
      hci_event_hdr = record
        evt: cuint8;
        plen: cuint8;
      end;
    
    const
      HCI_EVENT_HDR_SIZE = 2;
    
    type
      hci_acl_hdr = record
        handle: cuint16;
        dlen: cuint16;
      end;
    
    const
      HCI_ACL_HDR_SIZE = 4;
    
    type
      hci_sco_hdr = record
        handle: cuint16;
        dlen: cuint8;
      end;
    
    const
      HCI_SCO_HDR_SIZE = 3;
    
    type
      hci_msg_hdr = record
        device: cuint16;
        type_: cuint16;
        plen: cuint16;
      end;
    
    const
      HCI_MSG_HDR_SIZE = 6;
    
  {$EndIf}
  
  const
    HCI_DATA_DIR = 1;
    HCI_FILTER_1 = 2;
    HCI_TIME_STAMP = 3;
    HCI_CMSG_DIR = $0001;
    HCI_CMSG_TSTAMP = $0002;
  
  type
    sockaddr_hci = record
      hci_family: sa_family_t;
      hci_dev: cushort;
    end;
  
  const
    HCI_DEV_NONE = $ffff;
  
  type
    TArray0to21Ofcuint32 = array[0..2-1] of cuint32;
    hci_filter = record
      type_mask: cuint32;
      event_mask: TArray0to21Ofcuint32;
      opcode: cuint16;
    end;
  
  const
    HCI_FLT_TYPE_BITS = 31;
    HCI_FLT_EVENT_BITS = 63;
    HCI_FLT_OGF_BITS = 63;
    HCI_FLT_OCF_BITS = 127;
  
  type
    hci_dev_stats = record
      err_rx: cuint32;
      err_tx: cuint32;
      cmd_tx: cuint32;
      evt_rx: cuint32;
      acl_tx: cuint32;
      acl_rx: cuint32;
      sco_tx: cuint32;
      sco_rx: cuint32;
      byte_rx: cuint32;
      byte_tx: cuint32;
    end;
    TArray0to81Ofcchar = array[0..8-1] of cchar;
    hci_dev_info = record
      dev_id: cuint16;
      name: TArray0to81Ofcchar;
      bdaddr: bdaddr_t;
      flags: cuint32;
      type_: cuint8;
      features: TArray0to81Ofcuint8;
      pkt_type: cuint32;
      link_policy: cuint32;
      link_mode: cuint32;
      acl_mtu: cuint16;
      acl_pkts: cuint16;
      sco_mtu: cuint16;
      sco_pkts: cuint16;
      stat: hci_dev_stats;
    end;
    hci_conn_info = record
      handle: cuint16;
      bdaddr: bdaddr_t;
      type_: cuint8;
      out: cuint8;
      state: cuint16;
      link_mode: cuint32;
    end;
    hci_dev_req = record
      dev_id: cuint16;
      dev_opt: cuint32;
    end;
    TArray0to0Ofhci_dev_req = array[0..0] of hci_dev_req;
    hci_dev_list_req = record
      dev_num: cuint16;
      dev_req: TArray0to0Ofhci_dev_req;
    end;
    TArray0to0Ofhci_conn_info = array[0..0] of hci_conn_info;
    hci_conn_list_req = record
      dev_id: cuint16;
      conn_num: cuint16;
      conn_info: TArray0to0Ofhci_conn_info;
    end;
    hci_conn_info_req = record
      bdaddr: bdaddr_t;
      type_: cuint8;
      conn_info: TArray0to0Ofhci_conn_info;
    end;
    hci_inquiry_req = record
      dev_id: cuint16;
      flags: cuint16;
      lap: TArray0to31Ofcuint8;
      length: cuint8;
      num_rsp: cuint8;
    end;
  
  const
    IREQ_CACHE_FLUSH = $0001;
  
  type
    hci_remotename_req = record
      dev_id: cuint16;
      flags: cuint16;
      bdaddr: bdaddr_t;
      name: TArray0to2481Ofcuint8;
    end;
  
{$EndIf}
{$IfNDef __HCI_LIB_H}
  
  type
    hci_request = record
      ogf: cuint16;
      ocf: cuint16;
      event: cint;
      cparam: pointer;
      clen: cint;
      rparam: pointer;
      rlen: cint;
    end;
    hci_version = record
      manufacturer: cuint16;
      hci_ver: cuint8;
      hci_rev: cuint16;
      lmp_ver: cuint8;
      lmp_subver: cuint16;
    end;
  
  function hci_open_dev(dev_id: cint): cint; cdecl; external;
  function hci_close_dev(dd: cint): cint; cdecl; external;
  function hci_send_cmd(dd: cint; ogf: cuint16; ocf: cuint16; plen: cuint8; param: pointer): cint; cdecl; external;
  
  type
    Phci_request = ^hci_request;
  
  function hci_send_req(dd: cint; req: Phci_request; timeout: cint): cint; cdecl; external;
  function hci_create_connection(dd: cint; bdaddr: Pbdaddr_t; ptype: cuint16; clkoffset: cuint16; rswitch: cuint8; handle: pcuint16; to_: cint): cint; cdecl; external;
  function hci_disconnect(dd: cint; handle: cuint16; reason: cuint8; to_: cint): cint; cdecl; external;
  
  type
    Pinquiry_info = ^inquiry_info;
    PPinquiry_info = ^Pinquiry_info;
  
  function hci_inquiry_1(dev_id: cint; len: cint; num_rsp: cint; lap: pcuint8; ii: PPinquiry_info; flags: clong): cint; cdecl; external name 'hci_inquiry';
  
  type
    Phci_dev_info = ^hci_dev_info;
  
  function hci_devinfo(dev_id: cint; di: Phci_dev_info): cint; cdecl; external;
  function hci_devba(dev_id: cint; bdaddr: Pbdaddr_t): cint; cdecl; external;
  function hci_devid(str: pcchar): cint; cdecl; external;
  function hci_read_local_name(dd: cint; len: cint; name: pcchar; to_: cint): cint; cdecl; external;
  function hci_write_local_name(dd: cint; name: pcchar; to_: cint): cint; cdecl; external;
  function hci_read_remote_name(dd: cint; bdaddr: Pbdaddr_t; len: cint; name: pcchar; to_: cint): cint; cdecl; external;
  function hci_read_remote_name_with_clock_offset(dd: cint; bdaddr: Pbdaddr_t; pscan_rep_mode: cuint8; clkoffset: cuint16; len: cint; name: pcchar; to_: cint): cint; cdecl; external;
  function hci_read_remote_name_cancel(dd: cint; bdaddr: Pbdaddr_t; to_: cint): cint; cdecl; external;
  
  type
    Phci_version = ^hci_version;
  
  function hci_read_remote_version(dd: cint; handle: cuint16; ver: Phci_version; to_: cint): cint; cdecl; external;
  function hci_read_remote_features(dd: cint; handle: cuint16; features: pcuint8; to_: cint): cint; cdecl; external;
  function hci_read_remote_ext_features(dd: cint; handle: cuint16; page: cuint8; max_page: pcuint8; features: pcuint8; to_: cint): cint; cdecl; external;
  function hci_read_clock_offset(dd: cint; handle: cuint16; clkoffset: pcuint16; to_: cint): cint; cdecl; external;
  function hci_read_local_version(dd: cint; ver: Phci_version; to_: cint): cint; cdecl; external;
  function hci_read_local_commands(dd: cint; commands: pcuint8; to_: cint): cint; cdecl; external;
  function hci_read_local_features(dd: cint; features: pcuint8; to_: cint): cint; cdecl; external;
  function hci_read_local_ext_features(dd: cint; page: cuint8; max_page: pcuint8; features: pcuint8; to_: cint): cint; cdecl; external;
  function hci_read_bd_addr(dd: cint; bdaddr: Pbdaddr_t; to_: cint): cint; cdecl; external;
  function hci_read_class_of_dev(dd: cint; cls: pcuint8; to_: cint): cint; cdecl; external;
  function hci_write_class_of_dev(dd: cint; cls: cuint32; to_: cint): cint; cdecl; external;
  function hci_read_voice_setting(dd: cint; vs: pcuint16; to_: cint): cint; cdecl; external;
  function hci_write_voice_setting(dd: cint; vs: cuint16; to_: cint): cint; cdecl; external;
  function hci_read_current_iac_lap(dd: cint; num_iac: pcuint8; lap: pcuint8; to_: cint): cint; cdecl; external;
  function hci_write_current_iac_lap(dd: cint; num_iac: cuint8; lap: pcuint8; to_: cint): cint; cdecl; external;
  function hci_read_stored_link_key(dd: cint; bdaddr: Pbdaddr_t; all: cuint8; to_: cint): cint; cdecl; external;
  function hci_write_stored_link_key(dd: cint; bdaddr: Pbdaddr_t; key: pcuint8; to_: cint): cint; cdecl; external;
  function hci_delete_stored_link_key(dd: cint; bdaddr: Pbdaddr_t; all: cuint8; to_: cint): cint; cdecl; external;
  function hci_authenticate_link(dd: cint; handle: cuint16; to_: cint): cint; cdecl; external;
  function hci_encrypt_link(dd: cint; handle: cuint16; encrypt: cuint8; to_: cint): cint; cdecl; external;
  function hci_change_link_key(dd: cint; handle: cuint16; to_: cint): cint; cdecl; external;
  function hci_switch_role(dd: cint; bdaddr: Pbdaddr_t; role: cuint8; to_: cint): cint; cdecl; external;
  function hci_park_mode(dd: cint; handle: cuint16; max_interval: cuint16; min_interval: cuint16; to_: cint): cint; cdecl; external;
  function hci_exit_park_mode(dd: cint; handle: cuint16; to_: cint): cint; cdecl; external;
  function hci_read_inquiry_scan_type(dd: cint; type_: pcuint8; to_: cint): cint; cdecl; external;
  function hci_write_inquiry_scan_type(dd: cint; type_: cuint8; to_: cint): cint; cdecl; external;
  function hci_read_inquiry_mode(dd: cint; mode: pcuint8; to_: cint): cint; cdecl; external;
  function hci_write_inquiry_mode(dd: cint; mode: cuint8; to_: cint): cint; cdecl; external;
  function hci_read_afh_mode(dd: cint; mode: pcuint8; to_: cint): cint; cdecl; external;
  function hci_write_afh_mode(dd: cint; mode: cuint8; to_: cint): cint; cdecl; external;
  function hci_read_ext_inquiry_response(dd: cint; fec: pcuint8; data: pcuint8; to_: cint): cint; cdecl; external;
  function hci_write_ext_inquiry_response(dd: cint; fec: cuint8; data: pcuint8; to_: cint): cint; cdecl; external;
  function hci_read_simple_pairing_mode(dd: cint; mode: pcuint8; to_: cint): cint; cdecl; external;
  function hci_write_simple_pairing_mode(dd: cint; mode: cuint8; to_: cint): cint; cdecl; external;
  function hci_read_local_oob_data(dd: cint; hash: pcuint8; randomizer: pcuint8; to_: cint): cint; cdecl; external;
  function hci_read_transmit_power_level(dd: cint; handle: cuint16; type_: cuint8; level: pcint8; to_: cint): cint; cdecl; external;
  function hci_read_link_supervision_timeout(dd: cint; handle: cuint16; timeout: pcuint16; to_: cint): cint; cdecl; external;
  function hci_write_link_supervision_timeout(dd: cint; handle: cuint16; timeout: cuint16; to_: cint): cint; cdecl; external;
  function hci_set_afh_classification(dd: cint; map: pcuint8; to_: cint): cint; cdecl; external;
  function hci_read_link_quality(dd: cint; handle: cuint16; link_quality: pcuint8; to_: cint): cint; cdecl; external;
  function hci_read_rssi(dd: cint; handle: cuint16; rssi: pcint8; to_: cint): cint; cdecl; external;
  function hci_read_afh_map(dd: cint; handle: cuint16; mode: pcuint8; map: pcuint8; to_: cint): cint; cdecl; external;
  function hci_read_clock(dd: cint; handle: cuint16; which: cuint8; clock: pcuint32; accuracy: pcuint16; to_: cint): cint; cdecl; external;
  function hci_local_name(dd: cint; len: cint; name: pcchar; to_: cint): cint; cdecl; external;
  function hci_remote_name(dd: cint; bdaddr: Pbdaddr_t; len: cint; name: pcchar; to_: cint): cint; cdecl; external;
  
  type
    funcintddintdev_idlongarg = function(dd: cint; dev_id: cint; arg: clong): cint; cdecl;
  
  function hci_for_each_dev(flag: cint; func: funcintddintdev_idlongarg; arg: clong): cint; cdecl; external;
  function hci_get_route(bdaddr: Pbdaddr_t): cint; cdecl; external;
  function hci_dtypetostr(type_: cint): pcchar; cdecl; external;
  function hci_dflagstostr(flags: cuint32): pcchar; cdecl; external;
  function hci_ptypetostr(ptype: cuint): pcchar; cdecl; external;
  function hci_strtoptype(str: pcchar; val: pcuint): cint; cdecl; external;
  function hci_scoptypetostr(ptype: cuint): pcchar; cdecl; external;
  function hci_strtoscoptype(str: pcchar; val: pcuint): cint; cdecl; external;
  function hci_lptostr(ptype: cuint): pcchar; cdecl; external;
  function hci_strtolp(str: pcchar; val: pcuint): cint; cdecl; external;
  function hci_lmtostr(ptype: cuint): pcchar; cdecl; external;
  function hci_strtolm(str: pcchar; val: pcuint): cint; cdecl; external;
  function hci_cmdtostr(cmd: cuint): pcchar; cdecl; external;
  function hci_commandstostr(commands: pcuint8; pref: pcchar; width: cint): pcchar; cdecl; external;
  function hci_vertostr(ver: cuint): pcchar; cdecl; external;
  function hci_strtover(str: pcchar; ver: pcuint): cint; cdecl; external;
  function lmp_vertostr(ver: cuint): pcchar; cdecl; external;
  function lmp_strtover(str: pcchar; ver: pcuint): cint; cdecl; external;
  function lmp_featurestostr(features: pcuint8; pref: pcchar; width: cint): pcchar; cdecl; external;
{$EndIf}
{$IfNDef __HIDP_H}
  
  const
    HIDP_MINIMUM_MTU = 48;
    HIDP_DEFAULT_MTU = 48;
  
  {off $Define HIDPCONNADD:=_IOW('H', 200, int)}
  {off $Define HIDPCONNDEL:=_IOW('H', 201, int)}
  {off $Define HIDPGETCONNLIST:=_IOR('H', 210, int)}
  {off $Define HIDPGETCONNINFO:=_IOR('H', 211, int)}
  
  const
    HIDP_VIRTUAL_CABLE_UNPLUG = 0;
    HIDP_BOOT_PROTOCOL_MODE = 1;
    HIDP_BLUETOOTH_VENDOR_ID = 9;
  
  type
    TArray0to1281Ofcchar = array[0..128-1] of cchar;
    hidp_connadd_req = record
      ctrl_sock: cint;
      intr_sock: cint;
      parser: cuint16;
      rd_size: cuint16;
      rd_data: pcuint8;
      country: cuint8;
      subclass: cuint8;
      vendor: cuint16;
      product: cuint16;
      version: cuint16;
      flags: cuint32;
      idle_to: cuint32;
      name: TArray0to1281Ofcchar;
    end;
    hidp_conndel_req = record
      bdaddr: bdaddr_t;
      flags: cuint32;
    end;
    hidp_conninfo = record
      bdaddr: bdaddr_t;
      flags: cuint32;
      state: cuint16;
      vendor: cuint16;
      product: cuint16;
      version: cuint16;
      name: TArray0to1281Ofcchar;
    end;
    Phidp_conninfo = ^hidp_conninfo;
    hidp_connlist_req = record
      cnum: cuint32;
      ci: Phidp_conninfo;
    end;
  
{$EndIf}
{$IfNDef __L2CAP_H}
  
  const
    L2CAP_DEFAULT_MTU = 672;
    L2CAP_DEFAULT_FLUSH_TO = $FFFF;
  
  {off $Define L2CAP_CONN_TIMEOUT:=(HZ * 40)}
  
  type
    sockaddr_l2 = record
      l2_family: sa_family_t;
      l2_psm: cushort;
      l2_bdaddr: bdaddr_t;
    end;
  
  const
    L2CAP_OPTIONS_1 = $01;
  
  type
    l2cap_options = record
      omtu: cuint16;
      imtu: cuint16;
      flush_to: cuint16;
      mode: cuint8;
    end;
  
  const
    L2CAP_CONNINFO_1 = $02;
  
  type
    l2cap_conninfo = record
      hci_handle: cuint16;
      dev_class: TArray0to31Ofcuint8;
    end;
  
  const
    L2CAP_LM = $03;
    L2CAP_LM_MASTER = $0001;
    L2CAP_LM_AUTH = $0002;
    L2CAP_LM_ENCRYPT = $0004;
    L2CAP_LM_TRUSTED = $0008;
    L2CAP_LM_RELIABLE = $0010;
    L2CAP_LM_SECURE = $0020;
    L2CAP_COMMAND_REJ = $01;
    L2CAP_CONN_REQ_1 = $02;
    L2CAP_CONN_RSP_1 = $03;
    L2CAP_CONF_REQ_1 = $04;
    L2CAP_CONF_RSP_1 = $05;
    L2CAP_DISCONN_REQ_1 = $06;
    L2CAP_DISCONN_RSP_1 = $07;
    L2CAP_ECHO_REQ = $08;
    L2CAP_ECHO_RSP = $09;
    L2CAP_INFO_REQ_1 = $0a;
    L2CAP_INFO_RSP_1 = $0b;
  
  type
    l2cap_hdr = record
      len: cuint16;
      cid: cuint16;
    end;
  
  const
    L2CAP_HDR_SIZE = 4;
  
  type
    l2cap_cmd_hdr = record
      code: cuint8;
      ident: cuint8;
      len: cuint16;
    end;
  
  const
    L2CAP_CMD_HDR_SIZE = 4;
  
  type
    l2cap_cmd_rej = record
      reason: cuint16;
    end;
  
  const
    L2CAP_CMD_REJ_SIZE = 2;
  
  type
    l2cap_conn_req = record
      psm: cuint16;
      scid: cuint16;
    end;
  
  const
    L2CAP_CONN_REQ_SIZE = 4;
  
  type
    l2cap_conn_rsp = record
      dcid: cuint16;
      scid: cuint16;
      result: cuint16;
      status: cuint16;
    end;
  
  const
    L2CAP_CONN_RSP_SIZE = 8;
    L2CAP_CR_SUCCESS = $0000;
    L2CAP_CR_PEND = $0001;
    L2CAP_CR_BAD_PSM = $0002;
    L2CAP_CR_SEC_BLOCK = $0003;
    L2CAP_CR_NO_MEM = $0004;
    L2CAP_CS_NO_INFO = $0000;
    L2CAP_CS_AUTHEN_PEND = $0001;
    L2CAP_CS_AUTHOR_PEND = $0002;
  
  type
    l2cap_conf_req = record
      dcid: cuint16;
      flags: cuint16;
      data: TArray0to0Ofcuint8;
    end;
  
  const
    L2CAP_CONF_REQ_SIZE = 4;
  
  type
    l2cap_conf_rsp = record
      scid: cuint16;
      flags: cuint16;
      result: cuint16;
      data: TArray0to0Ofcuint8;
    end;
  
  const
    L2CAP_CONF_RSP_SIZE = 6;
    L2CAP_CONF_SUCCESS = $0000;
    L2CAP_CONF_UNACCEPT = $0001;
    L2CAP_CONF_REJECT = $0002;
    L2CAP_CONF_UNKNOWN = $0003;
  
  type
    l2cap_conf_opt = record
      type_: cuint8;
      len: cuint8;
      val: TArray0to0Ofcuint8;
    end;
  
  const
    L2CAP_CONF_OPT_SIZE = 2;
    L2CAP_CONF_MTU = $01;
    L2CAP_CONF_FLUSH_TO = $02;
    L2CAP_CONF_QOS = $03;
    L2CAP_CONF_RFC = $04;
    L2CAP_CONF_RFC_MODE = $04;
    L2CAP_CONF_MAX_SIZE = 22;
    L2CAP_MODE_BASIC = $00;
    L2CAP_MODE_RETRANS = $01;
    L2CAP_MODE_FLOWCTL = $02;
  
  type
    l2cap_disconn_req = record
      dcid: cuint16;
      scid: cuint16;
    end;
  
  const
    L2CAP_DISCONN_REQ_SIZE = 4;
  
  type
    l2cap_disconn_rsp = record
      dcid: cuint16;
      scid: cuint16;
    end;
  
  const
    L2CAP_DISCONN_RSP_SIZE = 4;
  
  type
    l2cap_info_req = record
      type_: cuint16;
    end;
  
  const
    L2CAP_INFO_REQ_SIZE = 2;
  
  type
    l2cap_info_rsp = record
      type_: cuint16;
      result: cuint16;
      data: TArray0to0Ofcuint8;
    end;
  
  const
    L2CAP_INFO_RSP_SIZE = 4;
    L2CAP_IT_CL_MTU = $0001;
    L2CAP_IT_FEAT_MASK = $0002;
    L2CAP_IR_SUCCESS = $0000;
    L2CAP_IR_NOTSUPP = $0001;
  
{$EndIf}
{$IfNDef __RFCOMM_H}
  
  const
    RFCOMM_DEFAULT_MTU = 127;
    RFCOMM_PSM = 3;
  
  {off $Define RFCOMM_CONN_TIMEOUT:=(HZ * 30)}
  {off $Define RFCOMM_DISC_TIMEOUT:=(HZ * 20)}
  
  type
    sockaddr_rc = record
      rc_family: sa_family_t;
      rc_bdaddr: bdaddr_t;
      rc_channel: cuint8;
    end;
  
  const
    RFCOMM_CONNINFO_1 = $02;
  
  type
    rfcomm_conninfo = record
      hci_handle: cuint16;
      dev_class: TArray0to31Ofcuint8;
    end;
  
  const
    RFCOMM_LM = $03;
    RFCOMM_LM_MASTER = $0001;
    RFCOMM_LM_AUTH = $0002;
    RFCOMM_LM_ENCRYPT = $0004;
    RFCOMM_LM_TRUSTED = $0008;
    RFCOMM_LM_RELIABLE = $0010;
    RFCOMM_LM_SECURE = $0020;
    RFCOMM_MAX_DEV = 256;
  
  {off $Define RFCOMMCREATEDEV:=_IOW('R', 200, int)}
  {off $Define RFCOMMRELEASEDEV:=_IOW('R', 201, int)}
  {off $Define RFCOMMGETDEVLIST:=_IOR('R', 210, int)}
  {off $Define RFCOMMGETDEVINFO:=_IOR('R', 211, int)}
  
  type
    rfcomm_dev_req = record
      dev_id: cint16;
      flags: cuint32;
      src: bdaddr_t;
      dst: bdaddr_t;
      channel: cuint8;
    end;
  
  const
    RFCOMM_REUSE_DLC = 0;
    RFCOMM_RELEASE_ONHUP = 1;
    RFCOMM_HANGUP_NOW = 2;
    RFCOMM_TTY_ATTACHED = 3;
  
  type
    rfcomm_dev_info = record
      id: cint16;
      flags: cuint32;
      state: cuint16;
      src: bdaddr_t;
      dst: bdaddr_t;
      channel: cuint8;
    end;
    TArray0to0Ofrfcomm_dev_info = array[0..0] of rfcomm_dev_info;
    rfcomm_dev_list_req = record
      dev_num: cuint16;
      dev_info: TArray0to0Ofrfcomm_dev_info;
    end;
  
{$EndIf}
{$IfNDef __SCO_H}
  
  const
    SCO_DEFAULT_MTU = 500;
    SCO_DEFAULT_FLUSH_TO = $FFFF;
  
  {off $Define SCO_CONN_TIMEOUT:=(HZ * 40)}
  {off $Define SCO_DISCONN_TIMEOUT:=(HZ * 2)}
  {off $Define SCO_CONN_IDLE_TIMEOUT:=(HZ * 60)}
  
  type
    sockaddr_sco = record
      sco_family: sa_family_t;
      sco_bdaddr: bdaddr_t;
    end;
  
  const
    SCO_OPTIONS_1 = $01;
  
  type
    sco_options = record
      mtu: cuint16;
    end;
  
  const
    SCO_CONNINFO_1 = $02;
  
  type
    sco_conninfo = record
      hci_handle: cuint16;
      dev_class: TArray0to31Ofcuint8;
    end;
  
{$EndIf}
{$IfNDef __SDP_H}
  
  const
    SDP_UNIX_PATH = '/var/run/sdp';
    SDP_RESPONSE_TIMEOUT = 20;
    SDP_REQ_BUFFER_SIZE = 2048;
    SDP_RSP_BUFFER_SIZE = 65535;
    SDP_PDU_CHUNK_SIZE = 1024;
    SDP_PSM = $0001;
    SDP_UUID = $0001;
    UDP_UUID = $0002;
    RFCOMM_UUID = $0003;
    TCP_UUID = $0004;
    TCS_BIN_UUID = $0005;
    TCS_AT_UUID = $0006;
    OBEX_UUID = $0008;
    IP_UUID = $0009;
    FTP_UUID = $000a;
    HTTP_UUID = $000c;
    WSP_UUID = $000e;
    BNEP_UUID = $000f;
    UPNP_UUID = $0010;
    HIDP_UUID = $0011;
    HCRP_CTRL_UUID = $0012;
    HCRP_DATA_UUID = $0014;
    HCRP_NOTE_UUID = $0016;
    AVCTP_UUID = $0017;
    AVDTP_UUID = $0019;
    CMTP_UUID = $001b;
    UDI_UUID = $001d;
    L2CAP_UUID = $0100;
    SDP_SERVER_SVCLASS_ID = $1000;
    BROWSE_GRP_DESC_SVCLASS_ID = $1001;
    PUBLIC_BROWSE_GROUP = $1002;
    SERIAL_PORT_SVCLASS_ID = $1101;
    LAN_ACCESS_SVCLASS_ID = $1102;
    DIALUP_NET_SVCLASS_ID = $1103;
    IRMC_SYNC_SVCLASS_ID = $1104;
    OBEX_OBJPUSH_SVCLASS_ID = $1105;
    OBEX_FILETRANS_SVCLASS_ID = $1106;
    IRMC_SYNC_CMD_SVCLASS_ID = $1107;
    HEADSET_SVCLASS_ID = $1108;
    CORDLESS_TELEPHONY_SVCLASS_ID = $1109;
    AUDIO_SOURCE_SVCLASS_ID = $110a;
    AUDIO_SINK_SVCLASS_ID = $110b;
    AV_REMOTE_TARGET_SVCLASS_ID = $110c;
    ADVANCED_AUDIO_SVCLASS_ID = $110d;
    AV_REMOTE_SVCLASS_ID = $110e;
    VIDEO_CONF_SVCLASS_ID = $110f;
    INTERCOM_SVCLASS_ID = $1110;
    FAX_SVCLASS_ID = $1111;
    HEADSET_AGW_SVCLASS_ID = $1112;
    WAP_SVCLASS_ID = $1113;
    WAP_CLIENT_SVCLASS_ID = $1114;
    PANU_SVCLASS_ID = $1115;
    NAP_SVCLASS_ID = $1116;
    GN_SVCLASS_ID = $1117;
    DIRECT_PRINTING_SVCLASS_ID = $1118;
    REFERENCE_PRINTING_SVCLASS_ID = $1119;
    IMAGING_SVCLASS_ID = $111a;
    IMAGING_RESPONDER_SVCLASS_ID = $111b;
    IMAGING_ARCHIVE_SVCLASS_ID = $111c;
    IMAGING_REFOBJS_SVCLASS_ID = $111d;
    HANDSFREE_SVCLASS_ID = $111e;
    HANDSFREE_AGW_SVCLASS_ID = $111f;
    DIRECT_PRT_REFOBJS_SVCLASS_ID = $1120;
    REFLECTED_UI_SVCLASS_ID = $1121;
    BASIC_PRINTING_SVCLASS_ID = $1122;
    PRINTING_STATUS_SVCLASS_ID = $1123;
    HID_SVCLASS_ID = $1124;
    HCR_SVCLASS_ID = $1125;
    HCR_PRINT_SVCLASS_ID = $1126;
    HCR_SCAN_SVCLASS_ID = $1127;
    CIP_SVCLASS_ID = $1128;
    VIDEO_CONF_GW_SVCLASS_ID = $1129;
    UDI_MT_SVCLASS_ID = $112a;
    UDI_TA_SVCLASS_ID = $112b;
    AV_SVCLASS_ID = $112c;
    SAP_SVCLASS_ID = $112d;
    PBAP_PCE_SVCLASS_ID = $112e;
    PBAP_PSE_SVCLASS_ID = $112f;
    PBAP_SVCLASS_ID = $1130;
    PNP_INFO_SVCLASS_ID = $1200;
    GENERIC_NETWORKING_SVCLASS_ID = $1201;
    GENERIC_FILETRANS_SVCLASS_ID = $1202;
    GENERIC_AUDIO_SVCLASS_ID = $1203;
    GENERIC_TELEPHONY_SVCLASS_ID = $1204;
    UPNP_SVCLASS_ID = $1205;
    UPNP_IP_SVCLASS_ID = $1206;
    UPNP_PAN_SVCLASS_ID = $1300;
    UPNP_LAP_SVCLASS_ID = $1301;
    UPNP_L2CAP_SVCLASS_ID = $1302;
    VIDEO_SOURCE_SVCLASS_ID = $1303;
    VIDEO_SINK_SVCLASS_ID = $1304;
    VIDEO_DISTRIBUTION_SVCLASS_ID = $1305;
    APPLE_AGENT_SVCLASS_ID = $2112;
    SDP_SERVER_PROFILE_ID = SDP_SERVER_SVCLASS_ID;
    BROWSE_GRP_DESC_PROFILE_ID = BROWSE_GRP_DESC_SVCLASS_ID;
    SERIAL_PORT_PROFILE_ID = SERIAL_PORT_SVCLASS_ID;
    LAN_ACCESS_PROFILE_ID = LAN_ACCESS_SVCLASS_ID;
    DIALUP_NET_PROFILE_ID = DIALUP_NET_SVCLASS_ID;
    IRMC_SYNC_PROFILE_ID = IRMC_SYNC_SVCLASS_ID;
    OBEX_OBJPUSH_PROFILE_ID = OBEX_OBJPUSH_SVCLASS_ID;
    OBEX_FILETRANS_PROFILE_ID = OBEX_FILETRANS_SVCLASS_ID;
    IRMC_SYNC_CMD_PROFILE_ID = IRMC_SYNC_CMD_SVCLASS_ID;
    HEADSET_PROFILE_ID = HEADSET_SVCLASS_ID;
    CORDLESS_TELEPHONY_PROFILE_ID = CORDLESS_TELEPHONY_SVCLASS_ID;
    AUDIO_SOURCE_PROFILE_ID = AUDIO_SOURCE_SVCLASS_ID;
    AUDIO_SINK_PROFILE_ID = AUDIO_SINK_SVCLASS_ID;
    AV_REMOTE_TARGET_PROFILE_ID = AV_REMOTE_TARGET_SVCLASS_ID;
    ADVANCED_AUDIO_PROFILE_ID = ADVANCED_AUDIO_SVCLASS_ID;
    AV_REMOTE_PROFILE_ID = AV_REMOTE_SVCLASS_ID;
    VIDEO_CONF_PROFILE_ID = VIDEO_CONF_SVCLASS_ID;
    INTERCOM_PROFILE_ID = INTERCOM_SVCLASS_ID;
    FAX_PROFILE_ID = FAX_SVCLASS_ID;
    HEADSET_AGW_PROFILE_ID = HEADSET_AGW_SVCLASS_ID;
    WAP_PROFILE_ID = WAP_SVCLASS_ID;
    WAP_CLIENT_PROFILE_ID = WAP_CLIENT_SVCLASS_ID;
    PANU_PROFILE_ID = PANU_SVCLASS_ID;
    NAP_PROFILE_ID = NAP_SVCLASS_ID;
    GN_PROFILE_ID = GN_SVCLASS_ID;
    DIRECT_PRINTING_PROFILE_ID = DIRECT_PRINTING_SVCLASS_ID;
    REFERENCE_PRINTING_PROFILE_ID = REFERENCE_PRINTING_SVCLASS_ID;
    IMAGING_PROFILE_ID = IMAGING_SVCLASS_ID;
    IMAGING_RESPONDER_PROFILE_ID = IMAGING_RESPONDER_SVCLASS_ID;
    IMAGING_ARCHIVE_PROFILE_ID = IMAGING_ARCHIVE_SVCLASS_ID;
    IMAGING_REFOBJS_PROFILE_ID = IMAGING_REFOBJS_SVCLASS_ID;
    HANDSFREE_PROFILE_ID = HANDSFREE_SVCLASS_ID;
    HANDSFREE_AGW_PROFILE_ID = HANDSFREE_AGW_SVCLASS_ID;
    DIRECT_PRT_REFOBJS_PROFILE_ID = DIRECT_PRT_REFOBJS_SVCLASS_ID;
    REFLECTED_UI_PROFILE_ID = REFLECTED_UI_SVCLASS_ID;
    BASIC_PRINTING_PROFILE_ID = BASIC_PRINTING_SVCLASS_ID;
    PRINTING_STATUS_PROFILE_ID = PRINTING_STATUS_SVCLASS_ID;
    HID_PROFILE_ID = HID_SVCLASS_ID;
    HCR_PROFILE_ID = HCR_SCAN_SVCLASS_ID;
    HCR_PRINT_PROFILE_ID = HCR_PRINT_SVCLASS_ID;
    HCR_SCAN_PROFILE_ID = HCR_SCAN_SVCLASS_ID;
    CIP_PROFILE_ID = CIP_SVCLASS_ID;
    VIDEO_CONF_GW_PROFILE_ID = VIDEO_CONF_GW_SVCLASS_ID;
    UDI_MT_PROFILE_ID = UDI_MT_SVCLASS_ID;
    UDI_TA_PROFILE_ID = UDI_TA_SVCLASS_ID;
    AV_PROFILE_ID = AV_SVCLASS_ID;
    SAP_PROFILE_ID = SAP_SVCLASS_ID;
    PBAP_PCE_PROFILE_ID = PBAP_PCE_SVCLASS_ID;
    PBAP_PSE_PROFILE_ID = PBAP_PSE_SVCLASS_ID;
    PBAP_PROFILE_ID = PBAP_SVCLASS_ID;
    PNP_INFO_PROFILE_ID = PNP_INFO_SVCLASS_ID;
    GENERIC_NETWORKING_PROFILE_ID = GENERIC_NETWORKING_SVCLASS_ID;
    GENERIC_FILETRANS_PROFILE_ID = GENERIC_FILETRANS_SVCLASS_ID;
    GENERIC_AUDIO_PROFILE_ID = GENERIC_AUDIO_SVCLASS_ID;
    GENERIC_TELEPHONY_PROFILE_ID = GENERIC_TELEPHONY_SVCLASS_ID;
    UPNP_PROFILE_ID = UPNP_SVCLASS_ID;
    UPNP_IP_PROFILE_ID = UPNP_IP_SVCLASS_ID;
    UPNP_PAN_PROFILE_ID = UPNP_PAN_SVCLASS_ID;
    UPNP_LAP_PROFILE_ID = UPNP_LAP_SVCLASS_ID;
    UPNP_L2CAP_PROFILE_ID = UPNP_L2CAP_SVCLASS_ID;
    VIDEO_SOURCE_PROFILE_ID = VIDEO_SOURCE_SVCLASS_ID;
    VIDEO_SINK_PROFILE_ID = VIDEO_SINK_SVCLASS_ID;
    VIDEO_DISTRIBUTION_PROFILE_ID = VIDEO_DISTRIBUTION_SVCLASS_ID;
    APPLE_AGENT_PROFILE_ID = APPLE_AGENT_SVCLASS_ID;
    SDP_SERVER_RECORD_HANDLE = $0000;
    SDP_ATTR_RECORD_HANDLE = $0000;
    SDP_ATTR_SVCLASS_ID_LIST = $0001;
    SDP_ATTR_RECORD_STATE = $0002;
    SDP_ATTR_SERVICE_ID = $0003;
    SDP_ATTR_PROTO_DESC_LIST = $0004;
    SDP_ATTR_BROWSE_GRP_LIST = $0005;
    SDP_ATTR_LANG_BASE_ATTR_ID_LIST = $0006;
    SDP_ATTR_SVCINFO_TTL = $0007;
    SDP_ATTR_SERVICE_AVAILABILITY = $0008;
    SDP_ATTR_PFILE_DESC_LIST = $0009;
    SDP_ATTR_DOC_URL = $000a;
    SDP_ATTR_CLNT_EXEC_URL = $000b;
    SDP_ATTR_ICON_URL = $000c;
    SDP_ATTR_ADD_PROTO_DESC_LIST = $000d;
    SDP_ATTR_GROUP_ID = $0200;
    SDP_ATTR_IP_SUBNET = $0200;
    SDP_ATTR_VERSION_NUM_LIST = $0200;
    SDP_ATTR_SVCDB_STATE = $0201;
    SDP_ATTR_SERVICE_VERSION = $0300;
    SDP_ATTR_EXTERNAL_NETWORK = $0301;
    SDP_ATTR_SUPPORTED_DATA_STORES_LIST = $0301;
    SDP_ATTR_FAX_CLASS1_SUPPORT = $0302;
    SDP_ATTR_REMOTE_AUDIO_VOLUME_CONTROL = $0302;
    SDP_ATTR_FAX_CLASS20_SUPPORT = $0303;
    SDP_ATTR_SUPPORTED_FORMATS_LIST = $0303;
    SDP_ATTR_FAX_CLASS2_SUPPORT = $0304;
    SDP_ATTR_AUDIO_FEEDBACK_SUPPORT = $0305;
    SDP_ATTR_NETWORK_ADDRESS = $0306;
    SDP_ATTR_WAP_GATEWAY = $0307;
    SDP_ATTR_HOMEPAGE_URL = $0308;
    SDP_ATTR_WAP_STACK_TYPE = $0309;
    SDP_ATTR_SECURITY_DESC = $030a;
    SDP_ATTR_NET_ACCESS_TYPE = $030b;
    SDP_ATTR_MAX_NET_ACCESSRATE = $030c;
    SDP_ATTR_IP4_SUBNET = $030d;
    SDP_ATTR_IP6_SUBNET = $030e;
    SDP_ATTR_SUPPORTED_CAPABILITIES = $0310;
    SDP_ATTR_SUPPORTED_FEATURES = $0311;
    SDP_ATTR_SUPPORTED_FUNCTIONS = $0312;
    SDP_ATTR_TOTAL_IMAGING_DATA_CAPACITY = $0313;
    SDP_ATTR_SUPPORTED_REPOSITORIES = $0314;
    SDP_ATTR_SPECIFICATION_ID = $0200;
    SDP_ATTR_VENDOR_ID = $0201;
    SDP_ATTR_PRODUCT_ID = $0202;
    SDP_ATTR_VERSION = $0203;
    SDP_ATTR_PRIMARY_RECORD = $0204;
    SDP_ATTR_VENDOR_ID_SOURCE = $0205;
    SDP_ATTR_HID_DEVICE_RELEASE_NUMBER = $0200;
    SDP_ATTR_HID_PARSER_VERSION = $0201;
    SDP_ATTR_HID_DEVICE_SUBCLASS = $0202;
    SDP_ATTR_HID_COUNTRY_CODE = $0203;
    SDP_ATTR_HID_VIRTUAL_CABLE = $0204;
    SDP_ATTR_HID_RECONNECT_INITIATE = $0205;
    SDP_ATTR_HID_DESCRIPTOR_LIST = $0206;
    SDP_ATTR_HID_LANG_ID_BASE_LIST = $0207;
    SDP_ATTR_HID_SDP_DISABLE = $0208;
    SDP_ATTR_HID_BATTERY_POWER = $0209;
    SDP_ATTR_HID_REMOTE_WAKEUP = $020a;
    SDP_ATTR_HID_PROFILE_VERSION = $020b;
    SDP_ATTR_HID_SUPERVISION_TIMEOUT = $020c;
    SDP_ATTR_HID_NORMALLY_CONNECTABLE = $020d;
    SDP_ATTR_HID_BOOT_DEVICE = $020e;
    SDP_PRIMARY_LANG_BASE = $0100;
    SDP_ATTR_SVCNAME_PRIMARY = $0000 + SDP_PRIMARY_LANG_BASE;
    SDP_ATTR_SVCDESC_PRIMARY = $0001 + SDP_PRIMARY_LANG_BASE;
    SDP_ATTR_PROVNAME_PRIMARY = $0002 + SDP_PRIMARY_LANG_BASE;
    SDP_DATA_NIL = $00;
    SDP_UINT8 = $08;
    SDP_UINT16 = $09;
    SDP_UINT32 = $0A;
    SDP_UINT64 = $0B;
    SDP_UINT128 = $0C;
    SDP_INT8 = $10;
    SDP_INT16 = $11;
    SDP_INT32 = $12;
    SDP_INT64 = $13;
    SDP_INT128 = $14;
    SDP_UUID_UNSPEC = $18;
    SDP_UUID16 = $19;
    SDP_UUID32 = $1A;
    SDP_UUID128 = $1C;
    SDP_TEXT_STR_UNSPEC = $20;
    SDP_TEXT_STR8 = $25;
    SDP_TEXT_STR16 = $26;
    SDP_TEXT_STR32 = $27;
    SDP_BOOL = $28;
    SDP_SEQ_UNSPEC = $30;
    SDP_SEQ8 = $35;
    SDP_SEQ16 = $36;
    SDP_SEQ32 = $37;
    SDP_ALT_UNSPEC = $38;
    SDP_ALT8 = $3D;
    SDP_ALT16 = $3E;
    SDP_ALT32 = $3F;
    SDP_URL_STR_UNSPEC = $40;
    SDP_URL_STR8 = $45;
    SDP_URL_STR16 = $46;
    SDP_URL_STR32 = $47;
    SDP_ERROR_RSP = $01;
    SDP_SVC_SEARCH_REQ = $02;
    SDP_SVC_SEARCH_RSP = $03;
    SDP_SVC_ATTR_REQ = $04;
    SDP_SVC_ATTR_RSP = $05;
    SDP_SVC_SEARCH_ATTR_REQ = $06;
    SDP_SVC_SEARCH_ATTR_RSP = $07;
    SDP_SVC_REGISTER_REQ = $75;
    SDP_SVC_REGISTER_RSP = $76;
    SDP_SVC_UPDATE_REQ = $77;
    SDP_SVC_UPDATE_RSP = $78;
    SDP_SVC_REMOVE_REQ = $79;
    SDP_SVC_REMOVE_RSP = $80;
    SDP_INVALID_VERSION = $0001;
    SDP_INVALID_RECORD_HANDLE = $0002;
    SDP_INVALID_SYNTAX = $0003;
    SDP_INVALID_PDU_SIZE = $0004;
    SDP_INVALID_CSTATE = $0005;
  
  type
    sdp_pdu_hdr_t = record
      pdu_id: cuint8;
      tid: cuint16;
      plen: cuint16;
    end;
    uint128_t = record
      data: TArray0to161Ofcuint8;
    end;
    uuid_t = record
      type_: cuint8;
      value: record
        case longint of
          0: (uuid16: cuint16 );
          1: (uuid32: cuint32 );
          2: (uuid128: uint128_t );
        end;
    end;
    Psdp_list_t = ^sdp_list_t;
    _sdp_list = record
      next: Psdp_list_t;
      data: pointer;
    end;
    sdp_list_t = _sdp_list;
    sdp_lang_attr_t = record
      code_ISO639: cuint16;
      encoding: cuint16;
      base_offset: cuint16;
    end;
    sdp_profile_desc_t = record
      uuid: uuid_t;
      version: cuint16;
    end;
    sdp_version_t = record
      major: cuint8;
      minor: cuint8;
    end;
    sdp_buf_t = record
      data: pcuint8;
      data_size: cuint32;
      buf_size: cuint32;
    end;
    sdp_record_t = record
      handle: cuint32;
      pattern: Psdp_list_t;
      attrlist: Psdp_list_t;
      svclass: uuid_t;
    end;
    Psdp_data_t = ^sdp_data_t;
    sdp_data_struct = record
      dtd: cuint8;
      attrId: cuint16;
      val: record
        case longint of
          0: (int8: cint8 );
          1: (int16: cint16 );
          2: (int32: cint32 );
          3: (int64: cint64 );
          4: (int128: uint128_t );
          5: (uint8: cuint8 );
          6: (uint16: cuint16 );
          7: (uint32: cuint32 );
          8: (uint64: cuint64 );
          9: (uint128: uint128_t );
          10: (uuid: uuid_t );
          11: (str: pcchar );
          12: (dataseq: Psdp_data_t );
        end;
      next: Psdp_data_t;
      unitSize: cint;
    end;
    sdp_data_t = sdp_data_struct;
  
{$EndIf}
{$IfNDef __SDP_LIB_H}
  
  type
    sdp_list_func_t = procedure(param1pointer: pointer; param2pointer: pointer); cdecl;
    sdp_free_func_t = procedure(param1pointer: pointer); cdecl;
    sdp_comp_func_t = function(param1pointer: pointer; param2pointer: pointer): cint; cdecl;
  
  function sdp_list_append(list: Psdp_list_t; d: pointer): Psdp_list_t; cdecl; external;
  function sdp_list_remove(list: Psdp_list_t; d: pointer): Psdp_list_t; cdecl; external;
  function sdp_list_insert_sorted(list: Psdp_list_t; data: pointer; f: sdp_comp_func_t): Psdp_list_t; cdecl; external;
  procedure sdp_list_free(list: Psdp_list_t; f: sdp_free_func_t); cdecl; external;
  
  const
    SDP_RECORD_PERSIST = $01;
    SDP_DEVICE_RECORD = $02;
    SDP_RETRY_IF_BUSY = $01;
    SDP_WAIT_ON_CLOSE = $02;
    SDP_NON_BLOCKING = $04;
  
  type
    sdp_session_t = record
      sock: cint;
      state: cint;
      local: cint;
      flags: cint;
      tid: cuint16;
      priv: pointer;
    end;
    sdp_attrreq_type_t = (
      SDP_ATTR_REQ_INDIVIDUAL = 1,
      SDP_ATTR_REQ_RANGE
    );
    Psdp_session_t = ^sdp_session_t;
  
  function sdp_connect(src: Pbdaddr_t; dst: Pbdaddr_t; flags: cuint32): Psdp_session_t; cdecl; external;
  function sdp_close(session: Psdp_session_t): cint; cdecl; external;
  function sdp_get_socket(session: Psdp_session_t): cint; cdecl; external;
  function sdp_create(sk: cint; flags: cuint32): Psdp_session_t; cdecl; external;
  function sdp_get_error(session: Psdp_session_t): cint; cdecl; external;
  function sdp_process(session: Psdp_session_t): cint; cdecl; external;
  function sdp_service_search_async(session: Psdp_session_t; search: Psdp_list_t; max_rec_num: cuint16): cint; cdecl; external;
  function sdp_service_attr_async(session: Psdp_session_t; handle: cuint32; reqtype: sdp_attrreq_type_t; attrid_list: Psdp_list_t): cint; cdecl; external;
  function sdp_service_search_attr_async(session: Psdp_session_t; search: Psdp_list_t; reqtype: sdp_attrreq_type_t; attrid_list: Psdp_list_t): cint; cdecl; external;
  function sdp_gen_tid(session: Psdp_session_t): cuint16; cdecl; external;
  function sdp_general_inquiry(ii: Pinquiry_info; dev_num: cint; duration: cint; found: pcuint8): cint; cdecl; external;
  
  type
    Psdp_record_t = ^sdp_record_t;
  
  function sdp_get_int_attr(rec: Psdp_record_t; attr: cuint16; value: pcint): cint; cdecl; external;
  function sdp_get_string_attr(rec: Psdp_record_t; attr: cuint16; value: pcchar; valuelen: cint): cint; cdecl; external;
  function sdp_data_alloc(dtd: cuint8; value: pointer): Psdp_data_t; cdecl; external;
  function sdp_data_alloc_with_length(dtd: cuint8; value: pointer; length: cuint32): Psdp_data_t; cdecl; external;
  procedure sdp_data_free(data: Psdp_data_t); cdecl; external;
  function sdp_data_get(rec: Psdp_record_t; attr_id: cuint16): Psdp_data_t; cdecl; external;
  
  type
    Ppointer = ^pointer;
  
  function sdp_seq_alloc(dtds: Ppointer; values: Ppointer; len: cint): Psdp_data_t; cdecl; external;
  function sdp_seq_alloc_with_length(dtds: Ppointer; values: Ppointer; length: pcint; len: cint): Psdp_data_t; cdecl; external;
  function sdp_seq_append(seq: Psdp_data_t; data: Psdp_data_t): Psdp_data_t; cdecl; external;
  function sdp_attr_add(rec: Psdp_record_t; attr: cuint16; data: Psdp_data_t): cint; cdecl; external;
  procedure sdp_attr_remove(rec: Psdp_record_t; attr: cuint16); cdecl; external;
  procedure sdp_attr_replace(rec: Psdp_record_t; attr: cuint16; data: Psdp_data_t); cdecl; external;
  function sdp_set_uuidseq_attr(rec: Psdp_record_t; attr: cuint16; seq: Psdp_list_t): cint; cdecl; external;
  
  type
    PPsdp_list_t = ^Psdp_list_t;
  
  function sdp_get_uuidseq_attr(rec: Psdp_record_t; attr: cuint16; seqp: PPsdp_list_t): cint; cdecl; external;
  function sdp_attr_add_new(rec: Psdp_record_t; attr: cuint16; dtd: cuint8; p: pointer): cint; cdecl; external;
  procedure sdp_set_info_attr(rec: Psdp_record_t; name: pcchar; prov: pcchar; desc: pcchar); cdecl; external;
  function sdp_set_access_protos(rec: Psdp_record_t; proto: Psdp_list_t): cint; cdecl; external;
  function sdp_set_add_access_protos(rec: Psdp_record_t; proto: Psdp_list_t): cint; cdecl; external;
  function sdp_get_proto_port(list: Psdp_list_t; proto: cint): cint; cdecl; external;
  function sdp_get_proto_desc(list: Psdp_list_t; proto: cint): Psdp_data_t; cdecl; external;
  function sdp_set_lang_attr(rec: Psdp_record_t; list: Psdp_list_t): cint; cdecl; external;
  procedure sdp_set_service_id(rec: Psdp_record_t; uuid: uuid_t); cdecl; external;
  procedure sdp_set_group_id(rec: Psdp_record_t; grouuuid: uuid_t); cdecl; external;
  function sdp_set_profile_descs(rec: Psdp_record_t; desc: Psdp_list_t): cint; cdecl; external;
  procedure sdp_set_url_attr(rec: Psdp_record_t; clientExecURL: pcchar; docURL: pcchar; iconURL: pcchar); cdecl; external;
  function sdp_service_search_req(session: Psdp_session_t; search: Psdp_list_t; max_rec_num: cuint16; rsp_list: PPsdp_list_t): cint; cdecl; external;
  function sdp_service_attr_req(session: Psdp_session_t; handle: cuint32; reqtype: sdp_attrreq_type_t; attrid_list: Psdp_list_t): Psdp_record_t; cdecl; external;
  function sdp_service_search_attr_req(session: Psdp_session_t; search: Psdp_list_t; reqtype: sdp_attrreq_type_t; attrid_list: Psdp_list_t; rsp_list: PPsdp_list_t): cint; cdecl; external;
  function sdp_record_alloc: Psdp_record_t; cdecl; external;
  procedure sdp_record_free(rec: Psdp_record_t); cdecl; external;
  function sdp_device_record_register_binary(session: Psdp_session_t; device: Pbdaddr_t; data: pcuint8; size: cuint32; flags: cuint8; handle: pcuint32): cint; cdecl; external;
  function sdp_device_record_register(session: Psdp_session_t; device: Pbdaddr_t; rec: Psdp_record_t; flags: cuint8): cint; cdecl; external;
  function sdp_record_register(session: Psdp_session_t; rec: Psdp_record_t; flags: cuint8): cint; cdecl; external;
  function sdp_device_record_unregister_binary(session: Psdp_session_t; device: Pbdaddr_t; handle: cuint32): cint; cdecl; external;
  function sdp_device_record_unregister(session: Psdp_session_t; device: Pbdaddr_t; rec: Psdp_record_t): cint; cdecl; external;
  function sdp_record_unregister(session: Psdp_session_t; rec: Psdp_record_t): cint; cdecl; external;
  function sdp_device_record_update_binary(session: Psdp_session_t; device: Pbdaddr_t; handle: cuint32; data: pcuint8; size: cuint32): cint; cdecl; external;
  function sdp_device_record_update(session: Psdp_session_t; device: Pbdaddr_t; rec: Psdp_record_t): cint; cdecl; external;
  function sdp_record_update(sess: Psdp_session_t; rec: Psdp_record_t): cint; cdecl; external;
  procedure sdp_record_print(rec: Psdp_record_t); cdecl; external;
  
  type
    Puuid_t = ^uuid_t;
  
  function sdp_uuid16_create(uuid: Puuid_t; data: cuint16): Puuid_t; cdecl; external;
  function sdp_uuid32_create(uuid: Puuid_t; data: cuint32): Puuid_t; cdecl; external;
  function sdp_uuid128_create(uuid: Puuid_t; data: pointer): Puuid_t; cdecl; external;
  function sdp_uuid16_cmp(p1: pointer; p2: pointer): cint; cdecl; external;
  function sdp_uuid128_cmp(p1: pointer; p2: pointer): cint; cdecl; external;
  function sdp_uuid_to_uuid128(uuid: Puuid_t): Puuid_t; cdecl; external;
  procedure sdp_uuid16_to_uuid128(uuid128: Puuid_t; uuid16: Puuid_t); cdecl; external;
  procedure sdp_uuid32_to_uuid128(uuid128: Puuid_t; uuid32: Puuid_t); cdecl; external;
  function sdp_uuid128_to_uuid(uuid: Puuid_t): cint; cdecl; external;
  function sdp_uuid_to_proto(uuid: Puuid_t): cint; cdecl; external;
  function sdp_uuid_extract(buffer: pcuint8; uuid: Puuid_t; scanned: pcint): cint; cdecl; external;
  procedure sdp_uuid_print(uuid: Puuid_t); cdecl; external;
  
  const
    MAX_LEN_UUID_STR = 37;
    MAX_LEN_PROTOCOL_UUID_STR = 8;
    MAX_LEN_SERVICECLASS_UUID_STR = 28;
    MAX_LEN_PROFILEDESCRIPTOR_UUID_STR = 28;
  
  function sdp_uuid2strn(uuid: Puuid_t; str: pcchar; n: PtrUInt): cint; cdecl; external;
  function sdp_proto_uuid2strn(uuid: Puuid_t; str: pcchar; n: PtrUInt): cint; cdecl; external;
  function sdp_svclass_uuid2strn(uuid: Puuid_t; str: pcchar; n: PtrUInt): cint; cdecl; external;
  function sdp_profile_uuid2strn(uuid: Puuid_t; str: pcchar; n: PtrUInt): cint; cdecl; external;
  function sdp_get_access_protos(rec: Psdp_record_t; protos: PPsdp_list_t): cint; cdecl; external;
  function sdp_get_add_access_protos(rec: Psdp_record_t; protos: PPsdp_list_t): cint; cdecl; external;
  function sdp_get_lang_attr(rec: Psdp_record_t; langSeq: PPsdp_list_t): cint; cdecl; external;
  function sdp_get_profile_descs(rec: Psdp_record_t; profDesc: PPsdp_list_t): cint; cdecl; external;
  function sdp_get_server_ver(rec: Psdp_record_t; pVnumList: PPsdp_list_t): cint; cdecl; external;
  function sdp_get_service_id(rec: Psdp_record_t; uuid: Puuid_t): cint; cdecl; external;
  function sdp_get_group_id(rec: Psdp_record_t; uuid: Puuid_t): cint; cdecl; external;
  function sdp_get_record_state(rec: Psdp_record_t; svcRecState: pcuint32): cint; cdecl; external;
  function sdp_get_service_avail(rec: Psdp_record_t; svcAvail: pcuint8): cint; cdecl; external;
  function sdp_get_service_ttl(rec: Psdp_record_t; svcTTLInfo: pcuint32): cint; cdecl; external;
  function sdp_get_database_state(rec: Psdp_record_t; svcDBState: pcuint32): cint; cdecl; external;
  function sdp_extract_pdu(pdata: pcuint8; scanned: pcint): Psdp_record_t; cdecl; external;
  function sdp_extract_string(param1pcuint8: pcuint8; param2pcint: pcint): Psdp_data_t; cdecl; external;
  procedure sdp_data_print(data: Psdp_data_t); cdecl; external;
  procedure sdp_print_service_attr(alist: Psdp_list_t); cdecl; external;
  function sdp_attrid_comp_func(key1: pointer; key2: pointer): cint; cdecl; external;
  procedure sdp_set_seq_len(ptr: pcuint8; length: cuint32); cdecl; external;
  
  type
    Psdp_buf_t = ^sdp_buf_t;
  
  procedure sdp_set_attrid(pdu: Psdp_buf_t; id: cuint16); cdecl; external;
  procedure sdp_append_to_pdu(dst: Psdp_buf_t; d: Psdp_data_t); cdecl; external;
  procedure sdp_append_to_buf(dst: Psdp_buf_t; data: pcuint8; len: cuint32); cdecl; external;
  function sdp_gen_pdu(pdu: Psdp_buf_t; data: Psdp_data_t): cint; cdecl; external;
  function sdp_gen_record_pdu(rec: Psdp_record_t; pdu: Psdp_buf_t): cint; cdecl; external;
  function sdp_extract_seqtype(buf: pcuint8; dtdp: pcuint8; seqlen: pcint): cint; cdecl; external;
  function sdp_extract_attr(pdata: pcuint8; extractedLength: pcint; rec: Psdp_record_t): Psdp_data_t; cdecl; external;
  procedure sdp_pattern_add_uuid(rec: Psdp_record_t; uuid: Puuid_t); cdecl; external;
  procedure sdp_pattern_add_uuidseq(rec: Psdp_record_t; seq: Psdp_list_t); cdecl; external;
  function sdp_send_req_w4_rsp(session: Psdp_session_t; req: pcuint8; rsp: pcuint8; reqsize: cuint32; rspsize: pcuint32): cint; cdecl; external;
{$EndIf}
implementation

end.
