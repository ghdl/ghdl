library ieee;
use ieee.std_logic_1164.all;

library ipbus;


package ipbus_tvm_pkg is

  --===============================================================================================
  -- Types and constants for IPbus TVM
  --===============================================================================================
  constant C_SCOPE : string := "IPbus TVM";

  type t_ipbus_transaction_type_id is (
    READ,
    WRITE,
    NON_INC_READ,    -- Non-incrementing read
    NON_INC_WRITE,   -- Non-incrementing write
    RMW_BITS,        -- Read/modify/write bits
    RMW_SUM,         -- Read/modify/write sum
    CONF_SPACE_READ, -- Configuration space read
    CONF_SPACE_WRITE -- Configuration space write
  );

  type t_ipbus_transaction_info_code is (
    REQ_HANDLED_SUCCESSFULLY, -- Request handled successfully by target
    BAD_HEADER,
    RESERVED_0x2,
    RESERVED_0x3,
    BUS_ERROR_ON_READ,
    BUS_ERROR_ON_WRITE,
    BUS_TIMEOUT_ON_READ,
    BUS_TIMEOUT_ON_WRITE,
    RESERVED_0x8,
    RESERVED_0x9,
    RESERVED_0xA,
    RESERVED_0xB,
    RESERVED_0xC,
    RESERVED_0xD,
    RESERVED_0xE,
    OUTBOUND_REQUEST
  );

  type t_ipbus_tranaction_header is
  record
    protocol_version : natural;
    transaction_id   : natural;
    words            : natural;
    type_id          : t_ipbus_transaction_type_id;
    info_code        : t_ipbus_transaction_info_code;
  end record;

  type t_ipbus_transaction is
  record
    header : t_ipbus_tranaction_header;
    _body  : t_slv_array;
  end record;

end package ipbus_tvm_pkg;

--=================================================================================================
--=================================================================================================

package body ipbus_tvm_pkg is

end package body ipbus_tvm_pkg;
