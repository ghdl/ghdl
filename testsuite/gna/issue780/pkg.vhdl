library ieee;
use ieee.std_logic_1164.all;

package pkg is
   type t_slv_array      is array (natural range <>) of std_logic_vector;
  type t_ipbus_tranaction_header is
  record
    protocol_version : natural range 0 to 2**4  - 1;
    transaction_id   : natural range 0 to 2**12 - 1;
    words            : natural range 0 to 2**8  - 1;
--    type_id          : t_ipbus_transaction_type_id;
--    info_code        : t_ipbus_transaction_info_code;
  end record;

  type t_ipbus_transaction is
  record
    header : t_ipbus_tranaction_header;
    bodyy  : t_slv_array;
  end record;

  signal trans : t_ipbus_transaction(bodyy(7 downto 0)(31 downto 0));
end pkg;
