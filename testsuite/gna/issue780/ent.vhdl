library ieee;
context ieee.ieee_std_context;

entity dut is
end entity dut;

architecture rtl of dut is

  type t_slv_array is array (natural range <>) of std_logic_vector;
  type t_ipbus_tranaction_header is
  record
    protocol_version : natural range 0 to 2**4  - 1;
    transaction_id   : natural range 0 to 2**12 - 1;
    words            : natural range 0 to 2**8  - 1;
  end record;

  type t_ipbus_transaction is
  record
    header : t_ipbus_tranaction_header;
    bodyy  : t_slv_array;
  end record;

  signal transaction : t_ipbus_transaction(bodyy(7 downto 0)(31 downto 0));

begin

end architecture rtl;
