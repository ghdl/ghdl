library ieee;
use ieee.std_logic_1164.all;

entity ghdl_bug is
  generic(
    type generic_data_type;
    GC_RESET_VALUE : generic_data_type
  );
  port (
    clk       : in  std_logic;
    reset     : in  std_logic;
    value_in  : in  generic_data_type;
    value_out : out generic_data_type
  );
end entity ghdl_bug;

architecture rtl of ghdl_bug is
begin

    p_ff : process(clk) is
    begin
      if reset = '1' then
        value_out <= GC_RESET_VALUE;
      elsif rising_edge(clk) then
        value_out <= value_in;
      end if;
    end process p_ff;

end architecture rtl;
