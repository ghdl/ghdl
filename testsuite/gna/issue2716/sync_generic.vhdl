library ieee;
use ieee.std_logic_1164.all;

entity sync_generic is
  generic(
    type generic_data_type;
    GC_RESET_VALUE          : generic_data_type;
    GC_NUMBER_OF_SYNC_FFS   : positive  := 2
  );
  port (
    clk       : in  std_logic;
    rst       : in  std_logic;
    value_in  : in  generic_data_type;
    value_out : out generic_data_type
  );
end entity sync_generic;

architecture rtl of sync_generic is

  type t_data_type_array is array(natural range<>) of generic_data_type;

  signal value_ff : t_data_type_array(GC_NUMBER_OF_SYNC_FFS-1 downto 0) := (others=>GC_RESET_VALUE);

begin

  p_sync : process(clk) is
  begin
    if rising_edge(clk) then
      if rst = '1' then
        value_ff <= (others=>GC_RESET_VALUE);
      else
        value_ff <= value_ff(GC_NUMBER_OF_SYNC_FFS-2 downto 0) & value_in;
      end if;
    end if;
  end process p_sync;

  value_out <= value_ff(GC_NUMBER_OF_SYNC_FFS-1);

end architecture rtl;
