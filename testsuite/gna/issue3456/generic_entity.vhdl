library ieee;
use ieee.std_logic_1164.all;

entity generic_entity is
  generic (
    type gt_generic_data_type;
    GC_RESET_VALUE : gt_generic_data_type
  );
  port (
    clk           : in  std_logic;
    reset         : in  std_logic;
    input_value   : in  gt_generic_data_type;
    output_value  : out gt_generic_data_type
  );
end entity generic_entity;

architecture sim of generic_entity is
begin

  p_output_value : process(clk) is
  begin
    if rising_edge(clk) then
--      if reset = '1' then
        output_value <= GC_RESET_VALUE;
--      else
--        output_value <= input_value;
--      end if;
    end if;
  end process p_output_value;

end architecture sim;
