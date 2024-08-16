library ieee;
use ieee.std_logic_1164.all;

entity ghdl_bug_2_tb is
end entity ghdl_bug_2_tb;

architecture sim of ghdl_bug_2_tb is

  type t_slv_array is array (natural range <>) of std_logic_vector;

  constant C_ARRAY_SIZE : natural := 2000;

  type t_record is
  record
    slv_array  : t_slv_array(0 to C_ARRAY_SIZE-1)(7 downto 0);
  end record t_record;

  function generate_record
  return t_record is
    variable v_record : t_record;
  begin
    return v_record;
  end function generate_record;

begin

  p_sequencer : process
    variable v_record : t_record;
  begin
    v_record := generate_record;
    std.env.finish;
  end process p_sequencer;

end architecture sim;
