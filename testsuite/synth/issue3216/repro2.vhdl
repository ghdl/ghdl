library ieee;
use ieee.std_logic_1164.all,
  ieee.numeric_std.all;

entity repro_2_atthesametime is

  port (
    input_cmd  : in  std_logic_vector(3 downto 0);
    input_data : in  std_logic_vector(20 downto 0);
    the_output : out std_logic_vector(4 downto 0)
    );
end entity repro_2_atthesametime;

architecture arch of repro_2_atthesametime is
 begin

  main_proc : process (input_cmd) is
    variable the_index : integer;
    variable op_A, op_B, op_S : signed(4 downto 0 );
    variable temp_var : std_logic_vector( 3 downto 0 );
  begin
    the_index := to_integer(unsigned(input_cmd));
    -- the_index := 4;
    op_A := ("0",signed(input_data(the_index + 3 downto the_index) ));
    -- temp_var := input_data(the_index + 3 downto the_index);
    -- op_A := ("0",signed(temp_var));
    op_B := "00001";
    op_S := op_A + op_B;
    the_output <= std_logic_vector(op_S);
  end process main_proc;

end architecture arch;
