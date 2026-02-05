library ieee;
use ieee.std_logic_1164.all;

entity rotate_RL is

  generic (
    right_not_left : boolean := false);

  port (
    CLK        : in  std_logic;
    the_input  : in  std_logic_vector;
    the_output : out std_logic_vector);

end entity rotate_RL;

architecture arch of rotate_RL is

begin  -- architecture arch
  assert the_input'length = the_output'length report "The size of the input vector '" & integer'image(the_input'length) &
    ") should be the same as the output vector (" & integer'image(the_output'length) & ")"
    severity failure;
  assert right_not_left report "The rotation is left" severity note;
  assert not right_not_left report "The rotation is right" severity note;

  main_proc : process (CLK) is
  begin
    if rising_edge(CLK) then
      if right_not_left then
        the_output(the_output'high - 1 downto the_output'low) <=
          the_input(the_input'high downto the_input'low + 1);
        the_output(the_output'high) <= '0';
      else
        the_output(the_output'high downto the_output'low + 1) <=
          the_input(the_input'high - 1 downto the_input'low);
        the_output(the_output'low) <= '0';
      end if;
    end if;
  end process main_proc;
end architecture arch;
