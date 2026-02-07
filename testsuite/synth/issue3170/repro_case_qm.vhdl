library ieee;
use ieee.std_logic_1164.all;

entity repro_case_qm is

  port (
    CLK        : in  std_logic;
    the_input  : in  std_logic_vector(2 downto 0);
    the_output : out std_logic_vector(1 downto 0));

end entity repro_case_qm;

architecture arch of repro_case_qm is

begin  -- architecture arch

  main_proc : process(the_input)
  begin
    if rising_edge(CLK) then
      case? the_input is
        when "000"  => the_output <= "00";
        when "001"  => the_output <= "01";
        when "01-"  => the_output <= "10";
        when "1--"  => the_output <= "11";
        when others => null;
      end case?;
    end if;
  end process main_proc;

end architecture arch;

