
library ieee;
use ieee.std_logic_1164.all,
  ieee.numeric_std.all;

entity tb_repro_case_qm is

end entity tb_repro_case_qm;

architecture arch of tb_repro_case_qm is
  signal CLK        : std_logic                    := '1';
  signal counter    : std_logic_vector(2 downto 0) := "000";
  signal the_output : std_logic_vector(1 downto 0);
  signal exit_flag  : boolean                      := false;
  component repro_case_qm is

    port (
      CLK        : in  std_logic;
      the_input  : in  std_logic_vector(2 downto 0);
      the_output : out std_logic_vector(1 downto 0));

  end component repro_case_qm;
begin  -- architecture arch

  main_proc : process
  begin
    if not exit_flag then
      CLK_IF : if CLK = '0' then
        counter <= std_logic_vector(unsigned(counter) + 1);
        assert false report "Input: " & integer'image(to_integer(unsigned(counter))) & ", Output: " & integer'image(to_integer(unsigned(the_output))) severity note;
        if counter = "111" then
          exit_flag <= true;
        end if;
      end if CLK_IF;
      CLK <= not CLK;
      wait for 1 ps;
    else
      wait;                             -- for ever
    end if;
  end process main_proc;

  repro_case_qm_instanc : repro_case_qm port map (
    CLK        => CLK,
    the_input  => counter,
    the_output => the_output);
end architecture arch;
