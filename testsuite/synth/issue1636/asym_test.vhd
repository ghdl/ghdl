library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity asym_test is
  port (
    clk_i  : in  std_logic;
    dat_i  : in  std_logic_vector(15 downto 0);
    addr_i : in  std_logic_vector(3 downto 0);
    dat_o : out std_logic_vector(15 downto 0)
  );
end asym_test;

architecture arch of asym_test is

  type   reg_file_t is array (15 downto 0) of std_logic_vector(15 downto 0);
  signal reg_file : reg_file_t;

begin

  process(clk_i)
  begin
    if rising_edge(clk_i) then
        reg_file(to_integer(unsigned(addr_i)))(15 downto 1) <= dat_i(15 downto 1);
    end if;
  end process;

  process(addr_i, reg_file)
  begin
      dat_o <= reg_file(to_integer(unsigned(addr_i)));
  end process;

end arch;
