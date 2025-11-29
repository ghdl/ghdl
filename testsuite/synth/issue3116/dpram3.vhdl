library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dpram3 is
  port (raddr : std_logic_vector (3 downto 0);
        rdat : out std_logic;
        r0 : out std_logic_vector (16 downto 0);
        clk : std_logic);
end;

architecture behav of dpram3 is
  type mem_t is array(0 to 1) of std_logic_vector(15 downto 0);
  signal mem : unsigned(0 to 15) := x"1234";
begin
  process (clk)
  begin
    if rising_edge (clk) then
      rdat <= mem (to_integer(unsigned (raddr)));
    end if;
  end process;

  r0 (15 downto 0) <= std_logic_vector(mem);
  r0 (16) <= '1';
end behav;
