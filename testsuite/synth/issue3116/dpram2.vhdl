library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dpram2 is
  port (raddr : std_logic_vector (3 downto 0);
        rdat : out std_logic;
        r0 : out std_logic_vector (16 downto 0);
        clk : std_logic);
end;

architecture behav of dpram2 is
  type mem_t is array(0 to 1) of std_logic_vector(15 downto 0);
  signal mem : unsigned(0 to 15) := x"1234";
  signal mem2 : mem_t;
begin
  process (clk)
  begin
    if rising_edge (clk) then
      rdat <= mem (to_integer(unsigned (raddr)));
    end if;
  end process;

  mem2 <= (std_logic_vector(mem), std_logic_vector(mem));
  r0 <= mem2(0)(0) & mem2(1);
end behav;
