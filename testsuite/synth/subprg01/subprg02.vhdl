library ieee;
use ieee.std_logic_1164.all;

entity subprg02 is
  port (a : std_logic_vector (3 downto 0);
        n : natural range 0 to 1;
        clk : std_logic;
        n0 : out std_logic_vector (3 downto 0);
        na : out std_logic_vector (3 downto 0));
end subprg02;

architecture behav of subprg02 is
  procedure neg (v : inout std_logic_vector(3 downto 0)) is
  begin
    v := not v;
  end neg;

begin
  process(clk)
    type t_arr is array (natural range <>) of std_logic_vector(3 downto 0);
    variable mem : t_arr (0 to 1);
  begin
    if rising_edge (clk) then
      mem (n) := a;
      neg (mem (n));
      na <= mem (n);
    end if;

    --  FIXME: this is needed so that MEM is not considered as a memory.
    n0 <= mem (0);
  end process;
end behav;
