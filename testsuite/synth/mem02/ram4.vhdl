library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram4 is
  port (raddr : std_logic_vector (1 downto 0);
        rdat : out std_logic_vector(1 downto 0);
        rst : std_logic;
        init : std_logic_vector (7 downto 0);
        clk : std_logic);
end ram4;

architecture behav of ram4 is
  type rindx is record
    idx : natural;
  end record;

  signal idx : rindx;
  signal mem : std_logic_vector(7 downto 0);
begin
  process (clk)
  begin
    if rising_edge (clk) then
      if rst = '1' then
        --  As MEM is written in a whole, this is not a RAM.
        mem <= init;
      end if;
      rdat <= mem((idx.idx+1) * 2 - 1 downto idx.idx * 2);
    end if;
  end process;

  idx.idx <=to_integer(unsigned (raddr));
end behav;
