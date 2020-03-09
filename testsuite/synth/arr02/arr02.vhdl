library ieee;
use ieee.std_logic_1164.all;

entity arr02 is
  port (
    a : std_logic_vector (31 downto 0);
    sel : natural range 0 to 3;
    clk : std_logic;
    res : out std_logic_vector (3 downto 0));
end arr02;

architecture behav of arr02 is
  type t_mem is array (0 to 3) of std_logic_vector (7 downto 0);
  type t_stage is record
    sel : natural range 0 to 3;
    val : t_mem;
  end record;

  signal s : t_stage;
begin
  process (clk) is
  begin
    if rising_edge (clk) then
      s.sel <= sel;
      s.val <= (a (31 downto 24),
                a (23 downto 16),
                a (15 downto 8),
                a (7 downto 0));
    end if;
  end process;

  process (clk) is
  begin
    if rising_edge (clk) then
      res <= s.val (s.sel)(3 downto 0);
    end if;
  end process;
end behav;
