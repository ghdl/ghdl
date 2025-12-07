library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity slice13 is
  port (clk : std_logic;
        addr1 : std_logic_vector(1 downto 0);
        addr2 : std_logic_vector(2 downto 0);
        dat : std_logic_vector (1 downto 0);
        res : out std_logic_vector (1 downto 0));
end slice13;

architecture behav of slice13 is
  signal a1, a2 : natural;
  type t_rec is record
    b : std_logic;
    v : std_logic_vector (7 downto 0);
  end record;
  type t_mem is array (natural range <>) of t_rec;
  signal mem : t_mem(0 to 3) := (('1', x"01"),
                                 ('0', x"02"),
                                 ('0', x"d3"),
                                 ('1', x"e4"));
begin
  a1 <= to_integer(unsigned(addr1));
  a2 <= to_integer(unsigned(addr2));

  process(clk)
  begin
    if rising_edge (clk) then
      res <= mem (a1).v(a2*2 + 1 downto a2*2);
    end if;
  end process;
end behav;
