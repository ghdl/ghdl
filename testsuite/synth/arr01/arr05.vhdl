library ieee;
use ieee.std_logic_1164.all;

entity arr05 is
  port (clk : in std_logic;
        val : std_logic_vector(7 downto 0);
        res : out std_logic_vector(7 downto 0);
        par : out std_logic);
end arr05;

architecture behav of arr05 is
  type pipe_el is record
    val : std_logic_vector(7 downto 0);
    odd : std_logic;
  end record;

  type pipe_arr is array (0 to 4) of pipe_el;

  type pipe_type is record
    p : pipe_arr;
  end record;

  signal mem : pipe_type;
  signal n_mem : pipe_type;
  signal tick : std_logic := '0';
begin
  process(clk)
  begin
    if rising_edge (clk) then
      mem <= n_mem;
      tick <= not tick;
    end if;
  end process;

  process(mem, val, tick)
  begin
    for i in 1 to pipe_arr'high loop
      n_mem.p (i) <= mem.p (i - 1);
    end loop;
    n_mem.p (0).val <= val;
    n_mem.p (0).odd <= tick;
  end process;

  res <= mem.p (pipe_arr'high).val;
  par <= mem.p (pipe_arr'high).odd;
end behav;
