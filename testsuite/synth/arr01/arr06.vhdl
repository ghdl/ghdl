library ieee;
use ieee.std_logic_1164.all;

entity arr06 is
  port (clk : in std_logic;
        val : std_logic_vector(7 downto 0);
        res : out std_logic_vector(7 downto 0);
        par : out std_logic);
end arr06;

architecture behav of arr06 is
  type pipe_el is record
    val : std_logic_vector(7 downto 0);
    odd : std_logic;
  end record;

  type pipe_type is array (0 to 4) of pipe_el;

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
    variable v : pipe_type;
  begin
    for i in 1 to pipe_type'high loop
      v (i) := mem (i - 1);
    end loop;
    v (0).val := val;
    v (0).odd := tick;

    n_mem <= v;
  end process;

  res <= mem (pipe_type'high).val;
  par <= mem (pipe_type'high).odd;
end behav;
