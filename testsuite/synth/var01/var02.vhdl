library ieee;
use ieee.std_logic_1164.all;

entity var02 is
  port (clk : std_logic;
        mask : std_logic_vector (3 downto 0);
        val : std_logic_vector (31 downto 0);
        res : out std_logic_vector (31 downto 0));
end var02;

architecture behav of var02 is
  signal r : std_logic_vector (31 downto 0) := (others => '0');
  signal r_up : std_logic_vector (31 downto 0) := (others => '0');
begin
  process (all)
    variable t : std_logic_vector (31 downto 0) := (others => '0');
    variable hi, lo : natural;
  begin
    t := r;
    for i in 0 to 3 loop
      if mask (i) = '1' then
        lo := i * 8;
        hi := lo + 7;
        t (hi downto lo) := val (hi downto lo);
      end if;
    end loop;
    r_up <= t;
  end process;

  process (clk)
  begin
    if rising_edge (clk) then
      r <= r_up;
    end if;
  end process;
  res <= r;
end behav;
