library ieee;
use ieee.std_logic_1164.all;


entity dummy_sub2 is
port (
  clk : in std_logic;
  dummy : out std_logic
);
end entity;


architecture a of dummy_sub2 is
  signal first_cycle : std_logic := '1';
begin
  support : process (clk)
  begin
    if rising_edge(clk) then
      dummy <= '0';
      assert clk = '0';
    end if;
  end process;
  
end architecture;


library ieee;
use ieee.std_logic_1164.all;

entity dummy_top2 is
  port(
    clk : in std_logic;
    dummy : out std_logic
  );
end entity;

architecture a of dummy_top2 is
begin
  ------------------------------------------------------------------------------
  dummy_sub_inst : entity work.dummy_sub2
    port map(
      clk => clk,
      dummy => open -- Connecting dummy here triggers instantiation of dummy_sub
    );

end architecture;
