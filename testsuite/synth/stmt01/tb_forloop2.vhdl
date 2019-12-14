entity tb_forloop2 is
end tb_forloop2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_forloop2 is
  signal vin : std_logic_vector (7 downto 0);
  signal vout : std_logic_vector (3 downto 0);
  signal clk : std_logic;
  signal b : std_logic;
  signal c : std_logic;
  signal z : std_logic;
begin
  dut: entity work.forloop2
    port map (vin => vin, vout => vout, clk => clk);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    vin <= x"00";
    pulse;
    assert vout = x"0" severity failure;
    wait;
  end process;
end behav;
