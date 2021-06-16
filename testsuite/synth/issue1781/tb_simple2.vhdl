entity tb_simple2 is
end tb_simple2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_simple2 is
  signal addr : std_logic_vector(7 downto 0);
  signal rdat : std_logic_vector(15 downto 0);
  signal wdat : std_logic_vector(15 downto 0);
  signal wren : std_logic;
  signal rden : std_logic;
  signal clk : std_logic;
begin
  dut: entity work.simple2
    port map (clk_i => clk, rden_i => rden, wren_i => wren,
              addr_i => addr, data_i => wdat, data_o => rdat);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    addr <= x"00";
    wdat <= x"0001";
    wren <= '1';
    rden <= '0';
    pulse;

    addr <= x"01";
    wdat <= x"0002";
    pulse;

    --  Simple read.
    addr <= x"00";
    wren <= '0';
    rden <= '1';
    pulse;
    assert rdat = x"0001" severity failure;

    --  Check write through.
    addr <= x"03";
    wren <= '1';
    wdat <= x"3333";
    pulse;
    assert rdat = x"3333" severity failure;

    wait;
  end process;
end behav;
