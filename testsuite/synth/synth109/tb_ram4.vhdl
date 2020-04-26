entity tb_ram4 is
end tb_ram4;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ram4 is
  signal clk : std_logic;
  signal en : std_logic;
  signal we : std_logic;
  signal addr : std_logic_vector(5 downto 0);
  signal rdat : std_logic_vector(31 downto 0);
  signal wdat : std_logic_vector(31 downto 0);
begin
  dut: entity work.ram4
    port map (clkB => clk, enB => en, weB => we, addrB => addr,
              diB => wdat, doB => rdat);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    en <= '1';
    we <= '1';
    addr <= b"00_0000";
    wdat <= x"11_22_33_f0";
    pulse;
    assert rdat = x"11_22_33_f0" severity failure;

    addr <= b"00_0001";
    wdat <= x"11_22_33_f1";
    pulse;
    assert rdat = x"11_22_33_f1" severity failure;

    --  Read.
    we <= '0';
    addr <= b"00_0000";
    wdat <= x"ff_22_33_f1";
    pulse;
    assert rdat = x"11_22_33_f0" severity failure;

    addr <= b"00_0001";
    wdat <= x"ff_22_33_f1";
    pulse;
    assert rdat = x"11_22_33_f1" severity failure;

    --  Disable.
    en <= '0';
    we <= '1';
    addr <= b"00_0000";
    wdat <= x"11_22_33_f0";
    pulse;
    assert rdat = x"11_22_33_f1" severity failure;

    wait;
  end process;
end behav;
