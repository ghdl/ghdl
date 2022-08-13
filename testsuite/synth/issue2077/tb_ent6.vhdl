entity tb_ent6 is
end tb_ent6;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent6 is
  signal waddr2, raddr2 : natural range 0 to 255;
  signal wdat : std_logic_vector (7 downto 0);
  signal rdat : std_logic;
  signal wen : std_logic;
  signal clk : std_logic;
begin
  dut: entity work.ent6
    generic map (WIDTH => 8)
    port map (clk => clk, write_enable => wen,
              write_address => waddr2, input => wdat,
              read_address => raddr2, output => rdat);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    waddr2 <= 3;
    wdat <= x"13";
    wen <= '1';
    pulse;

    waddr2 <= 2;
    wdat <= x"ff";
    pulse;

    raddr2 <= 3;
    wen <= '0';
    pulse;
    assert rdat = '0' severity failure;

    raddr2 <= 2;
    pulse;
    assert rdat = '1' severity failure;

    wait;
  end process;
end behav;
