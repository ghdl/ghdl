entity tb_ent5 is
end tb_ent5;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent5 is
  signal addr1 : natural range 0 to 3;
  signal waddr2, raddr2 : natural range 0 to 255;
  signal rdat, wdat : std_logic_vector (7 downto 0);
  signal wen : std_logic;
  signal clk : std_logic;
begin
  dut: entity work.ent5
    generic map (WIDTH => 8)
    port map (clk => clk, write_enable => wen, active_way => addr1,
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
    addr1 <= 1;
    waddr2 <= 3;
    wdat <= x"13";
    wen <= '1';
    pulse;

    waddr2 <= 2;
    wdat <= x"12";
    pulse;

    raddr2 <= 3;
    wen <= '0';
    pulse;
    assert rdat = x"13" severity failure;

    wait;
  end process;
end behav;
