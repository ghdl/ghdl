entity tb_ram02 is
end tb_ram02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ram02 is
  signal raddr, waddr : std_logic_vector(3 downto 0) := (others => '0');
  signal rdat : std_logic_vector(7 downto 0);
  signal wdat : std_logic_vector(7 downto 0);
  signal wen : std_logic;
  signal clk : std_logic;
begin
  dut: entity work.ram02
    port map (raddr => raddr, rdat => rdat,
              waddr => waddr, wdat => wdat,
              wen => wen, clk => clk);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    --  [0] := x03, 1
    waddr <= "0000";
    wdat <= x"03";
    wen <= '1';
    pulse;

    --  [1] := x41
    waddr <= "0001";
    wdat <= x"41";
    raddr <= "0000";
    pulse;
    assert rdat = x"03" severity failure;

    --  [4] := x07
    waddr <= "0100";
    wdat <= x"07";
    raddr <= "0001";
    wait for 1 ns;
    pulse;
    assert rdat = x"41" severity failure;

    --  Not en.
    waddr <= "0000";
    wen <= '0';
    raddr <= "0000";
    pulse;
    assert rdat = x"03" severity failure;

    --  [4] := x23
    wen <= '1';
    waddr <= "0100";
    wdat <= x"23";
    raddr <= "0001";
    pulse;
    assert rdat = x"41" severity failure;

    wait;
  end process;
end behav;
