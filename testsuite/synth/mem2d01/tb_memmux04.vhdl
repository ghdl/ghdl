entity tb_memmux04 is
end tb_memmux04;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_memmux04 is
  signal wen  : std_logic;
  signal waddr : std_logic_vector (3 downto 0);
  signal wdat : std_logic_vector (31 downto 0);
  signal raddr : std_logic_vector (3 downto 0);
  signal rsel : std_logic_vector (1 downto 0);
  signal rdat : std_logic_vector (7 downto 0);
  signal clk  : std_logic;
begin
  dut : entity work.memmux04
    port map (
      wen  => wen,
      waddr => waddr,
      wdat => wdat,
      raddr => raddr,
      rsel => rsel,
      rdat => rdat,
      clk  => clk);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;

    variable v : std_logic_vector(3 downto 0);
    variable s : std_logic_vector(1 downto 0);
  begin
    wen <= '1';
    waddr <= x"0";
    wdat <= x"0123_5670";
    pulse;

    wen <= '1';
    waddr <= x"1";
    wdat <= x"1234_6781";
    raddr <= x"0";
    rsel <= "00";
    pulse;
    assert rdat = x"70" severity failure;

    --  Fill the memory.
    for i in 0 to 15 loop
      wen <= '1';
      v := std_logic_vector (to_unsigned (i, 4));
      waddr <= v;
      wdat (3 downto 0) <= v;
      wdat (7 downto 4) <= x"0";
      wdat (11 downto 8) <= v;
      wdat (15 downto 12) <= x"1";
      wdat (19 downto 16) <= v;
      wdat (23 downto 20) <= x"2";
      wdat (27 downto 24) <= v;
      wdat (31 downto 28) <= x"3";
      pulse;
    end loop;

    --  Check the memory.
    wen <= '0';
    for i in 0 to 15 loop
      v := std_logic_vector (to_unsigned (i, 4));
      raddr <= v;
      for j in 0 to 3 loop
        s := std_logic_vector (to_unsigned (j, 2));
        rsel <= s;
        pulse;
        assert rdat (3 downto 0) = v severity failure;
        assert rdat (5 downto 4) = s severity failure;
      end loop;
    end loop;
    wait;
  end process;
end behav;
