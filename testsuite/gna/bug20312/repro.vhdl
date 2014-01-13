entity pipeline is
  generic (width : natural; depth : natural);
  port (i : bit_vector (1 to width);
        o : out bit_vector (1 to width);  
        clk : bit);
end pipeline;

architecture behav of pipeline is
  type pipe is array (1 to depth) of bit_vector (1 to width);
begin
  process (clk) is
    variable tmp : pipe;
  begin
    if clk = '1' then
      o <= tmp (1);
      tmp (1 to depth - 1) := tmp (2 to depth);
      tmp (depth) := i;
    end if;
  end process;
end behav;

entity tb is
end tb;

architecture behav of tb is
  constant width : natural := 4;
  signal i : bit_vector (1 to width);
  signal o : bit_vector (1 to width);
  signal clk : bit;
begin
  p : entity work.pipeline
    generic map (width => width, depth => 3)
    port map (i => i, o => o, clk => clk);
  process is
  begin
    i <= "1011";
    
    clk <= '0';
    wait for 0 ns;
    clk <= '1';
    wait for 0 ns;

    i <= "1010";
    
    clk <= '0';
    wait for 0 ns;
    clk <= '1';
    wait for 0 ns;

    i <= "1001";
    
    clk <= '0';
    wait for 0 ns;
    clk <= '1';
    wait for 0 ns;

    i <= "1000";
    
    clk <= '0';
    wait for 0 ns;
    clk <= '1';
    wait for 0 ns;

    i <= "1011";
    
    clk <= '0';
    wait for 0 ns;
    clk <= '1';
    wait for 0 ns;

    wait;
  end process;
end behav;
