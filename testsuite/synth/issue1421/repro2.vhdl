entity repro2 is
  port (clk : bit;
        rst : bit;
        v : bit_vector (1 downto 0);
        res : out bit_vector(1 downto 0));
end;

architecture behav of repro2 is
  type myrec is record
    b : bit;
    c : bit;
  end record;
  signal s, sin : myrec;
begin
  sin <= (v(1), v(0));

  process (clk)
  begin
    if clk'event and clk = '1' then
      s <= sin;
    end if;
    if rst = '0' then
      s.c <= '0';
    end if;
  end process;

  res <= (s.c, s.b);
end behav;
