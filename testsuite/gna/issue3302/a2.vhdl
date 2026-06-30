entity A2 is
  port (
    d : out integer range 0 to 511
  );
end A2;

architecture rtl of A2 is
begin
end architecture;

entity B2 is
end B2;

architecture rtl of B2 is
  signal d : integer range 0 to 511;
begin
  A : entity work.A2(rtl)
    port map(d => d);
end architecture;

entity C2 is
end C2;

architecture sim of C2 is
begin

  B_INST : entity work.B2(rtl);

  C_PROC : process
    variable e : integer;
  begin
    e := << signal B_INST.d : integer range 0 to 512 >> ;
    wait;
  end process;

end architecture;
