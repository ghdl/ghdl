entity A is
  port (
    d : out integer range 0 to 511
  );
end A;

architecture rtl of A is
begin
end architecture;

entity B is
end B;

architecture rtl of B is
  signal d : integer range 0 to 511;
begin
  A : entity work.A(rtl)
    port map(d => d);
end architecture;

entity C is
end C;

architecture sim of C is
begin

  B_INST : entity work.B(rtl);

  C_PROC : process
    variable e : integer;
  begin
    e := << signal B_INST.d : integer range 0 to 511 >> ;
    wait;
  end process;

end architecture;
