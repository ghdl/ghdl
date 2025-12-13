entity eq01 is
  port (i : in bit;
        o : out boolean);
end;

architecture behav of eq01 is
  type t_mem is array(natural range <>) of bit_vector;

begin
  process(i)
    variable s1 : t_mem(0 to 2)(3 downto 0);
    variable s2 : t_mem(0 to 2)(2 downto 0);
  begin
    s2(0)(0) := i;
    o <=  s1 /= s2;
  end process;
end;
