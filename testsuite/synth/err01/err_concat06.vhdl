entity err_concat06 is
  generic (idx : natural := 4);
  port (i : in bit;
        o : out boolean);
end;

architecture behav of err_concat06 is
  type t_mem is array(natural range <>) of bit_vector;

begin
  process(i)
    variable s1 : t_mem(0 to 2)(3 downto 0);
    variable bv : bit_vector(1 downto 0);
  begin
    bv(0) := i;
    o <=  (bv & s1) = (0 to 3 => "00");
  end process;
end;
