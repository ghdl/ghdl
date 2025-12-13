entity err_concat07 is
  generic (idx : natural := 4);
  port (i : in bit;
        o : out boolean);
end;

architecture behav of err_concat07 is
  type t_mem is array(natural range <>) of bit_vector;

begin
  process(i)
    variable bv1 : bit_vector(1 downto 0);
    variable bv2 : bit_vector(2 downto 0);
  begin
    bv1(0) := i;
    o <=  t_mem'(bv1 & bv2) = (0 to 3 => "00");
  end process;
end;
