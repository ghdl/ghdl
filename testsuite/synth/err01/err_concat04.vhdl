entity err_concat04 is
  generic (idx : natural := 4);
  port (o : out bit);
end;

architecture behav of err_concat04 is
  type t_mem is array(natural range <>) of bit_vector;

begin
  process
    variable bv1 : bit_vector(1 downto 0);
    variable bv2 : bit_vector(3 downto 0);
  begin
    assert t_mem'(bv1 & bv2) =(0 to 1 => "00");
    wait;
  end process;
end;
