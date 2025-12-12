entity err_concat03 is
  generic (idx : natural := 4);
  port (o : out bit);
end;

architecture behav of err_concat03 is
  type t_mem is array(natural range <>) of bit_vector;

begin
  process
    variable s1 : t_mem(0 to 2)(3 downto 0);
    variable s2 : t_mem(0 to 2)(1 downto 0);
  begin
    assert s1 & s2 =(0 to 5 => "00");
    wait;
  end process;
end;
