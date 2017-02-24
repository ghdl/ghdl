entity test is
end test;

architecture tb of test is
begin
  process
    variable a  : integer range 7 downto 0;
    variable b  : a'subtype;
    variable av : bit_vector(7 downto 0);
    variable bv : av'subtype;
  begin
    report integer'image(b'subtype'left);
    report integer'image(bv'left);

    -- The following line crashes the analysis.
    report integer'image(av'subtype'left);

    wait; -- forever
  end process;
end tb;
