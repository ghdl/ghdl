entity qual is
end;

architecture behav of qual is
begin
  process
    variable cnt : natural range 0 to 15;
  begin
    cnt := cnt'last'(others => '1');
  end process;
end behav;
