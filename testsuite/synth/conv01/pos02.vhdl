entity pos02 is
end pos02;

architecture behav of pos02 is
begin
  process
    variable v : natural;
  begin
    v := 2;
    assert bit'val(v) > '1';
    wait;
  end process;
end behav;

            
