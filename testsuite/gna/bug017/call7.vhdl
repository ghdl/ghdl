entity call7 is
end;

architecture behav of call7 is
  procedure check (s : string) is
  begin
    wait for 1 ns;
    assert s (2) = 'a';
  end;
  function get_str
    return string is
  begin
    return "xaz";
  end get_str;
begin
  process
  begin
    check ("bac");
    wait for 2 ns;
    check (get_str);
    report "SUCCESS" severity note;
    wait;
  end process;
 
end behav;
