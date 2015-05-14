entity ent is
end entity;

architecture a of ent is
begin
  main : process is
    constant c : boolean := false;
    variable v : boolean;

    type enum is (a, b, short, \Extended\, \Weird\\Name\);
    variable e : enum;
  begin
    report to_string(c); -- Cause TYPES.INTERNAL_ERROR
    report to_string(false);  -- Cause TYPES.INTERNAL_ERROR
    report to_string(integer'(1)); -- Cause TYPES.INTERNAL_ERROR
    report to_string(v); -- Works
    report to_string(\Extended\);
    report to_string(\Weird\\Name\);

    assert to_string(CR) = (1 => CR) severity failure;
    assert to_string(integer'(1)) = "1" severity failure;
    assert to_string(integer'(-12)) = "-12" severity failure;
    assert to_string(FaLse) = "false" severity failure;

    assert to_string (\Extended\) = "Extended" severity failure;
    assert to_string(\Weird\\Name\) = "Weird\Name" severity failure;
    assert enum'image(\Weird\\Name\) = "\Weird\\Name\" severity failure;
    e := \Weird\\Name\;
    assert enum'image(e) = "\Weird\\Name\" severity failure;
    report to_string(e);
    assert to_string(e) = "Weird\Name" severity failure;
    wait;
  end process;
end architecture;
