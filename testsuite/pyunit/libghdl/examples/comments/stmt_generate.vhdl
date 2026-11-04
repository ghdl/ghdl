architecture arch of e is
  constant k : bit := '1';
  signal s, t : bit;
begin
  -- before :fg:
  fg : for i in 0 to 3 generate  -- line of the header of :fg:
                                 -- continuation of :fg:
    -- before :x:
    signal x : bit;  -- line of :x:
  begin  -- line of 'begin' of :fg:
    -- before :s1:
    s1 : x <= s;  -- line of :s1:
  end generate;  -- line of the end of :fg:

  -- before :ig:
  ig : if a1 : k = '1' generate  -- line of the header of :ig:
    -- before :s2:
    s2 : t <= s;  -- line of :s2:
  -- before the alternative :ig_else:
  elsif a2 : k = '0' generate  -- line of :ig_else:
                               -- continuation of :ig_else:
    -- before :s3:
    s3 : t <= s;
  -- before the alternative :ig_else:
  else a3 : generate  -- line of :ig_else:
    -- before :s4:
    s4 : t <= s;
  end a3;
  end generate;  -- line of the end of :ig:

  -- before :cg:
  cg : case k generate  -- line of the header of :cg:
    -- before the alternative :cg_when:
    when c0 : '0' =>  -- line of :cg_when:
      -- before :s5:
      s5 : t <= s;
    -- before the alternative :cg_when:
    when '1' =>  -- line of :cg_when:
      -- before :s6:
      s6 : t <= s;
  end generate;  -- line of the end of :cg:
end architecture;
