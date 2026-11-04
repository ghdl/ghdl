entity e is
  port (a : in bit);
begin  -- line of 'begin' of e
  -- before ea
  ea : assert a = '1';  -- line of ea
end entity;

architecture arch of e is
  constant k : bit := '1';
  signal s, t : bit;
  component comp is
  end component;

  procedure pr (x : bit) is
    variable v : bit;
  begin  -- line of 'begin' of pr
    -- before s1
    s1 : v := x;  -- line of s1
                  -- continuation of s1
    -- before an unlabeled statement
    v := x;  -- line of the unlabeled statement

    -- before i
    i : if x = '1' then  -- line of the header of i
      -- before i1
      i1 : null;
    -- before the elsif
    elsif x = '0'  -- inside the elsif
      or x = '1' then  -- line of the elsif
      -- before i2
      i2 : null;
    -- before the else
    else  -- line of the else
      i3 : null;
    end if;  -- line of the end of i

    -- before cs
    cs : case x is  -- line of the header of cs
      -- before the first alternative
      when '0' =>  -- line of the first alternative
        -- before c1
        c1 : null;
      -- before the second alternative
      when others =>
        -- before fl
        fl : for j in 0 to 1 loop  -- line of the header of fl
          -- before e1
          e1 : exit;
        end loop;
        -- before wl
        wl : while true loop  -- line of the header of wl
          -- before n1
          n1 : next;
        end loop;
    end case;
  end pr;
begin  -- line of 'begin' of arch
  -- before sa
  sa : s <= t;  -- line of sa
                -- continuation of sa
  -- before ca
  ca : s <= '1' when t = '1' else  -- inside ca
            '0';  -- line of the end of ca

  -- before ie
  ie : entity work.e  -- inside ie
    port map (
      -- inside ie, again
      a => s
    );  -- line of the end of ie

  -- before ic, replaced by the analysis
  ic : comp;  -- line of ic

  -- before pc
  pc : pr (s);  -- line of pc

  -- before asrt, replaced by the analysis
  asrt : assert s = '1' report "message";  -- line of asrt

  -- before p
  p : process (s)  -- line of the header of p
    variable v : bit;
    -- before 'begin', discarded
  begin  -- line of 'begin' of p
    -- before st
    st : v := '1';  -- line of st
    -- before 'end', discarded
  end process;  -- line of the end of p

  -- before fg
  fg : for j in 0 to 3 generate  -- line of the header of fg
    -- before x
    signal x : bit;
  begin  -- line of 'begin' of fg
    -- before s1
    s1 : x <= s;  -- line of s1
  end generate;  -- line of the end of fg

  -- before ig
  ig : if a1 : k = '1' generate  -- line of the header of ig
    -- before s2
    s2 : t <= s;
  -- before the elsif generate
  elsif a2 : k = '0' generate  -- line of the elsif generate
    s3 : t <= s;
  -- before the else generate
  else a3 : generate  -- line of the else generate
    s4 : t <= s;
  end a3;
  end generate;

  -- before cg
  cg : case k generate  -- line of the header of cg
    -- before the first generate alternative
    when c0 : '0' =>  -- line of the first generate alternative
      s5 : t <= s;
    -- before the second generate alternative
    when others =>
      s6 : t <= s;
  end generate;
end architecture;
