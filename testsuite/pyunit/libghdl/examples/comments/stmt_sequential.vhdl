architecture arch of e is
  procedure pr (x : bit) is
    -- before :v:
    variable v : bit;
  begin  -- line of 'begin' of :pr:
    -- before :s1:
    s1 : v := x;  -- line of :s1:
                  -- continuation of :s1:
    -- before :nolabel:
    v := x;  -- line of :nolabel:

    -- before :c:
    c : pr (x);  -- line of :c:

    -- before :w:
    w : wait;  -- line of :w:

    -- before :a:
    a : assert false report "x";  -- line of :a:

    -- before :r:
    r : report "x";  -- line of :r:

    -- before :n:
    n : null;  -- line of :n:

    -- before :ml:
    ml : v := x  -- inside :ml:
           when x = '1' else '0';  -- line of the end of :ml:

    -- before :i:
    i : if x = '1' then  -- line of the header of :i:
                         -- continuation of :i:
      -- before :i1:
      i1 : null;  -- line of :i1:
    -- before the clause :i_else:
    elsif x = '0'  -- inside :i_else:
      or x = '1' then  -- line of :i_else:
      -- before :i2:
      i2 : null;
    -- before the clause :i_else:
    else  -- line of :i_else:
      -- before :i3:
      i3 : null;
    end if;  -- line of the end of :i:

    -- before :cs:
    cs : case x is  -- line of the header of :cs:
      -- before the alternative :cs_when:
      when '0' =>  -- line of :cs_when:
        -- before :c1:
        c1 : null;
      -- before the alternative :cs_when:
      when others =>  -- line of :cs_when:
        -- before :fl:
        fl : for j in 0 to 1 loop  -- line of the header of :fl:
          -- before :e1:
          e1 : exit;  -- line of :e1:
        end loop;  -- line of the end of :fl:
        -- before :wl:
        wl : while true loop  -- line of the header of :wl:
          -- before :n1:
          n1 : next;  -- line of :n1:
        end loop;
        -- before :ll:
        ll : loop  -- line of the header of :ll:
          -- before :rt:
          rt : return;  -- line of :rt:
        end loop;
    end case;  -- line of the end of :cs:
  end pr;
begin
  p : process
  begin
    -- before :ps:
    ps : wait;  -- line of :ps:
  end process;
end architecture;
