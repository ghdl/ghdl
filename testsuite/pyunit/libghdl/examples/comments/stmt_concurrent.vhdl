entity e is
  port (a : in bit);
begin  -- line of 'begin' of :e:
  -- before :ea:
  ea : assert a = '1';  -- line of :ea:
end entity;

architecture arch of e is
  signal s, t : bit;
  component comp is
  end component;
  procedure pr (x : bit) is
  begin
  end pr;
  default clock is rising_edge (s);
begin  -- line of 'begin' of :arch:
  -- before :sa:
  sa : s <= t;  -- line of :sa:
                -- continuation of :sa:
  -- before :ca:
  ca : s <= '1' when t = '1' else  -- inside :ca:
            -- inside :ca:
            '0';  -- line of the end of :ca:

  -- before :nolabel:
  t <= s;  -- line of :nolabel:

  -- before :sel:
  sel : with s select  -- inside :sel:
    t <= '1' when '0',
         '0' when others;  -- line of the end of :sel:

  -- before :ie:
  ie : entity work.e  -- inside :ie:
    port map (  -- inside :ie:
      -- inside :ie:
      a => s  -- inside :ie:
    );  -- line of the end of :ie:

  -- before :ic:
  ic : comp;  -- line of :ic:

  -- before :pc:
  pc : pr (s);  -- line of :pc:

  -- before :asrt:
  asrt : assert s = '1' report "message";  -- line of :asrt:

  -- before :cv:
  cv : cover {s};  -- line of :cv:

  -- before :asm:
  asm : assume always s;  -- line of :asm:

  -- before :rs:
  rs : restrict {s};  -- line of :rs:

  -- before :pa:
  pa : assert always s;  -- line of :pa:

  -- before :p:
  p : process (s)  -- line of the header of :p:
  begin  -- line of 'begin' of :p:
    -- before :st:
    st : null;  -- line of :st:
  end process;  -- line of the end of :p:

  -- before :b:
  b : block  -- line of the header of :b:
  begin  -- line of 'begin' of :b:
    -- before :inb:
    inb : t <= s;  -- line of :inb:
  end block;  -- line of the end of :b:
end architecture;
