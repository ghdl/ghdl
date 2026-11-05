entity e is  -- line of the header of :e:
             -- continuation of :e:
  port (
    -- for :a:
    a : in bit  -- line of :a:
  );
end entity;

package p is  -- line of the header of :p:
              -- continuation of :p:
  -- for :c:
  constant c : bit := '0';

  type r is record  -- line of the header of :r:
                    -- continuation of :r:
    -- for :x:
    x : bit;  -- line of :x:
  end record;

  --  before :r2:
  type r2 is
    record  -- line of the header of :r2:
      y : bit;
    end record;

  type pt is protected  -- line of the header of :pt:
                        -- continuation of :pt:
    -- for :m:
    procedure m;
  end protected;

  component comp is  -- line of the header of :comp:
    port (
      -- for :z:
      z : bit
    );
  end component;
end package;

package body p is  -- line of the header of :p:
                   -- continuation of :p:
  -- before :pt:
  type pt is protected body  -- line of the header of :pt:
                             -- continuation of :pt:
    -- for :m:
    procedure m is
    begin
    end m;
  end protected body;
end package body;

architecture a of e is  -- line of the header of :a:
                        -- continuation of :a:
  -- for :s:
  signal s : bit;
begin
  pr : process (s) is  -- line of the header of :pr:
                       -- continuation of :pr:
    -- for :v:
    variable v : bit;
  begin
  end process;

  pr2 : process  -- line of the header of :pr2:
    -- for :v2:
    variable v2 : bit;
  begin
    wait;
  end process;

  bk : block is  -- line of the header of :bk:
                 -- continuation of :bk:
    -- for :t:
    signal t : bit;
  begin
  end block;
end architecture;

architecture a2 of e  -- before 'is' of :a2:
is  -- line of the 'is' of :a2:
  -- for :s2:
  signal s2 : bit;
begin
end architecture;
