entity e is
end entity;  -- line of the end of :e:
             -- continuation of :e:

architecture a of e is
  component c is
    port (p : bit);
  end component;  -- line of the end of :c:

  -- for :s:
  signal s : bit;
begin
end architecture;  -- line of the end of :a:

package p is
  type r is record
    x : bit;
  end record;  -- line of the end of :r:

  -- for :r2:
  type r2 is record
    y : bit;
  end record r2;  -- line of the end of :r2:

  type pt is protected
    procedure m;
  end protected;  -- line of the end of :pt:

  function f return bit;

  -- for :k:
  constant k : bit := '0';
end package;  -- line of the end of :p:

package body p is
  type pt is protected body
    procedure m is
    begin
    end procedure;  -- line of the end of :m:
  end protected body;  -- line of the end of :pt:

  function f return bit is
  begin
    return '0';
  end function;  -- line of the end of :f:

  -- for :k2:
  constant k2 : bit := '0';
end package body;  -- line of the end of :p:
