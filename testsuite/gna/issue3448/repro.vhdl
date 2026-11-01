package pkg is
  type node;
  type ptr is access node;
  type node is record
    value : integer;
    nxt   : ptr;
  end record;
  constant c : integer;
end package;

package body pkg is
  constant c : integer := 5;
end package body;

package gen is
  generic (n : natural);
  type node;
  type ptr is access node;
  type node is record
    value : integer;
  end record;
end package;

package inst is new work.gen generic map (n => 3);

entity repro is
end entity;

architecture arch of repro is
  package inner is
    type node;
    type ptr is access node;
    type node is record
      value : integer;
    end record;
  end package;
begin
end architecture;
