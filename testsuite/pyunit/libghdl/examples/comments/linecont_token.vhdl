entity linecont_token is
  port (
    q : out bit  --  Comment for :q:
  );
end entity;

--  Comment for :a: (after the last port of the entity)
architecture a of linecont_token is
begin
end architecture;

package pkg is
  type rec is record
    a : bit;  --  Comment for :a:
  end record;

  --  Comment for :c: (after the record)
  constant c : natural := 1;

  function f (
    x : bit  --  Comment for :x:
  ) return bit;

  --  Comment for :p: (after the parameters)
  procedure p;
end package;
