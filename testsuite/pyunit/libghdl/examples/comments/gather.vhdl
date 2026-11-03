package gather is
  generic (
    --  Comment for :gt:
    type gt;
    --  Comment for :gf:
    function gf return bit;
    --  Comment for :gp:
    procedure gp;
    --  Comment for :gk:
    package gk is new work.q generic map (<>)
  );

  --  Comment for :int:
  type int is range 0 to 7;
  type flt is range 0.0 to 1.0;  --  Comment for :flt:

  --  Comment for :phys:
  type phys is range 0 to 1000
    units
      --  Comment for :pu:
      pu;
      pk = 1000 pu;  --  Comment for :pk:
    end units;

  --  Comment for :node:
  type node;

  --  Comment for :sub:
  subtype sub is int range 0 to 3;
  constant c : int := 1;

  --  Comment for :al:
  alias al : int is c;

  --  Comment for :at:
  attribute at : string;
  --  Comment for :at: (the attribute specification)
  attribute at of c : constant is "x";

  --  Comment for :pt:
  type pt is protected
    --  Comment for :m:
    procedure m;
  end protected;

  --  Comment for :gtpl:
  group gtpl is (constant, constant);
  --  Comment for :grp:
  group grp : gtpl (c, c);

  --  Comment for :comp:
  component comp is
    port (
      --  Comment for :x:
      x : in bit
    );
  end component;

  type rec is record
    a : bit;
  end record;

  --  Comment for :vw:
  view vw of rec is
    a : in;
  end view;
end package;

package body gather is
  --  Comment for :gather: (the package body)

  --  Comment for :pt: (the protected type body)
  type pt is protected body
  end protected body;
end package body;

entity ent is
end entity;

architecture arch of ent is
  --  Comment for :comp: (the configuration specification)
  for all : comp use entity work.ent;
begin
  --  Comment for :blk:
  blk : block
  begin
  end block;
end architecture;
