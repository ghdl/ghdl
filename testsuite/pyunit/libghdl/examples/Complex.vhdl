-- :e1: comments before desIgn units (javadoc / .net documentation style)
-- :e1: mIGht be multiline
entity e1 is
end entitY;

-- :a1: comments before design units
-- :a1: might be multiline
architecture a1 of e1 is
begin
end architecture;

-- :p1: comments before design units
-- :p1: might be multiline
package p1 is
end package;

-- package body should be supported too to keep parity, but I have currently no usecase for it.

-- :ctx1: comments before design units
-- :ctx1: might be multiline
context ctx1 is
end context;

-- :cfg1: comments before design units
-- :cfg1: might be multiline
configuration cfg1 of e1 is
  for a1
  end for;
end configuration;


library ieee;
use ieee.std_logic_1164.all;

entity e2 is
  -- :e2: comments in design units (python doc-string style)
    -- :e2: might be multi line
    generic (
      -- :FREQUENCY: comment before a generic
        -- :FREQUENCY: might be multiline
        constant FREQUENCY : positive;
      constant BITS      : positive; -- :BITS: comment after a generic are mostly single line,
                                       -- :BITS: but could be multi line too
        -- in case comment is before and after
        constant DEBUG     : boolean   -- :DEBUG: the after has presidency
    );
    port (
      signal Clock : in std_logic    -- :Clock: same as for generics
    );
end entity;

architecture a2 of e2 is
  -- :a2: comments in design units (python doc-string style)
    -- :a2: might be multi line
begin

end architecture;

-- As packages define public elements like constants, types and sub-programs, we are interested in such documentation too.
package p2 is
  -- :p2: comments in design units (python doc-string style)
    -- :p2: might be multi line

    -- :DEBUG: comment before
    constant DEBUG : boolean := TRUE;
    constant SYNC_STAGES : positive := 3; -- :SYNC_STAGES: comment after

    -- :AType1: comment before
    type AType1 is array(natural range <>) of bit;
    type AType2 is array(natural range <>) of bit; -- :AType2: comment after

    -- same applies to subtype, alias, attributes, ...

    -- :RType: comment before
    type RType is record
        -- :RType: xor comment inside

        -- :elem1: per element comment before (note the comment "block" is separated by newlines)
        elem1 : integer;
        elem2 : integer; -- :elem2: per element comment behind
    end record;

    -- :log2: as functions are longer in definitions, it might be written before
    function log2(param : positive) return natural;

    function log2(
        -- :log2: otoh, we also want to document parameters too (similar to a record with comments)

      -- :param1: comment before
      param1 : integer;
        param2 : boolean  -- :param2: comment after
    ) return natural;

    -- this applies to procedures as well.



end package;

context ctx2 is
  -- :ctx2: comments in design units (python doc-string style)
    -- :ctx2: might be multi line
end context;

configuration cfg2 of e2 is
  -- :cfg2: comments in design units (python doc-string style)
  -- :cfg2: might be multi line
  for a2
  end for;
end configuration;







-- This should allow for any kind of documentation style and embedded documentation language.
-- A real implementation might use similar rules are Python+docutils+Sphinx. Here we would e.g.
-- document a function either before (or inside) a function declaration and use the
--   :arg name: description
-- syntax.


-- :math: Package `math` provides math extensions not provided by the IEEE packages.
package math is
    -- :log2: Computes the logarithm to base 2.
    -- :log2:
    -- :log2: :arg param: Input value
    -- :log2: :returns:   Logarithm
    function log2(param : positive) return natural;
end package;
