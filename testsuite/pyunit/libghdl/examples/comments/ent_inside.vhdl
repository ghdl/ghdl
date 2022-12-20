library ieee;
use ieee.std_logic_1164.all;

entity e2 is
  -- comments in design units (python doc-string style) :e2:
    -- might be multi line :e2:
    generic (
      -- comment before a generic :FREQUENCY:
        -- might be multiline :FREQUENCY:
        constant FREQUENCY : positive
    );
    port (
      signal Clock : in std_logic
    );
end entity;
