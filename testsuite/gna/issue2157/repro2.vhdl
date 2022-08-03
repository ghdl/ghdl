library ieee;
  use ieee.std_logic_1164.all;

entity repro is
  port (
     a, c, d, e, f, x : std_logic;
     b : std_logic_vector(3 downto 0);
     clk : in std_logic
  );
end;

architecture psl of repro is
begin
  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  NEXT_EVENT_0_a :
--    assert always ((a and b = x"4") -> next_event_a(c)[1 to 2](b = x"4"))
--    assert always ((a and b = x"4") -> ({(not c)[*]; c; {[*0] | { (not c)[*]; c}}} |->(b = x"4")))
--    assert always (a -> ({c; d; {[*0] | {(not e)[*];f}}} |->x))
--        assert always (a -> ({c; {[*0] | {d[*]; e}}} |->x))
--    assert always ({c; {[*0] | {d[*]; e}}} |->x)
--      assert never {c; {[*0] | {d[*]; e}}}
--    assert always ({c | {c; d[*]; e}} |->x)
--    assert always ({c; {d[*]; e}} |->x)
    report "NEXT_EVENT_0_a failed";
end architecture psl;

--  next_event_a(b)[k:l](p) = {b[->k:l]} |-> p
--  b[->k:l] = b[->k] | ... | b[->l]
--            = {!b[*];b}[*k:l]
--  s[*i:j] = s[*i] | ... | s[*j]

--  next_event_a(b)[k:l](p) = {{!b[*];b}[*k] | .. | {!b[*];b}[*l] } |-> p
