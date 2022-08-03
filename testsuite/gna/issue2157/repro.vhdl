library ieee;
  use ieee.std_logic_1164.all;

entity repro is
  port (
     a, c : std_logic;
     b : std_logic_vector(3 downto 0);
     clk : in std_logic
  );
end;

architecture psl of repro is
begin
  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  NEXT_EVENT_0_a :
    assert always ((a and b = x"4") -> next_event_a(c)[1 to 2](b = x"4"))
    report "NEXT_EVENT_0_a failed";
end architecture psl;
