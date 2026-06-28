library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

entity myentity is
  port (
    inl : inout std_logic := 'U';
    inr : inout real := -4.2
    );
end myentity;

architecture arch of myentity is
begin
  dotrans : process
    constant vals    : std_logic_vector (1 to 9) := "10ZWLH-UX";
    constant expect  : std_logic_vector (1 to 7) := "01ZXHL-";
    constant scalars : String (1 to 7) := "01ZXHL-";
    variable i       : integer := 1;
  begin
    wait for 1 ns;
    if i <= 9 then
      inl <= vals (i);
      wait for 1 ns;
    elsif i <= 16 then
      -- Each time inr changes, VPI will set a new value for inl.
      inr <= Real (i);
      wait for 1 ns;
      if inl /= expect (i - 9) then
        report ("Error: Translated scalar " & scalars (i - 9) &
                " to std_logic " & std_logic'image (inl) & ", " &
                std_logic'image (expect (i - 9)) & "was expected."); 
      end if;
    else
      report "All tests passed." severity failure;
    end if;
    i := i + 1;
  end process dotrans;
end arch;

