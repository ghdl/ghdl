entity C is
end;

architecture sim of C is
begin
  process
  begin
    report "ab" & "cd" & 'e';
    report "&"("abc" & 'd', "e");
    report "&"("&"("abc", 'd'), "e");
    wait;
  end process;
end architecture;
