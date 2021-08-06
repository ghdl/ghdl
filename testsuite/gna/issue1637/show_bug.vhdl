entity show_bug is
end entity show_bug;

architecture arch of show_bug is
begin

  process is
  begin
        report("a", "b");
        wait;
  end process;

end architecture arch;
