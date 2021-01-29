entity tb is
end entity;

architecture pass of tb is
begin
  process begin
    report "Hello wrapping/exitcb [pass]!" severity note;
    wait;
  end process;
end;

architecture fail of tb is
begin
  process begin
    report "Hello wrapping/exitcb [fail]!" severity failure;
    wait;
  end process;
end;
