entity tb is
end entity;

architecture arch of tb is
begin
  process begin
    report "Hello!" severity failure;
    wait;
  end process;
end;
