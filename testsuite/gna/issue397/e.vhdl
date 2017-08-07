entity bug is
end entity;

architecture a of bug is
begin
  main : process
    type rec_t is record
      field : natural;
    end record;

    impure function get_rec return rec_t is
    begin
      return (field => 10);
    end;

    -- Comment out this procedure to avoid the crash
    procedure get_rec(variable rec : out rec_t) is
    begin
      rec := get_rec;
    end;

  begin
    assert get_rec.field = 10 severity failure;
    wait;
  end process;
end;
