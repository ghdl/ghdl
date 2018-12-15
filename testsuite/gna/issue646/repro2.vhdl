entity repro2 is
end entity;

architecture tb of repro2 is

  type tb_cfg_t is record
    value : string;
  end record tb_cfg_t;

  function get_msg return string is
  begin
    return "goodbye";
  end get_msg;

  constant tb_cfg: tb_cfg_t := ( value => get_msg );
begin
  assert tb_cfg.value > "a";
end tb;


