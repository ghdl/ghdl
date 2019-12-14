entity repro3 is
end entity;

architecture tb of repro3 is

  type tb_cfg_t is record
    value : string;
  end record tb_cfg_t;

  function get_msg return string is
  begin
    return "goodbye";
  end get_msg;

  function init return tb_cfg_t is
  begin
    return (value => get_msg);
  end init;

  constant tb_cfg: tb_cfg_t := init;
begin
  assert tb_cfg.value > "a";
end tb;


