-- Adapted from https://vhdlwhiz.com/linked-list/
package generic_stack_pkg is
  generic (
    type datatype
  );
  type stack is protected
    procedure push(data : in datatype);
    impure function pop return datatype;
    impure function is_empty return boolean;
  end protected;
end package generic_stack_pkg;

package body generic_stack_pkg is
  type stack is protected body
    type item;
    type ptr is access item;
    type item is record
      data : datatype;
      next_item : ptr;
    end record;
    variable root : ptr;

    procedure push(data : in datatype) is
      variable new_item : ptr;
      variable node : ptr;
    begin
      new_item := new item;
      new_item.data := data;
      if root = null then
        root := new_item;
      else
        node := root;
        while node.next_item /= null loop
          node := node.next_item;
        end loop;
        node.next_item := new_item;
      end if;
    end;

    impure function pop return datatype is
      variable node : ptr;
      variable ret_val : datatype;
    begin
      node := root;
      root := root.next_item;
      ret_val := node.data;
      deallocate(node);
      return ret_val;
    end;

    impure function is_empty return boolean is
    begin
      return root = null;
    end;
  end protected body;
end package body generic_stack_pkg;
