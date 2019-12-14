# Gdb commands useful to debug ghdl

define pt
call vhdl.disp_tree.disp_iir ($arg0, 1, 10)
end

document pt
Print the structure of the iirs that is $arg0.
end

define ptf
call vhdl.disp_tree.disp_iir ($arg0, 1, 0)
end

document ptf
Print the iirs that is $arg0.
end

define pt1
call vhdl.disp_tree.disp_iir ($arg0, 1, 1)
end

define prt
set lang c
print (iirs__iir *) $
set lang ada
end

define pl
call disp_iir_list ($arg0, 0, 0)
end

define psrc
  call debug_source_loc (vhdl.nodes.get_location ($arg0))
end

document pl
Print the list of iirs that is $arg0.
end

define plf
call disp_iir_list ($arg0, 0, 1)
end

document plf
Print flatly the list of iirs that is $arg0.
end

define ptc
call vhdl.disp_tree.disp_chain ($arg0, 0, 0)
end

document ptc
Print the chain of iirs that is $arg0
end

define pv
call disp_value ($arg0)
end

document pv
Print the value that is $.
end

define ploc
call disp_iir_location ($arg0)
end

document ploc
Print the location for iir $.
end

set lang ada

# Must be the last command: some distributions use a shared libgnat by default,
# and don't have the minimal required set of debug info to support this command.
# As a result, this command fails and stop this script.
catch exception
