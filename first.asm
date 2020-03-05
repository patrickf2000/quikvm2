lbl main
i_load 4
i_load 6
i_add
i_print
jmp part2

lbl part1
i_load 7
i_print
jmp done

lbl part2
i_load 100
i_print
jmp part1

lbl done
exit
