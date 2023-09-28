.text
main:
    addiu $v0, $zero, 10
    # Initialize $4 and $5
    addi $4, $zero, -5   # $4 = -5
    addi $5, $zero, 10   # $5 = 10
    
    # BLTZ: Branch if Less Than Zero
    bltz $4, ltz_label
    addi $6, $zero, 1    # $6 = 1 (executed if $4 >= 0)
    j end_label
    
ltz_label:
    addi $6, $zero, 2    # $6 = 2 (executed if $4 < 0)
    
    # BGEZ: Branch if Greater Than or Equal to Zero
    bgez $6, gez_label
    addi $7, $zero, 3    # $7 = 3 (executed if $4 < 0)
    j end_label
    
gez_label:
    addi $7, $zero, -4    # $7 = 4 (executed if $4 >= 0)
    
    # BLTZAL: Branch and Link if Less Than Zero
    bltzal $7, ltl_label
    addi $8, $zero, 5    # $8 = 5 (executed if $4 >= 0)
    j end_label
    
ltl_label:
    addi $8, $zero, 6    # $8 = 6 (executed if $4 < 0)
    
    # BGEZAL: Branch and Link if Greater Than or Equal to Zero
    bgezal $8, gtl_label
    addi $9, $zero, 7    # $9 = 7 (executed if $4 < 0)
    j end_label
    
gtl_label:
    addi $9, $zero, 8    # $9 = 8 (executed if $4 >= 0)
    
    # SLL: Shift Left Logical
    sll $10, $5, 2       # $10 = $5 << 2 (shift left by 2 bits)
    
    # SRL: Shift Right Logical
    srl $11, $5, 2       # $11 = $5 >> 2 (shift right by 2 bits)
    
    # SRA: Shift Right Arithmetic
    sra $12, $5, 2       # $12 = $5 >> 2 (arithmetic shift right by 2 bits)
    
    # SLLV: Shift Left Logical Variable
    addi $13, $zero, 2   # $13 = 2 (shift amount)
    sllv $14, $5, $13    # $14 = $5 << $13 (shift left by $13 bits)
    
end_label:
    # Exit
    addi $v0, $zero, 10   # Exit syscall
    syscall

