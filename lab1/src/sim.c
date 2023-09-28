#include <stdio.h>
#include "shell.h"

//sign extend :imm
uint32_t imm_sign_extend(uint32_t imm){
    int32_t have_sign_imm = *((int16_t*)&imm);
    uint32_t result = *((uint32_t*)&have_sign_imm);
    return result;
}
//sign extend :byte
uint32_t byte_sign_extend(uint8_t byte){
    int32_t have_sign_byte = *((int8_t*)&byte);
    uint32_t result = *((uint32_t*)&have_sign_byte);
    return result;
}
//sign extend :2 bytes
uint32_t bytes_sign_extend(uint16_t bytes){
    int32_t have_sign_bytes = *((int16_t*)&bytes);
    uint32_t result = *((uint32_t*)&have_sign_bytes);
    return result;
}


void process_instruction()
{
    /* execute one instruction here. You should use CURRENT_STATE and modify
     * values in NEXT_STATE. You can call mem_read_32() and mem_write_32() to
     * access memory. */
    
    //turn next instructions,if is J or Jal,PC will update again
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
    //get op
    uint32_t curr_inst = mem_read_32(CURRENT_STATE.PC);
    uint32_t op = curr_inst >> 26;
    //First Judge: is R?
    //R instructions
    if (op == 0){
        //get rs,rt,rd,sa,func
        uint32_t rs = curr_inst >>21 & 0b11111;
        uint32_t rt = curr_inst >>16 & 0b11111;
        uint32_t rd = curr_inst >>11 & 0b11111;
        uint32_t sa = curr_inst >> 6 & 0b11111;
        uint32_t func = curr_inst & 0b111111;
        switch (func) {
            case 0b000000: {
                // SLL
                NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] <<sa;
                break;
            }
            case 0b000010: {
                // SRL
                NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >>sa;
                break;
            }
            case 0b000011: {
                // SRA
                NEXT_STATE.REGS[rd] = *((int32_t*)&CURRENT_STATE.REGS[rt]) >> sa;
                break;
            }
            case 0b000100: {
                // SLLV
                uint32_t shamt = CURRENT_STATE.REGS[rs] & 0x1f;
                NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << shamt;
                break;
            }
            case 0b000110: {
                // SRLV
                uint32_t shamt = CURRENT_STATE.REGS[rs] & 0x1f;
                NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> shamt;
                break;
            }
            case 0b000111: {
                // SRAV
                uint32_t shamt = CURRENT_STATE.REGS[rs] & 0x1f;
                NEXT_STATE.REGS[rd] = *((int32_t*)&CURRENT_STATE.REGS[rt]) >> shamt;
                break;
            }
            case 0b001000: {
                // JR
                NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
                break;
            }
            case 0b001001: {
                // JALR
                NEXT_STATE.REGS[rd] = CURRENT_STATE.PC + 4;
                NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
                break;
            }
            case 0b100000: {
                // ADD
                NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] + CURRENT_STATE.REGS[rs];
                break;
            }
            case 0b100001: {
                // ADDU
                NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] + CURRENT_STATE.REGS[rs];
                break;
            }
            case 0b100010: {
                // SUB
                NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] - CURRENT_STATE.REGS[rs];
                break;
            }
            case 0b100011: {
                // SUBU
                NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] - CURRENT_STATE.REGS[rs];
                break;
            }
            case 0b100100: {
                // AND
                NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] & CURRENT_STATE.REGS[rs];
                break;
            }
            case 0b100101: {
                // OR
                NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] | CURRENT_STATE.REGS[rs];
                break;
            }
            case 0b100110: {
                // XOR
                NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] ^ CURRENT_STATE.REGS[rt];
                break;
            }
            case 0b100111: {
                // NOR
                NEXT_STATE.REGS[rd] = ~(CURRENT_STATE.REGS[rt] | CURRENT_STATE.REGS[rs]);
                break;
            }
            case 0b101010: {
                // SLT
                int32_t sign_rs = CURRENT_STATE.REGS[rs] >> 31;
                int32_t sign_rt = CURRENT_STATE.REGS[rt] >> 31;
                int32_t value_rs = CURRENT_STATE.REGS[rs] &0x7FFFFFFF;
                int32_t value_rt = CURRENT_STATE.REGS[rt] &0x7FFFFFFF;
                if (sign_rs == sign_rt && sign_rs == 1){
                    NEXT_STATE.REGS[rd] = (value_rt < value_rs) ? 1 : 0;
                }
                else if (sign_rs == sign_rt && sign_rs == 0){
                    NEXT_STATE.REGS[rd] = (value_rt > value_rs) ? 1 : 0;
                }
                else if (sign_rs != sign_rt && sign_rs == 1){
                    NEXT_STATE.REGS[rd] = 1;
                }
                else{
                    NEXT_STATE.REGS[rd] = 0;
                }
                break;
            }
            case 0b101011: {
                // SLTU
                NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] < CURRENT_STATE.REGS[rt] ? 1 : 0;
                break;
            }
            case 0b011000: {
                // MULT
                int64_t value_rs = *((int32_t*)&CURRENT_STATE.REGS[rs]);
                int64_t value_rt = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                int64_t answer = value_rs * value_rt;
                uint64_t uint_answer = *((uint32_t*)&(answer));
                NEXT_STATE.HI = (uint32_t)((uint_answer >> 32) & 0xffffffff);
                NEXT_STATE.LO = (uint32_t)(uint_answer & 0xffffffff);
                break;
            }
            case 0b010000: {
                // MFHI
                NEXT_STATE.REGS[rd] = CURRENT_STATE.HI;
                break;
            }
            case 0b010010: {
                // MFLO
                NEXT_STATE.REGS[rd] = CURRENT_STATE.LO;
                break;
            }
            case 0b010001: {
                // MTHI
                NEXT_STATE.HI = CURRENT_STATE.REGS[rs];
                break;
            }
            case 0b010011: {
                // MTLO
                NEXT_STATE.LO = CURRENT_STATE.REGS[rs];
                break;
            }
            case 0b011001: {
                // MULTU
                uint64_t answer = CURRENT_STATE.REGS[rs] * CURRENT_STATE.REGS[rt];
                NEXT_STATE.HI = (answer >> 32) & 0xffffffff;
                NEXT_STATE.LO = answer & 0xffffffff;
                break;
            }
            case 0b011010: {
                // DIV
                int64_t value_rs = *((int32_t*)&CURRENT_STATE.REGS[rs]);
                int64_t value_rt = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                NEXT_STATE.HI = value_rs % value_rt;
                NEXT_STATE.LO = value_rs / value_rt;
                break;
            }
            case 0b011011: {
                // DIVU
                NEXT_STATE.HI = CURRENT_STATE.REGS[rs] % CURRENT_STATE.REGS[rt];
                NEXT_STATE.LO = CURRENT_STATE.REGS[rs] / CURRENT_STATE.REGS[rt];
                break;
            }
            case 0b001100: {
                // SYSCALL
                if (CURRENT_STATE.REGS[2] == 0x0a) {
                    RUN_BIT = 0;
                }
                break;
            }
        }
    }
    //I&J instructions
    else {
        //get rs,rt,imm
        uint32_t rs = curr_inst >>21 & 0b11111;
        uint32_t rt = curr_inst >>16 & 0b11111;
        uint32_t imm = curr_inst & 0xFFFF;
        switch (op) {
            case 0b000010: {
                // J
                uint32_t offset = curr_inst & 0x3ffffff;
                NEXT_STATE.PC = (CURRENT_STATE.PC & 0xf0000000) | (offset << 2);
                break;
            }
            case 0b000011: {
                // JAL
                uint32_t offset = curr_inst & 0x3ffffff;
                NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
                NEXT_STATE.PC = (CURRENT_STATE.PC & 0xf0000000) | (offset << 2);
                break;
            }
            case 0b000100: {
                // BEQ
                uint32_t offset = imm_sign_extend(imm) << 2;
                if (CURRENT_STATE.REGS[rs] == CURRENT_STATE.REGS[rt]) {
                    NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                }
                break;
            }
            case 0b000101: {
                // BNE
                uint32_t offset = imm_sign_extend(imm) << 2;
                if (CURRENT_STATE.REGS[rs] != CURRENT_STATE.REGS[rt]) {
                    NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                }
                break;
            }
            case 0b000110: {
                // BLEZ
                uint32_t offset = imm_sign_extend(imm) << 2;
                if ((CURRENT_STATE.REGS[rs] & 0x80000000) != 0 ||
                    CURRENT_STATE.REGS[rs] == 0) {
                    NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                } 
            }
            case 0b000111: {
                // BGTZ
                uint32_t offset = imm_sign_extend(imm) << 2;
                if ((CURRENT_STATE.REGS[rs] & 0x80000000) != 0 ||
                    CURRENT_STATE.REGS[rs] == 0) {
                    NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                }
            }
            case 0b001000: {
                // ADDI
                NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + imm_sign_extend(imm);
                break;
            }
            case 0b001001: {
                // ADDIU
                NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + imm_sign_extend(imm);
                break;
            }
            case 0b001010: {
                // SLTI
                NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] < imm_sign_extend(imm) ? 1 : 0;
                break;
            }
            case 0b001011: {
                // SLTIU
                NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] < imm_sign_extend(imm) ? 1 : 0;
                break;
            }
            case 0b001100: {
                // ANDI
                NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] & imm;
                break;
            }
            case 0b001101: {
                // ORI
                NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] | imm;
                break;
            }
            case 0b001110: {
                // XORI
                NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] ^ imm;
                break;
            }
            case 0b001111: {
                // LUI
                NEXT_STATE.REGS[rt]  = (CURRENT_STATE.REGS[rt] & 0x0000ffff) | (imm << 16);
                printf("%08x\n",NEXT_STATE.REGS[rt]);
                break;
            }
            case 0b100000: {
                // LB
                uint32_t addr = imm_sign_extend(imm) + CURRENT_STATE.REGS[rs];
                uint8_t byte = mem_read_32(addr) & 0xff;
                NEXT_STATE.REGS[rt] = byte_sign_extend(byte);
                break;
            }
            case 0b100001: {
                // LH
                uint32_t addr = imm_sign_extend(imm) + CURRENT_STATE.REGS[rs];
                uint16_t bytes = mem_read_32(addr) & 0xffff;
                NEXT_STATE.REGS[rt] = bytes_sign_extend(bytes);
                break;
            }
            case 0b100011: {
                // LW
                uint32_t addr = imm_sign_extend(imm) + CURRENT_STATE.REGS[rs];
                NEXT_STATE.REGS[rt] = mem_read_32(addr);
                break;
            }
            case 0b100100: {
                // LBU
                uint32_t addr = imm_sign_extend(imm) + CURRENT_STATE.REGS[rs];
                uint8_t byte = mem_read_32(addr) & 0xff;
                NEXT_STATE.REGS[rt] = byte;
                break;
            }
            case 0b100101: {
                // LHU
                uint32_t addr = imm_sign_extend(imm) + CURRENT_STATE.REGS[rs];
                uint16_t bytes = mem_read_32(addr) & 0xffff;
                NEXT_STATE.REGS[rt] = bytes;
                break;
            }
            case 0b101000: {
                // SB
                uint32_t addr = imm_sign_extend(imm) + CURRENT_STATE.REGS[rs];
                uint32_t val = (mem_read_32(addr) & 0xffffff00) | (CURRENT_STATE.REGS[rt] & 0xff);
                mem_write_32(addr, val);
                break;
            }
            case 0b101001: {
                // SH
                uint32_t addr = imm_sign_extend(imm) + CURRENT_STATE.REGS[rs];
                uint32_t val = (mem_read_32(addr) & 0xffff0000) | (CURRENT_STATE.REGS[rt] & 0xffff);
                mem_write_32(addr, val);
                break;
            }
            case 0b101011: {
                // SW
                uint32_t addr = imm_sign_extend(imm) + CURRENT_STATE.REGS[rs];
                uint32_t val = CURRENT_STATE.REGS[rt];
                mem_write_32(addr, val);
                break;
            }
            case 0b000001: {
                uint32_t offset = imm_sign_extend(imm) << 2;
                switch (rt) {
                    case 0b00000: {
                        // BLTZ
                        if ((CURRENT_STATE.REGS[rs] & 0x80000000) != 0) {
                            NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                        }
                        break;
                    }
                    case 0b00001: {
                        // BGEZ
                        if ((CURRENT_STATE.REGS[rs] & 0x80000000) == 0) {
                            NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                        }
                        
                        break;
                    }
                    case 0b10000: {
                        // BLTZAL
                        NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
                        if ((CURRENT_STATE.REGS[rs] & 0x80000000) != 0) {
                            NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                        }
                        break;
                    }
                    case 0b10001: {
                        // BGEZAL
                        NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
                        if ((CURRENT_STATE.REGS[rs] & 0x80000000) == 0) {
                            NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                        }
                        break;
                    }
                }
            }
            default:{
                printf("illegal instruction!");
                break;
            }
        }
    }
    printf("reg: 0x%08x\n", CURRENT_STATE.REGS[5]);
}
