#!/usr/bin/python
from __future__ import print_function
from itertools import repeat
from ml import *


def signExtend(fromBits, toBits, val):
    signBit = extractBits(val, fromBits - 1, 1)
    if signBit:
        fromBitsMask = (1 << fromBits) - 1
        toBitsMask = (1 << toBits) - 1
        newBitsMask = toBitsMask & ~fromBitsMask
        val |= newBitsMask

    return val

def asSigned(bits, val):
    signBit = extractBits(val, bits - 1, 1)
    if signBit:
        maxintPlus1 = 1 << bits
        val -= maxintPlus1

    return val


class EmulatorBreak(Exception): pass

class ZionEmulator(object):
    def __init__(self, memorySize=2048):
        self.memorySize = memorySize
        self.mem = [0] * self.memorySize

        self.clearState()

    def clearState(self):
        self.regs = [0] * NUM_REGISTERS

        # clear memory
        self.setMem()

        self.pc = 0

    def getMemWord(self, addr):
        assert addr % 2 == 0
        return (self.mem[addr] << 8) | self.mem[addr + 1]

    def setMemWord(self, addr, val):
        assert addr % 2 == 0
        assert 0x0000 <= val <= 0xffff
        self.mem[addr]     = val >> 8
        self.mem[addr + 1] = val & 0xff 

    def setMem(self, *words):
        addr = 0

        for i, w in enumerate(words):
            addr = i * 2
            self.setMemWord(addr, w)

        # fill rest of memory with break instructions
        self.mem[addr + 2:] = repeat(0xcc, self.memorySize - addr)

    def run(self):
        try:
            while True:
                self.step()
        except EmulatorBreak:
            return

    def step(self):
        if self.pc >= self.memorySize:
            raise StandardError("$pc out of bounds!")

        word = self.getMemWord(self.pc)
        self.pc += 2

        self.execute(word)

    def execute(self, word):
        name, operands = decodeML(word)
        # print(name, ', '.join(map(hex, operands)))

        methodName = 'do_' + name
        method = getattr(self, methodName)

        method(*operands)

    def do_add(self, rd, rs, rt):
        if rd == 0:
            return

        self.regs[rd] = self.regs[rs] + self.regs[rt]
        self.regs[rd] &= 0xffff

    def do_sub(self, rd, rs, rt):
        if rd == 0:
            return

        self.regs[rd] = self.regs[rs] - self.regs[rt]
        self.regs[rd] &= 0xffff

    def do_slt(self, rd, rs, rt):
        if rd == 0:
            return

        rsVal = asSigned(16, self.regs[rs])
        rtVal = asSigned(16, self.regs[rt])
        self.regs[rd] = 1 if (rsVal < rtVal) else 0

    def do_sltu(self, rd, rs, rt):
        if rd == 0:
            return

        rsVal = self.regs[rs]
        rtVal = self.regs[rt]
        self.regs[rd] = 1 if (rsVal < rtVal) else 0

    @staticmethod
    def _getRegOffsetAddr(rs, ofs):
        addr = self.regs[rs]
        assert addr % 2 == 0, "unaligned memory access"
        addr += asSigned(4, ofs)
        addr &= 0xffff
        return addr

    def do_lb(self, rd, rs, ofs):
        if rd == 0:
            return

        self.regs[rd] = self.mem[self._getRegOffsetAddr(rs, ofs)]

    def do_sb(self, rd, rs, ofs):
        self.mem[self._getRegOffsetAddr(rs, ofs)] = self.regs[rd]

    def do_lw(self, rd, rs, ofs):
        if rd == 0:
            return

        self.regs[rd] = self.getMemWord(self._getRegOffsetAddr(rs, ofs))

    def do_sw(self, rd, rs, ofs):
        self.setMemWord(self._getRegOffsetAddr(rs, ofs), self.regs[rd])

    def do_li8(self, rd, imm):
        self.regs[rd] = imm

    def do_lui(self, rd, imm):
        self.regs[rd] = imm << 8
        self.regs[rd] &= 0xffff

    def do_addi(self, rd, imm):
        self.regs[rd] += signExtend(8, 16, imm)
        self.regs[rd] &= 0xffff

    def do_ori(self, rd, imm):
        self.regs[rd] |= imm

    def _relJmp(self, bits, ofs):
        """do a relative jump with a signed offset width given bit width"""
        ofs = asSigned(bits, ofs)
        ofs *= 2
        self.pc += ofs

    def _link(self):
        """do "and link" part of bal/jal"""
        # note $pc has already been incremented
        self.regs[NICE_REGISTER_NAMES['$ra']] = self.pc

    def do_b(self, ofs):
        self._relJmp(11, ofs)

    def do_bal(self, ofs):
        self.do_b(ofs)
        self._link()

    def do_beqz(self, reg, ofs):
        if self.regs[reg] == 0:
            self._relJmp(7, ofs)

    def do_bnez(self, reg, ofs):
        if self.regs[reg] != 0:
            self._relJmp(7, ofs)

    def do_sll(self, rs, rt):
        self.regs[rs] <<= self.regs[rt]
        self.regs[rs] &= 0xffff

    def do_srl(self, rs, rt):
        self.regs[rs] >>= self.regs[rt]
        # self.regs[reg] &= 0xffff

    def do_slli(self, rs, imm):
        self.regs[rs] <<= imm
        self.regs[rs] &= 0xffff

    def do_srli(self, rs, imm):
        self.regs[rs] >>= imm
        # self.regs[reg] &= 0xffff

    def do_and(self, rs, rt):
        self.regs[rs] &= self.regs[rt]

    def do_or(self, rs, rt):
        self.regs[rs] |= self.regs[rt]

    def do_nor(self, rs, rt):
        self.regs[rs] |= self.regs[rt]
        self.regs[rs] = ~self.regs[rs] & 0xffff

    def do_xor(self, rs, rt):
        self.regs[rs] ^= self.regs[rt]

    def do_jr(self, reg):
        self.pc = self.regs[reg]

    def do_jalr(self, reg):
        self.do_jr(reg)
        self._link()

    def do_exts(self, rs, rt):
        self.regs[rs] = signExtend(8, 16, self.regs[rt])

    def do_break(self):
        raise EmulatorBreak



if __name__ == '__main__':
    emu = ZionEmulator()

    # calculate 12th(?) fibonacci number
    emu.setMem(
        0x8101, # li8 1, 1
        0x8201, # li8 2, 1
        0x8300, # li8 3, 0
        0x840a, # li8 4, 10
        # loop:
        0x0512, # add 5,1,2
        0x0102, # add 1,0,2 (move 1,2)
        0x0205, # add 2,0,5 (move 2,5)
        0x9301, # addi 3, 1
        0x3534, # sltu 5,3,4
        0xbafa, # bnez 5,-6
        )
    emu.run()

    print(emu.regs)
