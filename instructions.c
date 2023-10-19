#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#include "cpu.h"
#include "instructions.h"

static void XXX(CPU *cpu, addr_mode mode) {
	fprintf(stderr, "Invalid opcode $%x encountered at address $%.4x.\n", cpu->read(cpu->context, cpu->pc - 1), cpu->pc - 1);
	exit(1);
}

static inline void add(CPU *cpu, uint8_t op) {
	cpu->p &= ~Z & ~N;

	// check whether the low nibble needs adjustment beforehand
	uint8_t hcl = cpu->p & D && (cpu->a & 0x0F) + (op & 0x0F) + (!!(cpu->p & C)) > 0x09; 
	asm inline (
		"	movzx %[p], %%ax\n"
		"	bt $0, %%ax\n"
		"# clear the carry flag on emulated cpu after setting on x86\n"
		"	jnc adc_no_carry%=\n"
		"	and 0xfe, %[p]\n"
		"	stc\n"
		"adc_no_carry%=:\n"
		"	adc %[op], %[a]\n"
		"	jc adc_carry%=\n"
		"	jz adc_zero%=\n"
		"adc_carry_ret%=:\n"
		"adc_zero_ret%=:\n"
		"	bt $3, %%ax\n"
		"	jnc adc_no_hch%=\n"
		"	test %[hcl], %[hcl]\n"
		"	jz adc_no_hcl%=\n"
		"	add $0x06, %[a]\n"
		"adc_no_hcl%=:\n"
		"	cmp $0x9F, %[a]\n"
		"	jng adc_no_hch%=\n"
		"	add $0x60, %[a]\n"
		"	bts $0, %%ax\n"
		"adc_no_hch%=:\n"
		"	jo adc_overflow%=\n"
		"	jng adc_sign%=\n"
		"	jmp adc_done%=\n"
		"adc_carry%=:\n"
		"	bts $0, %%ax\n"
		"	jnz adc_carry_ret%=\n"
		"adc_zero%=:\n"
		"	bts $1, %%ax\n"
		"	jmp adc_zero_ret%=\n"
		"adc_overflow%=:\n"
		"	bts $6, %%ax\n"
		"	jns adc_done%=\n"
		"adc_sign%=:\n"
		"	bts $7, %%ax\n"
		"adc_done%=:\n"
		"	mov %%al, %[p]"
		: [a] "+rm" (cpu->a), [p] "+rm" (cpu->p)
		: [op] "rm" (op), [hcl] "r" (hcl)
		: "ax", "cc"
	);
}

static void ADC(CPU *cpu, addr_mode mode) {
	add(cpu, load(cpu, mode));
}

static void AND(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;
	cpu->a &= load(cpu, mode);
	if (!cpu->a) cpu->p |= Z;
	if (cpu->a < 0) cpu->p |= N;
}

static void ASL(CPU *cpu, addr_mode mode) {
	cpu->p &= ~C & ~Z & ~N;

	uint8_t op;
	uint16_t addr;
	if (mode == addr_acc) {
		op = cpu->a;
	} else {
		addr = mode(cpu);
		op = cpu->read(cpu->context, addr);
		++cpu->cycles;
	}

	if (op & 0x80) cpu->p |= C;
	op <<= 1;
	if (!op) cpu->p |= Z;
	if (op & 0x80) cpu->p |= N;

	if (mode == addr_acc) {
		cpu->a = op;
	} else {
		store(cpu, addr, op);
	}

	++cpu->cycles;
}

static inline void branch(CPU *cpu, int8_t offset) {
	if ((cpu->pc & 0xFF) + offset > 0xFF) ++cpu->cycles;
	cpu->pc += offset;
	++cpu->cycles;
}

static void BCC(CPU *cpu, addr_mode mode) {
	// still calculating an offset in here since pc still needs to advance
	int8_t offset = load(cpu, addr_imm);
	if (!(cpu->p & C)) branch(cpu, offset);
}

static void BCS(CPU *cpu, addr_mode mode) {
	int8_t offset = load(cpu, addr_imm);
	if (cpu->p & C) branch(cpu, offset);
}

static void BEQ(CPU *cpu, addr_mode mode) {
	int8_t offset = load(cpu, addr_imm);
	if (cpu->p & Z) branch(cpu, offset);
}

static void BIT(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z;

	uint8_t op = load(cpu, mode);
	if (!(cpu->a && op)) cpu->p |= Z;
	cpu->p |= op & 0xC0; // set overflow and sign flag to bits 6 and 7 of value
}

static void BMI(CPU *cpu, addr_mode mode) {
	int8_t offset = load(cpu, addr_imm);
	if (cpu->p & N) branch(cpu, offset);
}

static void BNE(CPU *cpu, addr_mode mode) {
	int8_t offset = load(cpu, addr_imm);
	if (!(cpu->p & Z)) branch(cpu, offset);
}

static void BPL(CPU *cpu, addr_mode mode) {
	int8_t offset = load(cpu, addr_imm);
	if (!(cpu->p & N)) branch(cpu, offset);
}

static void BRK(CPU *cpu, addr_mode mode) {
	cpu->p |= B;

	++cpu->cycles;
	interrupt(cpu);
	cpu->pc = 0xFFFE;
	cpu->pc = addr_abs(cpu);
}

static void BVC(CPU *cpu, addr_mode mode) {
	int8_t offset = load(cpu, addr_imm);
	if (!(cpu->p & V)) branch(cpu, offset);
}
static void BVS(CPU *cpu, addr_mode mode) {
	int8_t offset = load(cpu, addr_imm);
	if (cpu->p & V) branch(cpu, offset);
}

static void CLC(CPU *cpu, addr_mode mode) {
	cpu->p &= ~C;
	++cpu->cycles;
}

static void CLD(CPU *cpu, addr_mode mode) {
	cpu->p &= ~D;
	++cpu->cycles;
}

static void CLI(CPU *cpu, addr_mode mode) {
	cpu->p &= ~I;
	++cpu->cycles;
}

static void CLV(CPU *cpu, addr_mode mode) {
	cpu->p &= ~V;
	++cpu->cycles;
}

static inline void compare(CPU *cpu, addr_mode mode, uint8_t value) {
	cpu->p &= ~C & ~Z & ~N;
	int8_t result = value - load(cpu, mode);
	if (result >= 0) cpu->p |= C;
	if (!result) cpu->p |= Z;
	if (result < 0) cpu->p |= N;
}

static void CMP(CPU *cpu, addr_mode mode) {
	compare(cpu, mode, cpu->a);
}

static void CPX(CPU *cpu, addr_mode mode) {
	compare(cpu, mode, cpu->x);
}

static void CPY(CPU *cpu, addr_mode mode) {
	compare(cpu, mode, cpu->y);
}

static void decrement(CPU *cpu, uint8_t *value) {
	cpu->p &= ~Z & ~N;
	--*value;
	if (!*value) cpu->p |= Z;
	if (*value < 0) cpu->p |= N;
}

static void DEC(CPU *cpu, addr_mode mode) {
	uint8_t addr = mode(cpu);
	uint8_t op = cpu->read(cpu->context, addr);
	++cpu->cycles;

	decrement(cpu, &op);
	store(cpu, addr, op);
}

static void DEX(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	decrement(cpu, &cpu->x);
}
static void DEY(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	decrement(cpu, &cpu->y);
}

static void EOR(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;
	cpu->a ^= load(cpu, mode);
	if (!cpu->a) cpu->p |= Z;
	if (cpu->a < 0) cpu->p |= N;
}

static void increment(CPU *cpu, uint8_t *value) {
	cpu->p &= ~Z & ~N;
	++*value;
	if (!*value) cpu->p |= Z;
	if (*value < 0) cpu->p |= N;
}

static void INC(CPU *cpu, addr_mode mode) {
	uint8_t addr = mode(cpu);
	uint8_t op = cpu->read(cpu->context, addr);
	++cpu->cycles;

	increment(cpu, &op);
	store(cpu, addr, op);
}

static void INX(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	increment(cpu, &cpu->x);
}

static void INY(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	increment(cpu, &cpu->y);
}

static void JMP(CPU *cpu, addr_mode mode) {
	cpu->pc = mode(cpu);
}

static void JSR(CPU *cpu, addr_mode mode) {
	uint16_t addr = mode(cpu);
	uint16_t ret = cpu->pc - 1;
	++cpu->cycles;
	stack_push(cpu, ret >> 8);
	stack_push(cpu, ret & 0xFF);
	cpu->pc = addr;
}

static void LDA(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;

	cpu->a = load(cpu, mode);
	if (!cpu->a) cpu->p |= Z;
	if (cpu->a < 0) cpu->p |= N;
}

static void LDX(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;

	cpu->x = load(cpu, mode);
	if (!cpu->x) cpu->p |= Z;
	if (cpu->x < 0) cpu->p |= N;
}

static void LDY(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;

	cpu->y = load(cpu, mode);
	if (!cpu->y) cpu->p |= Z;
	if (cpu->y < 0) cpu->p |= N;
}

static void LSR(CPU *cpu, addr_mode mode) {
	cpu->p &= ~C & ~Z & ~N;

	uint8_t op;
	uint16_t addr;
	if (mode == addr_acc) {
		op = cpu->a;
	} else {
		addr = mode(cpu);
		op = cpu->read(cpu->context, addr);
		++cpu->cycles;
	}

	if (op & 0x80) cpu->p |= C;
	op >>= 1;
	if (!op) cpu->p |= Z;
	if (op & 0x80) cpu->p |= N;

	if (mode == addr_acc) {
		cpu->a = op;
	} else {
		store(cpu, addr, op);
	}
	
	++cpu->cycles;
}

static void NOP(CPU *cpu, addr_mode mode) {
	++cpu->cycles;
}

static void ORA(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;
	cpu->a |= load(cpu, mode);
	if (!cpu->a) cpu->p |= Z;
	if (cpu->a < 0) cpu->p |= N;
}

static void PHA(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	stack_push(cpu, cpu->a);
}

static void PHP(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	stack_push(cpu, cpu->p);
}

static void PLA(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;

	load(cpu, mode);
	cpu->a = stack_pull(cpu);
	++cpu->cycles;
	if (!cpu->a) cpu->p |= Z;
	if (cpu->a & (1 << 7)) cpu->p |= N;
}

static void PLP(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	cpu->p = stack_pull(cpu);
	++cpu->cycles;
}

static void ROL(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;

	uint8_t op;
	uint16_t addr;
	if (mode == addr_acc) {
		op = cpu->a;
	} else {
		addr = mode(cpu);
		op = cpu->read(cpu->context, addr);
		++cpu->cycles;
	}

	asm inline (
		"	movzx %[p], %%ax\n"
		"	bt $0, %%ax\n"
		"# clear the carry flag on emulated cpu after setting on x86\n"
		"	jnc rol_no_carry%=\n"
		"	and 0xfe, %[p]\n"
		"	stc\n"
		"rol_no_carry%=:\n"
		"	rcl $1, %[op]\n"
		"	jc rol_carry%=\n"
		"	jz rol_zero%=\n"
		"rol_carry_ret%=:\n"
		"	jng rol_sign%=\n"
		"	jmp rol_done%=\n"
		"rol_carry%=:\n"
		"	bts $0, %%ax\n"
		"	jnz rol_carry_ret%=\n"
		"rol_zero%=:\n"
		"	bts $1, %%ax\n"
		"	jmp rol_done%=\n"
		"rol_sign%=:\n"
		"	bts $7, %%ax\n"
		"rol_done%=:\n"
		"	mov %%al, %[p]"
		: [op] "+rm" (op), [p] "+rm" (cpu->p)
		: // no input operands
		: "ax", "cc"
	);

	if (mode == addr_acc) {
		cpu->a = op;
	} else {
		store(cpu, addr, op);
	}

	++cpu->cycles;
}

static void ROR(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;

	uint8_t op;
	uint16_t addr;
	if (mode == addr_acc) {
		op = cpu->a;
	} else {
		addr = mode(cpu);
		op = cpu->read(cpu->context, addr);
		++cpu->cycles;
	}

	asm inline (
		"	movzx %[p], %%ax\n"
		"	bt $0, %%ax\n"
		"# clear the carry flag on emulated cpu after setting on x86\n"
		"	jnc ror_no_carry%=\n"
		"	and 0xfe, %[p]\n"
		"	stc\n"
		"ror_no_carry%=:\n"
		"	rcr $1, %[op]\n"
		"	jc ror_carry%=\n"
		"	jz ror_zero%=\n"
		"ror_carry_ret%=:\n"
		"	jng ror_sign%=\n"
		"	jmp ror_done%=\n"
		"ror_carry%=:\n"
		"	bts $0, %%ax\n"
		"	jnz ror_carry_ret%=\n"
		"ror_zero%=:\n"
		"	bts $1, %%ax\n"
		"	jmp ror_done%=\n"
		"ror_sign%=:\n"
		"	bts $7, %%ax\n"
		"ror_done%=:\n"
		"	mov %%al, %[p]"
		: [op] "+rm" (op), [p] "+rm" (cpu->p)
		: // no input operands
		: "ax", "cc"
	);

	if (mode == addr_acc) {
		cpu->a = op;
	} else {
		store(cpu, addr, op);
	}
	
	++cpu->cycles;
}

static void RTI(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	cpu->p = stack_pull(cpu);
	++cpu->cycles;
	cpu->pc = stack_pull(cpu);
	cpu->pc |= stack_pull(cpu) << 8;
}

static void RTS(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	cpu->cycles += 2;
	cpu->pc = stack_pull(cpu);
	cpu->pc |= stack_pull(cpu) << 8;
}

static void SBC(CPU *cpu, addr_mode mode) {
	add(cpu, ~load(cpu, mode));
}

static void SEC(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	cpu->p |= C;
}

static void SED(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	cpu->p |= D;
}

static void SEI(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	cpu->p |= I;
}

static void STA(CPU *cpu, addr_mode mode) {
	uint8_t addr = mode(cpu);
	store(cpu, addr, cpu->a);
}

static void STX(CPU *cpu, addr_mode mode) {
	uint8_t addr = mode(cpu);
	store(cpu, addr, cpu->x);
}

static void STY(CPU *cpu, addr_mode mode) {
	uint8_t addr = mode(cpu);
	store(cpu, addr, cpu->y);
}

static void TAX(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;

	load(cpu, mode);
	cpu->x = cpu->a;
	if (!cpu->x) cpu->p |= Z;
	if (cpu->x & (1 << 7)) cpu->p |= N;
}

static void TAY(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;

	load(cpu, mode);
	cpu->y = cpu->a;
	if (!cpu->y) cpu->p |= Z;
	if (cpu->y & (1 << 7)) cpu->p |= N;
}

static void TSX(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;

	load(cpu, mode);
	cpu->x = cpu->s;
	if (!cpu->x) cpu->p |= Z;
	if (cpu->x & (1 << 7)) cpu->p |= N;
}

static void TXA(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;

	load(cpu, mode);
	cpu->a = cpu->x;
	if (!cpu->a) cpu->p |= Z;
	if (cpu->a & (1 << 7)) cpu->p |= N;
}

static void TXS(CPU *cpu, addr_mode mode) {
	load(cpu, mode);
	cpu->s = cpu->x;
}

static void TYA(CPU *cpu, addr_mode mode) {
	cpu->p &= ~Z & ~N;

	load(cpu, mode);
	cpu->a = cpu->y;
	if (!cpu->a) cpu->p |= Z;
	if (cpu->a & (1 << 7)) cpu->p |= N;
}

const opcode_t OPCODE_TABLE[256] = {
	{BRK, addr_imp}, {ORA, addr_inx}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {ORA, addr_zpg}, {ASL, addr_zpg}, {XXX, addr_imp},
	{PHP, addr_imp}, {ORA, addr_imm}, {ASL, addr_acc}, {XXX, addr_imp},
	{XXX, addr_imp}, {ORA, addr_abs}, {ASL, addr_abs}, {XXX, addr_imp},
	{BPL, addr_rel}, {ORA, addr_iny}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {ORA, addr_zpx}, {ASL, addr_zpx}, {XXX, addr_imp},
	{CLC, addr_imp}, {ORA, addr_aby}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {ORA, addr_abx}, {ASL, addr_abx}, {XXX, addr_imp},
	{JSR, addr_abs}, {AND, addr_inx}, {XXX, addr_imp}, {XXX, addr_imp},
	{BIT, addr_zpg}, {AND, addr_zpg}, {ROL, addr_zpg}, {XXX, addr_imp},
	{PLP, addr_imp}, {AND, addr_imm}, {ROL, addr_acc}, {XXX, addr_imp},
	{BIT, addr_abs}, {AND, addr_abs}, {ROL, addr_abs}, {XXX, addr_imp},
	{BMI, addr_rel}, {AND, addr_iny}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {AND, addr_zpx}, {ROL, addr_zpx}, {XXX, addr_imp},
	{SEC, addr_imp}, {AND, addr_aby}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {AND, addr_abx}, {ROL, addr_abx}, {XXX, addr_imp},
	{RTI, addr_imp}, {EOR, addr_inx}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {EOR, addr_zpg}, {LSR, addr_zpg}, {XXX, addr_imp},
	{PHA, addr_imp}, {EOR, addr_imm}, {LSR, addr_acc}, {XXX, addr_imp},
	{JMP, addr_abs}, {EOR, addr_abs}, {LSR, addr_abs}, {XXX, addr_imp},
	{BVC, addr_rel}, {EOR, addr_iny}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {EOR, addr_zpx}, {LSR, addr_zpx}, {XXX, addr_imp},
	{CLI, addr_imp}, {EOR, addr_aby}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {EOR, addr_abx}, {LSR, addr_abx}, {XXX, addr_imp},
	{RTS, addr_imp}, {ADC, addr_inx}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {ADC, addr_zpg}, {ROR, addr_zpg}, {XXX, addr_imp},
	{PLA, addr_imp}, {ADC, addr_imm}, {ROR, addr_acc}, {XXX, addr_imp},
	{JMP, addr_abi}, {ADC, addr_abs}, {ROR, addr_abs}, {XXX, addr_imp},
	{BVS, addr_rel}, {ADC, addr_iny}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {ADC, addr_zpx}, {ROR, addr_zpx}, {XXX, addr_imp},
	{SEI, addr_imp}, {ADC, addr_aby}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {ADC, addr_abx}, {ROR, addr_abx}, {XXX, addr_imp},
	{XXX, addr_imp}, {STA, addr_inx}, {XXX, addr_imp}, {XXX, addr_imp},
	{STY, addr_zpg}, {STA, addr_zpg}, {STX, addr_zpg}, {XXX, addr_imp},
	{DEY, addr_imp}, {XXX, addr_imp}, {TXA, addr_imp}, {XXX, addr_imp},
	{STY, addr_abs}, {STA, addr_abs}, {STX, addr_abs}, {XXX, addr_imp},
	{BCC, addr_rel}, {STA, addr_iny}, {XXX, addr_imp}, {XXX, addr_imp},
	{STY, addr_zpx}, {STA, addr_zpx}, {STX, addr_zpy}, {XXX, addr_imp},
	{TYA, addr_imp}, {STA, addr_aby}, {TXS, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {STA, addr_abx}, {XXX, addr_imp}, {XXX, addr_imp},
	{LDY, addr_imm}, {LDA, addr_inx}, {LDX, addr_imm}, {XXX, addr_imp},
	{LDY, addr_zpg}, {LDA, addr_zpg}, {LDX, addr_zpg}, {XXX, addr_imp},
	{TAY, addr_imp}, {LDA, addr_imm}, {TAX, addr_imp}, {XXX, addr_imp},
	{LDY, addr_abs}, {LDA, addr_abs}, {LDX, addr_abs}, {XXX, addr_imp},
	{BCS, addr_rel}, {LDA, addr_iny}, {XXX, addr_imp}, {XXX, addr_imp},
	{LDY, addr_zpx}, {LDA, addr_zpx}, {LDX, addr_zpy}, {XXX, addr_imp},
	{CLV, addr_imp}, {LDA, addr_aby}, {TSX, addr_imp}, {XXX, addr_imp},
	{LDY, addr_abx}, {LDA, addr_abx}, {LDX, addr_aby}, {XXX, addr_imp},
	{CPY, addr_imm}, {CMP, addr_inx}, {XXX, addr_imp}, {XXX, addr_imp},
	{CPY, addr_zpg}, {CMP, addr_zpg}, {DEC, addr_zpg}, {XXX, addr_imp},
	{INY, addr_imp}, {CMP, addr_imm}, {DEX, addr_imp}, {XXX, addr_imp},
	{CPY, addr_abs}, {CMP, addr_abs}, {DEC, addr_abs}, {XXX, addr_imp},
	{BNE, addr_rel}, {CMP, addr_iny}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {CMP, addr_zpx}, {DEC, addr_zpx}, {XXX, addr_imp},
	{CLD, addr_imp}, {CMP, addr_aby}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {CMP, addr_abx}, {DEC, addr_abx}, {XXX, addr_imp},
	{CPX, addr_imm}, {SBC, addr_inx}, {XXX, addr_imp}, {XXX, addr_imp},
	{CPX, addr_zpg}, {SBC, addr_zpg}, {INC, addr_zpg}, {XXX, addr_imp},
	{INX, addr_imp}, {SBC, addr_imm}, {NOP, addr_imp}, {XXX, addr_imp},
	{CPX, addr_abs}, {SBC, addr_abs}, {INC, addr_abs}, {XXX, addr_imp},
	{BEQ, addr_rel}, {SBC, addr_iny}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {SBC, addr_zpx}, {INC, addr_zpx}, {XXX, addr_imp},
	{SED, addr_imp}, {SBC, addr_aby}, {XXX, addr_imp}, {XXX, addr_imp},
	{XXX, addr_imp}, {SBC, addr_abx}, {INC, addr_abx}, {XXX, addr_imp}
};
