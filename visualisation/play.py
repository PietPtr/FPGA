import pygame
from fpga import *
from consts import *
import json
import sys, os
from subprocess import PIPE, Popen
import time
from pprint import pprint

class Main:
    def __init__(self, max_cycles):
        self.size = self.width, self.height = SIZE * 4, SIZE * 4
        self.screen = pygame.display.set_mode(self.size)
        self.max_cycles = max_cycles
        self.configs = {} # cycle number -> configuration dict of FPGA

    def load_from_binary(self):
        p = Popen(["../clash/Main"], stdout=PIPE, stderr=PIPE)
        # p = Popen(["echo", "3,lut,11_1111_1111", "2>&1"], stdout=PIPE)
        largest_seen_cycle = -1
        while True:
            line = p.stderr.readline()
            [cycle, lut, value] = line.decode("utf-8").split(",")
            cycle = int(cycle)
            
            if cycle > largest_seen_cycle:
                largest_seen_cycle = cycle
                print(f"Read cycle {cycle}.")
            
            if largest_seen_cycle > self.max_cycles:
                break
            
            self.insert(cycle, lut, value)


        p.kill()

        pprint(self.configs)
        self.set_fpga(0)

    def insert(self, cycle, lut_id, value_str):
        try:
            config = self.configs[cycle]
        except KeyError:
            config = {'tiles': {}}

        split = lut_id.split('_')

        coord = split[1] + "," + split[2]
        lut_name = "horzLut" if split[3] == "h" else "vertLut"

        lut_conf = {lut_name: {
            'enableReg0': value_str[0] == '1',
            'enableReg1': value_str[1] == '1',
            'lutConfig': value_str[3:12].replace('_', '')
        }}

        try:
            config['tiles'][coord] = config['tiles'][coord] | lut_conf
        except KeyError:
            config['tiles'][coord] = lut_conf

        self.configs[cycle] = config

    def set_fpga(self, cycle):
        self.current_cycle = cycle
        self.fpga = FPGA(self.configs[cycle], 4, 4)

    def next_cycle(self):
        cycle = self.max_cycles if self.current_cycle == self.max_cycles else self.current_cycle + 1
        self.set_fpga(cycle)
    
    def prev_cycle(self):
        cycle = 0 if self.current_cycle == 0 else self.current_cycle - 1
        self.set_fpga(cycle)

    def run(self):
        while True:
            for event in pygame.event.get():
                if event.type == pygame.QUIT: 
                    sys.exit()
                elif event.type == pygame.KEYUP:
                    print(event.key)
                    if event.key == pygame.K_RIGHT:
                        self.next_cycle()
                    elif event.key == pygame.K_LEFT:
                        self.prev_cycle()

            """
            Drawing
            """
            self.screen.fill(black)

            self.fpga.draw(self.screen)

            pygame.display.flip()

main = Main(310)
main.load_from_binary()
main.run()