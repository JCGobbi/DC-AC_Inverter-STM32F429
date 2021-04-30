# Ada MODBUS Protocol

This repository contains Ada software for the implementation of a DC-AC 50/60 Hz inverter from 12 V to 220 VAC with a low frequency transformer. The sinusoidal wave is sinthesized with PWM modulation. There is no hardware project for this inverter.

The software uses the [Ada Drivers Library](https://www.github.com/Adacore/Ada_Drivers_Library) from [Adacore](https://www.adacore.com). The main board used for developing this software is the NUCLEO-F429ZI, from [ST Microelectronics](https://www.st.com), that has connectors for Arduino shields, ZIO and MORPHO expansion connector, so you may connect any Arduino shield on it and develop your own power inverter board. If you want to use other nucleo boards, do the **Project Wizard** that comes with the Ada Drivers Library choosing your board. You will need to change the hardware addresses of the `stm_board.ads` file inside the `src` folder to adapt to your board.

You may use the GNAT Programming Studio from Adacore or Visual Studio Code from Microsoft to cross-compile these sources. For VSCode, it has a `.vscode/tasks.json` file that permits to check syntax and semantic, compile, build, clean, convert elf to hex and bin files and flash hex and bin files to board. Both IDEs need the [ST-LINK](https://github.com/stlink-org/stlink) (version 1.6 or later) to flash the executable to the nucleo board.
