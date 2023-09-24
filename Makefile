init:
	git submodule update --init
	cd rocket-chip && git submodule update --init hardfloat cde

compile:
	mill -i AXI2TL.compile
	mill -i AXI2TL.test.compile

test-dma:
	mill -i AXI2TL.test.runMain axi2tl.TestDMA -td build --full-stacktrace --target verilog
	mv build/TestDMA.v build/TestTop.v

clean:
	rm -rf ./build

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.idea.GenIdea/idea

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat
