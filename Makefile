init:
	git submodule update --init
	cd rocket-chip && git submodule update --init hardfloat api-config-chipsalliance

compile:
	mill -i AXI2TL.compile

test-dma:
	mill -i AXI2TL.test.runMain axi2tl.TestDMA -td build --full-stacktrace
	mv build/TestDMA.v build/TestTop.v

clean:
	rm -rf ./build

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.scalalib.GenIdea/idea

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat
