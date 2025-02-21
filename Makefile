.PHONY: docker/build
docker/build:
	docker build . -t riscv/mycpu

.PHONY: docker/run
docker/run:
	docker run -it -v $(PWD):/src riscv/mycpu
