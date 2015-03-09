# mirage-vnetif
Virtual network interface and software switch for Mirage. 

Provides the module `Vnetif` which can be used as a replacement for the regular `Netif` implementation in Xen and Unix. Stacks built using `Vnetif` are connected to a software switch that allows the stacks to communicate as if they were connected to the same LAN.

An example of a unikernel that communicates with itself over `Vnetif` can be seen [here](https://github.com/MagnusS/mirage-vnetif/blob/master/examples/connect/unikernel.ml). The example can be compiled for Unix and Xen and does not need access to a real network interface.

## Install
```
opam pin add mirage-vnetif https://github.com/MagnusS/mirage-vnetif.git
```

## Build examples
```
mirage configure --xen/--unix
make
```
