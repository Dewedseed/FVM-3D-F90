# FVM-3D-F90

Create a personal numerical method experimental platform.

## Attribute

- Note: Mark *Developing*

| Attribute        | Content                            |
| :--------------- | :--------------------------------- |
| Equation         | Navier-Stokes                      |
| Method           | Finite Volume Method           |
| Mesh             | Nodal-Centered Structural Grid     |
| Connectivity     | Ghost Mesh                         |
| Flux Split       | Roe, VanLeer, AUSM+                |
| Flux Reconstruct | Simple, MUSCL, WENO, *PPM*         |
| Flux limiter     | Minmod, VnaLeer, VanAlbada         |
| Turbulent Model  | *k-epsilon, k-omega*               |
| Time Stepping    | Runge-Kutta, *GMRES, Precondition* |
| Gas Model        | ideal gas, *real gas*              |
| Parallel         | *OpenMPI, OpenMP, CUDA*            |
| Mesh Partition   | METIS, IF, *REB*                   |
| File Format      | CGNS                               |
| Language         | Fortran-OOP                        |

## Usage

```bash
make install
make test2-sp
```

## Dependencies

1. [OpenMPI](https://www.open-mpi.org/)
2. [OpenMP](https://www.openmp.org/)
3. [CUDA](https://developer.nvidia.com/cuda-toolkit)
4. [CGNS 4.4.0](https://cgns.github.io/cgns-modern.github.io/index.html)
5. [METIS 5.2.1](https://github.com/KarypisLab/METIS/tree/master/manual)
6. [Fortran stdlib](https://stdlib.fortran-lang.cn/)
7. [Fortran METIS | ivan-pi](https://github.com/ivan-pi/fmetis)

## Reference

1. [Blazek, Jiri. Computational fluid dynamics: principles and applications. Butterworth-Heinemann, 2015.](https://www.sciencedirect.com/book/9780080999951/computational-fluid-dynamics-principles-and-applications)
2. [刘巍 - 计算空气动力学并行编程基础[M].国防工业出版社,2013.](https://xueshu.baidu.com/usercenter/paper/show?paperid=0120a707aa1814365dd37d84cb84dcd4&site=xueshu_se)
3. [Toro E F. Riemann solvers and numerical methods for fluid dynamics: a practical introduction[M]. Springer Science & Business Media, 2013.](https://books.google.com/books?hl=zh-CN&lr=&id=zkLtCAAAQBAJ&oi=fnd&pg=PA1&ots=SH9wzpZSR7&sig=MHQ6CqIapndBmVRxRvDDFaEvIYI#v=onepage&q&f=false)
4. [Hirsch C. Numerical computation of internal and external flows: The fundamentals of computational fluid dynamics[M]. Elsevier, 2007.](https://www.sciencedirect.com/book/9780750665940/numerical-computation-of-internal-and-external-flows)
