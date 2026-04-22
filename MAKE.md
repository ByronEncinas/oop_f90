# Summary of Targets

* `make` → standard build
* `make debug` → debugging and validation
* `make fast` → performance testing
* `make lapack` → linear algebra support
* `make fastlapack` → maximum performance


#  Build Instructions

This project uses a Makefile with multiple build modes for development, debugging, and 
performance optimization.

---

# Project Structure

- `src/` → library source files
- `app/` → main program
- `obj/` → compiled object and module files
- `bin/` → executable output

---

# Compiler

- Fortran compiler: `gfortran`

---

# Build Modes

## 1. Default build (recommended)

```bash
make
````

Flags:

* `-O3`
* `-Wall -Wextra -Wpedantic`
* `-std=f2008`

Use for:

* normal execution
* validated results
* general runs

---

## 2. Debug build

```bash
make debug
```

Flags:

* `-O0`
* `-g`
* `-fcheck=all`
* `-fbacktrace`

Use for:

* debugging crashes
* detecting memory issues
* fixing numerical errors
* validating solver correctness

---

## 3. Fast-math build

```bash
make fast
```

Flags:

* `-O3`
* `-ffast-math`
* `-march=native`
* `-flto`

Use for:

* performance benchmarking
* large simulations
* speed testing

Warning:

* may slightly change floating-point results
* not recommended for debugging

---

## 4. LAPACK/BLAS build

```bash
make lapack
```

Adds:

* `-llapack`
* `-lblas`

Use for:

* matrix operations
* linear algebra routines
* numerical solvers requiring LAPACK

---

## 5. Fast + LAPACK build

```bash
make fastlapack
```

Combines:

* fast-math optimizations
* LAPACK/BLAS linking

Use for:

* maximum performance simulations
* production-level runs

Warning:

* only use after verifying correctness with debug build

---

# Recommended Workflow

## Step 1 — Debug first

```bash
make debug
```

Fix:

* crashes
* allocation errors
* NaNs
* unstable numerical behavior

---

## Step 2 — Validate correctness

```bash
make
```

Confirm:

* correct numerical results
* expected convergence
* stable integration

---

## Step 3 — Optimize performance

```bash
make fast
```

Use only after validation.

